using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Windows.Forms;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;

using Nemerle.Compiler;
using Nemerle.Completion2;

using Nemerle.VisualStudio.LanguageService;

using MSBuild = Microsoft.Build.BuildEngine;
using SourceMap = System.Collections.Generic.Dictionary<string, Nemerle.VisualStudio.LanguageService.NemerleSource>;
using ProjectManager = Nemerle.VisualStudio.LanguageService.ProjectManager;
using Nemerle.VisualStudio.GUI;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Package;

namespace Nemerle.VisualStudio.Project
{
	public class ProjectInfo : IEngineCallback
	{
		public string ProjectFullPath { get; private set; }
		private HierarchyListener      _listener;
		private Dictionary<string,int> _fileMap   = new Dictionary<string, int>();
		private SourceMap              _sourceMap = new SourceMap();

		private static Collection<ProjectInfo> _projects = new Collection<ProjectInfo>();
		private string                         _projectLocation;

		public ProjectInfo(
			NemerleProjectNode     projectNode,
			IVsHierarchy           hierarchy,
			NemerleLanguageService languageService,
			string                 fileName, 
			string                 location
		)
		{
			ErrorHelper.ThrowIsNull(projectNode, "projectNode");
			ErrorHelper.ThrowIsNull(hierarchy,   "hierarchy");
			Debug.Assert(projectNode.Site != null);

			ProjectFullPath = Path.GetFullPath(fileName);
			_projectNode = projectNode;
			_hierarchy   = hierarchy;

			CompilationOptions options = new CompilationOptions();

			options.DoNotLoadStdlib = NoStdLib;
			options.DoNotLoadMacros = NoStdMacros;
			ProjectManager projectManager = new ProjectManager(languageService);

			_engine = new Engine(this, options, projectManager, new TraceWriter()); // it enables parser working.

			UpdateConditionalVariables();

			projectManager.Engine = Engine;

			Engine.TypedtreeCreated += delegate
			{
				_buildTypedtreeCount++;
				AstToolWindow tool = AstToolWindow.AstTool;
				if (tool != null)
					tool.BuildTypedtreeCount = _buildTypedtreeCount;
			};

			if (!string.IsNullOrEmpty(location))
				_projectLocation = location;
			else
				_projectLocation = Path.GetDirectoryName(fileName);

			if (!_projectLocation.EndsWith("\\"))
				_projectLocation += "\\";
		}

		public string ProjectName { get { return _projectNode.VSProject.Project.Name; } }
		
		int _buildTypedtreeCount;

		private Engine _engine;
		public  Engine  Engine
		{
			[DebuggerNonUserCode]
			get { ManagerClass.Instance = _engine; return _engine; }
		}

		private NemerleProjectNode _projectNode;
		public  NemerleProjectNode  ProjectNode { get { return _projectNode; } }

		bool GetBoolProp(string name) { return Utils.IsTrue(ProjectNode.BuildProject, name); }

		public bool RunDebugger { get { return GetBoolProp("RunDebugger"); } }

		public bool NoStdLib	  { get { return GetBoolProp("NoStdLib");	} }
		public bool NoStdMacros { get { return GetBoolProp("NoStdMacros"); } }

		IVsHierarchy _hierarchy;

		public void InitListener()
		{
			_listener = new HierarchyListener(_hierarchy);

			_listener.ItemAdded   += FileAdded;
			_listener.ItemDeleted += FileDeleted;

			_listener.StartListening(true);
		}

		public static Collection<ProjectInfo> Projects
		{
			get { return _projects; }
		}

		public IVsSmartOpenScope SmartOpenScope
		{
			get { return (IVsSmartOpenScope)ProjectNode.Site.GetService(typeof(SVsSmartOpenScope)); }
		}

		#region Project References

		List<ReferenceNode> _assemblyReferences = new List<ReferenceNode>();

		public void AddAssembly(ReferenceNode node)
		{
			_assemblyReferences.Add(node);
			Engine.Reset();
		}

		public void RemoveAssembly(ReferenceNode node)
		{
			bool res = _assemblyReferences.Remove(node);
			Debug.Assert(res, "Can't remove assembly reference '"
				+ node.Caption + "' (" + node.Url + ")");
			Engine.Reset();
		}

		/// <summary>Watchers for project assemble reference.</summary>
		List<FileSystemWatcher> _assembleReferenceWatchers = new List<FileSystemWatcher>();

		#region IEngineCallback Members


		public void ShowMessage(string message, MessageType messageType)
		{
			OLEMSGICON icon = OLEMSGICON.OLEMSGICON_CRITICAL;
			switch (messageType)
			{
				case MessageType.Error:   icon = OLEMSGICON.OLEMSGICON_CRITICAL; break;
				case MessageType.Hint:	icon = OLEMSGICON.OLEMSGICON_INFO;	 break;
				case MessageType.Info:	icon = OLEMSGICON.OLEMSGICON_INFO;	 break;
				case MessageType.Warning: icon = OLEMSGICON.OLEMSGICON_WARNING;  break;
			}
			OLEMSGBUTTON buttons = OLEMSGBUTTON.OLEMSGBUTTON_OK;
			OLEMSGDEFBUTTON defaultButton = OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST;
			VsShellUtilities.ShowMessageBox(ProjectNode.ProjectMgr.Site,
				message, NemerleConstants.ProductName, icon, buttons, defaultButton);
		}

		IEnumerable<string> IEngineCallback.GetAssemblyReferences()
		{
			ResetAssembleReferenceWatchers();
			
			foreach (ReferenceNode node in _assemblyReferences)
			{
				string assemblyId = null;
				var prjRef = node as ProjectReferenceNode;
				if (prjRef != null) // Is project reference...
					assemblyId = prjRef.ReferencedProjectOutputPath; // calc real assembly name.
				else if (node.Url != null) //IT: if dll does not exist, the Url will be null.
					assemblyId = node.Url;
				else
				{
					//TODO: Notify user about reference does not exist. 
				}

				if (assemblyId != null)
				{
					AddAssembleReferenceWatcher(assemblyId);
					yield return assemblyId;
				}
			}
		}

		#endregion

		private void AddAssembleReferenceWatcher(string filePath)
		{
			string path = Path.GetDirectoryName(filePath);
			string name = Path.GetFileName(filePath);
			FileSystemWatcher watcher = new FileSystemWatcher(path, name);
			watcher.NotifyFilter = NotifyFilters.LastWrite;
			watcher.Changed += watcher_Changed;
			_assembleReferenceWatchers.Add(watcher);
			watcher.EnableRaisingEvents = true;
		}

		void ResetAssembleReferenceWatchers()
		{
			foreach (FileSystemWatcher watcher in _assembleReferenceWatchers)
				watcher.Dispose();

			_assembleReferenceWatchers.Clear();
		}

		void watcher_Changed(object sender, FileSystemEventArgs e)
		{
			Engine.Reset();
		}

		#endregion

		internal void AddSource(NemerleSource source)
		{
			string path = source.GetFilePath();
			_sourceMap[path] = source;
		}

		internal void RemoveSource(NemerleSource source)
		{
			string path = source.GetFilePath();
			_sourceMap.Remove(path);
		}

		public NemerleSource GetSource(string filePath)
		{
			NemerleSource source;
			return _sourceMap.TryGetValue(filePath, out source) ? source : null;
		}

		/// <summary>
		/// If the provided file is attached to the project,  its timestamp (version) is returned.
		/// Otherwise 0.
		/// </summary>
		/// <param name="filePath">
		/// Full file path.
		/// </param>
		public int GetFileTimestamp(string filePath)
		{
			int version;
			return _fileMap.TryGetValue(filePath, out version) ? version : 0;
		}

		public bool IsFileInProject(string filePath)
		{
			ErrorHelper.ThrowIfPathNullOrEmpty(filePath, "filePath");
			return _fileMap.ContainsKey(filePath);
		}

		public void UpdateFile(ParseRequest request)
		{
			ErrorHelper.ThrowIsNull(request, "request");

			int	oldTimestamp;
			string filePath  = request.FileName;
			int	timestamp = request.Timestamp;

			if (!_fileMap.TryGetValue(filePath, out oldTimestamp))
				throw new ArgumentException("File '" + filePath + "' not found.", "filePath");

			if (timestamp != oldTimestamp)
			{
				Engine.Sources.AddOrUpdate(filePath, GetFileContent(request));
				_fileMap[filePath] = timestamp;
			}
		}

		string GetFileContent(ParseRequest request)
		{
			ErrorHelper.ThrowIsNull(request, "request");

			string text = request.Text;

			if (text == null)
				text = File.ReadAllText(request.FileName);

			return text;
		}

		private static NemerleFileNodeProperties GetNodeProperties(IVsHierarchy hierarchy, uint itemID)
		{
			ErrorHelper.ThrowIsNull(hierarchy, "hierarchy");

			object propertyValue;
			int	hr = hierarchy.GetProperty(itemID, (int)__VSHPROPID.VSHPROPID_BrowseObject, out propertyValue);

			if (hr != VSConstants.S_OK)
				throw new ArgumentException("Can't obtain VSHPROPID_BrowseObject for item with ID "
					+ itemID, "itemID", new Win32Exception(hr));

			NemerleFileNodeProperties properties = propertyValue as NemerleFileNodeProperties;

			if (properties == null)
				return new NemerleFileNodeProperties(((NodeProperties)propertyValue).Node);
			else
				return properties;
		}

		internal void UpdateConditionalVariables()
		{
			// Pass all project defined #define's to the compiler
			//
			string defineConstants = _projectNode.GetProjectProperty(
				NemerleBuildPropertyPageTag.DefineConstants.ToString(), false);

			_engine.Defines.Clear();
			if (!string.IsNullOrEmpty(defineConstants))
				foreach (string define in defineConstants.Replace(" \t\r\n", String.Empty).Split(';'))
					_engine.Defines.Add(define);
		}

		private void FileAdded(object sender, HierarchyEventArgs ergs)
		{
			Debug.Assert(ergs.TextBuffer == null);

			IVsHierarchy hierarchy = (IVsHierarchy)sender;
			NemerleFileNodeProperties nodeProps = GetNodeProperties(hierarchy, ergs.ItemID);

			if (nodeProps.BuildAction != BuildAction.Compile)
				return;
	
			string path = ergs.FileName;

			_fileMap.Add(path, 1);

			try
			{
				Engine.Sources.AddOrUpdate(path, File.ReadAllText(path));
			}
			catch (Exception ex)
			{
				// Same file can be included in different projects.
				// VladD2: For now we don't support it. 
				//
				const string format = "Error add file '{0}' to Completion Engine.\r\n{1}";
				Debug.WriteLine(string.Format(format, path, ex));
			}
		}

		private void FileDeleted(object sender, HierarchyEventArgs ergs)
		{
			Debug.Assert(ergs.TextBuffer == null);

			string path = ergs.FileName;

			_fileMap.Remove(path);
			Engine.Sources.Remove(path);
		}

		public static ProjectInfo FindProject(string fileName)
		{
			ErrorHelper.ThrowIfPathNullOrEmpty(fileName, "fileName");

			foreach (ProjectInfo proj in _projects)
				if (proj.IsFileInProject(fileName))
					return proj;

			return null;
		}

		public bool IsProjectAvailable
		{
			get { return Engine.IsProjectAvailable; }
		}

		public void ResetNamespaceTree()
		{
			Engine.ResetNamespaceTree();
		}

		public void AddRelocation(
			string filePath,
			int newEndIndex, int newEndLine,
			int oldEndIndex, int oldEndLine,
			int startIndex, int startLine)
		{
			ErrorHelper.ThrowIfPathNullOrEmpty(filePath, "filePath");

			if (!IsProjectAvailable || IsDocumentOpening)
				return;

			// If can't add relocation we must reparse types tree.
			if (!Project.AddRelocation(filePath, newEndIndex, newEndLine, 
				oldEndIndex, oldEndLine, startIndex, startLine))
			{
				// Временно отключено!
				//NemerleSource source = Utils.GetFileSource(_site, filePath);
				//string inserted = source.GetText(startLine - 1, startIndex - 1,
				//  newEndLine - 1, newEndIndex - 1);
				//
				//// If white space is inserted...
				//if (oldEndLine == startLine && startIndex == oldEndIndex
				//  && Utils.IsAllWhiteSpace(inserted)
				//)
				//{
				//  int zeroStartIndex = startIndex - 1;
				//  string startLineText = source.GetLine(startLine);
				//  if (zeroStartIndex <= 0 || zeroStartIndex + 1 >= startLineText.Length
				//	|| Utils.IsSeparator(startLineText[zeroStartIndex - 1])
				//	|| Utils.IsSeparator(startLineText[zeroStartIndex + 1])
				//  )
				//	Project.Engine.AddRelocation(Location.GetFileIndex(filePath),
				//	  startLine, startIndex, newEndLine, newEndIndex);
				//  return;
				//}

				// Reset link to the project. It leads next request to Project property
				// will create new project, and accordingly will create new types tree.
				ResetNamespaceTree();
#if DebugLocations

				// Update GUI-tree visualizing AST of the current file.
				int fileIndex = Location.GetFileIndex(filePath);

				Engine.Project.UpdateDebugTree(fileIndex);
#endif
			}
		}

		public Nemerle.Completion2.Project Project
		{
			[DebuggerStepThrough]
			get { return Engine.Project; }
		}

		bool _isDocumentOpening;

		public bool IsDocumentOpening
		{
			get { return _isDocumentOpening; }
			internal set { _isDocumentOpening = value; }
		}


		public CompletionElem[] CompleteWord(string filePath, int line, int col, ISource source)
		{
			ErrorHelper.ThrowIfPathNullOrEmpty(filePath, "filePath");
			return Engine.CompleteWord(filePath, line + 1, col + 1, source);
		}

		public QuickTipInfo GetQuickTip(string filePath, int line, int col, ISource source)
		{
			ErrorHelper.ThrowIfPathNullOrEmpty(filePath, "filePath");

			Trace.WriteLine(">>>> ##### GetQuickTip!");

			QuickTipInfo info = Project.GetQuickTipInfo(filePath, line + 1, col + 1);

			if (info != null)
				info.Text = info.Text.Replace(_projectLocation, "");

			Trace.WriteLine("<<<< ##### GetQuickTip!");

			return info;
		}

		public GotoInfo[] GetGoto(string filePath, int line, int col, ISource source)
		{
			Trace.WriteLine(">>>> ##### GetGoto!");
			ErrorHelper.ThrowIfPathNullOrEmpty(filePath, "filePath");
			GotoInfo[] result = Project.GetDefinition(filePath, line + 1, col + 1);
			Trace.WriteLine("<<<< ##### GetGoto!");
			return result;
		}

		public GotoInfo[] GetUsages(string filePath, int line, int col, ISource source)
		{
			Trace.WriteLine(">>>> ##### GetUsages!");
			ErrorHelper.ThrowIfPathNullOrEmpty(filePath, "filePath");
			GotoInfo[] result = Project.GetUsages(filePath, line + 1, col + 1);
			Trace.WriteLine("<<<< ##### GetUsages!");
			return result;
		}

		public NemerleMethods GetMethodTip(string filePath, int line, int col, ISource source)
		{
			Trace.WriteLine(">>>> ##### GetMethodTip!");
			ErrorHelper.ThrowIfPathNullOrEmpty(filePath, "filePath");

			MethodTipInfo info = Project.GetMethodTip(filePath, line + 1, col + 1, source);
			NemerleMethods result = info != null ? new NemerleMethods(info): null;
			Trace.WriteLine("<<<< ##### GetMethodTip!");
			return result;
		}

		internal void HighlightUsages(StringOrInt filePathOrIndex, int line, int column, ISource source, bool isPermanent)
		{
			Trace.WriteLine(">>>> ##### HighlightUsages!");
			if(filePathOrIndex.IsString)
				ErrorHelper.ThrowIfPathNullOrEmpty(filePathOrIndex.StringValue, "filePath");
			SourceTextManager manager = source as SourceTextManager;
			if (manager == null)
				return;
			ScanLexer lexer = manager.Source.Scanner.GetLexer();
			if (lexer == null)
				return;
			if(filePathOrIndex.IsString)
				Project.HighlightUsages(lexer, filePathOrIndex.StringValue, line + 1, column + 1, isPermanent);
			else
				Project.HighlightUsages(lexer, filePathOrIndex.IntValue, line + 1, column + 1, isPermanent);
			manager.Source.Recolorize(1, source.LineCount);
			Trace.WriteLine("<<<< ##### HighlightUsages!");
		}

		internal void RemoveLastHighlighting(ISource source)
		{
			Trace.WriteLine(">>>> ##### RemoveLastHighlighting!");
			SourceTextManager manager = source as SourceTextManager;
			if (manager == null)
				return;
			ScanLexer lexer = manager.Source.Scanner.GetLexer();
			if (lexer == null)
				return;
			lexer.RemoveLastHighlighting();
			manager.Source.Recolorize(1, source.LineCount);
			Trace.WriteLine("<<<< ##### HighlightUsages!");
		}

	}
}
