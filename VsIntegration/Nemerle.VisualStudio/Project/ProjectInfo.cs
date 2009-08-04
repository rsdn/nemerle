using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
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
using Nemerle.VisualStudio.Package;
using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;
using Microsoft.VisualStudio.TextManager.Interop;
using BndFlgs = System.Reflection.BindingFlags;
using MethodBuilderEx = Nemerle.Completion2.Factories.IntelliSenseModeMethodBuilder;

namespace Nemerle.VisualStudio.Project
{
	public class ProjectInfo : IEngineCallback
	{
		public   string                  ProjectFullPath { get; private set; }
		         HierarchyListener       _listener;
		readonly Dictionary<string, int> _fileMap = new Dictionary<string, int>();
		readonly SourceMap               _sourceMap = new SourceMap();
		public   NemerleLanguageService   LanguageService { get; private set; }
		readonly ErrorListProvider       _errorList;

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
			ErrorHelper.ThrowIsNull(languageService, "languageService");
			ErrorHelper.ThrowIsNull(projectNode, "projectNode");
			ErrorHelper.ThrowIsNull(hierarchy,   "hierarchy");
			Debug.Assert(projectNode.Site != null);

			LanguageService  = languageService;
			_errorList       = new ErrorListProvider(languageService.Site);
			ProjectFullPath  = Path.GetFullPath(fileName);
			_projectNode     = projectNode;
			_hierarchy       = hierarchy;

			CompilationOptions options = new CompilationOptions();

			options.DoNotLoadStdlib = NoStdLib;
			options.DoNotLoadMacros = NoStdMacros;
			ProjectManager projectManager = new ProjectManager(languageService);

			_engine = new Engine(this, options, projectManager, new TraceWriter(), false); // it enables parser working.

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

		public void BeginBuildTypesTree()
		{
			_engine.BeginBuildTypesTree();
		}

		public bool IsCosed { get; private set; }

		public void Close()
		{
			if (IsCosed)
				return;

			IsCosed = true;
			_projects.Remove(this);
			if (LanguageService.IsDisposed)
				LanguageService.AbortBackgroundParse();
		}

		public string ProjectName { get { return _projectNode.VSProject.Project.Name; } }
		
		int _buildTypedtreeCount;
		readonly List<MethodBuilderEx> _methodsCheckQueue = new List<MethodBuilderEx>(100);

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

		private void AddAssembleReferenceWatcher(string filePath)
		{
			if (!File.Exists(filePath))
				return;

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
			var taksForSource = GetTaksForSource(source.FileIndex);

			foreach (var task in taksForSource)
				task.DisposeTextLineMarker();

			string path = source.GetFilePath();
			_sourceMap.Remove(path);
		}

		public NemerleSource GetSource(string filePath)
		{
			NemerleSource source;
			return _sourceMap.TryGetValue(filePath, out source) ? source : null;
		}

		public IEnumerable<NemerleSource> GetSources()
		{
			return _sourceMap.Values;
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

		public static ProjectInfo FindProject(IVsHierarchy hierarchy)
		{
			foreach (ProjectInfo proj in _projects)
				if (Utilities.IsSameComObject(proj._hierarchy, hierarchy))
					return proj;

			return null;
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

		public MemberBuilder AddRelocation(
			string filePath,
			int newEndIndex, int newEndLine,
			int oldEndIndex, int oldEndLine,
			int startIndex, int startLine)
		{
			ErrorHelper.ThrowIfPathNullOrEmpty(filePath, "filePath");

			if (!IsProjectAvailable || IsDocumentOpening)
				return null;

			var resetedMember = Project.AddRelocation(filePath, newEndIndex, newEndLine, 
				oldEndIndex, oldEndLine, startIndex, startLine);

			// If can't add relocation we must reparse types tree.
			if (resetedMember == null)
			{
		
				////!!! ResetNamespaceTree();
#if DebugLocations

				// Update GUI-tree visualizing AST of the current file.
				int fileIndex = Location.GetFileIndex(filePath);

				Engine.Project.UpdateDebugTree(fileIndex);
#endif
			}

			return resetedMember;
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

			Debug.WriteLine(">>>> ##### GetQuickTip!");

			QuickTipInfo info = Project.GetQuickTipInfo(filePath, line + 1, col + 1);

			if (info != null)
				info.Text = info.Text.Replace(_projectLocation, "");

			Debug.WriteLine("<<<< ##### GetQuickTip!");

			return info;
		}

		public GotoInfo[] GetGoto(string filePath, int line, int col, ISource source)
		{
			Debug.WriteLine(">>>> ##### GetGoto!");
			ErrorHelper.ThrowIfPathNullOrEmpty(filePath, "filePath");
			GotoInfo[] result = Project.GetDefinition(filePath, line + 1, col + 1);
			Debug.WriteLine("<<<< ##### GetGoto!");
			return result;
		}

		public GotoInfo[] GetUsages(string filePath, int line, int col, ISource source)
		{
			Debug.WriteLine(">>>> ##### GetUsages!");
			ErrorHelper.ThrowIfPathNullOrEmpty(filePath, "filePath");
			GotoInfo[] result = Project.GetUsages(filePath, line + 1, col + 1);
			Debug.WriteLine("<<<< ##### GetUsages!");
			return result;
		}

		public NemerleMethods GetMethodTip(string filePath, int line, int col, ISource source)
		{
			Debug.WriteLine(">>>> ##### GetMethodTip!");
			ErrorHelper.ThrowIfPathNullOrEmpty(filePath, "filePath");

			MethodTipInfo info = Project.GetMethodTip(filePath, line + 1, col + 1, source);
			NemerleMethods result = info != null ? new NemerleMethods(info): null;
			Debug.WriteLine("<<<< ##### GetMethodTip!");
			return result;
		}

		#region HighlightUsages

		internal void HighlightUsages(StringOrInt filePathOrIndex, int line, int column, ISource source, bool isPermanent)
		{
			Debug.WriteLine(">>>> ##### HighlightUsages!");
			if (filePathOrIndex.IsString)
				ErrorHelper.ThrowIfPathNullOrEmpty(filePathOrIndex.StringValue, "filePath");
			var nsource = source as NemerleSource;

			if (nsource == null)
				return;

			ScanLexer lexer = nsource.Scanner.GetNewLexer();
			if (lexer == null)
				return;
			if (filePathOrIndex.IsString)
				Project.HighlightUsages(lexer, filePathOrIndex.StringValue, line + 1, column + 1, isPermanent);
			else
				Project.HighlightUsages(lexer, filePathOrIndex.IntValue, line + 1, column + 1, isPermanent);
			nsource.Recolorize(1, source.LineCount);
			Debug.WriteLine("<<<< ##### HighlightUsages!");
		}

		internal void RemoveLastHighlighting(ISource source)
		{
			Debug.WriteLine(">>>> ##### RemoveLastHighlighting!");
			var nsource = source as NemerleSource;
			if (nsource == null)
				return;
			ScanLexer lexer = nsource.Scanner.GetNewLexer();
			if (lexer == null)
				return;
			lexer.RemoveLastHighlighting();
			nsource.Recolorize(1, source.LineCount);
			Debug.WriteLine("<<<< ##### HighlightUsages!");
		}
		
		#endregion
	
		//static IEnumerable<Task> ToArray(TaskProvider.TaskCollection tasks)
		//{
		//  foreach (var item in tasks)
		//  {
		//    var task = item as Task;

		//    if (task != null)
		//      yield return task;
		//  }
		//}

		IEnumerable<NemerleErrorTask> GetTaksForSource(int fileIndex)
		{
			var tasks = _errorList.Tasks.OfType<NemerleErrorTask>();
			var taksForSource = tasks.Where(t => t.CompilerMessage.Location.FileIndex == fileIndex);
			return taksForSource;
		}

		internal void MakeCompilerMessagesTextMarkers(IVsTextLines buffer, int fileIndex)
		{
			var taksForSource = GetTaksForSource(fileIndex);

			foreach (var task in taksForSource)
				task.MakeTextMarker(buffer);
		}

		/// <summary>
		/// Chek one method from methodscheck queue. At first try check methods of current 
		/// currently active source file.
		/// </summary>
		/// <param name="currentFileIndex">
		/// The index of currently active source file. Use the Location.GetFileIndex() to obtain it.
		/// </param>
		/// <returns>new count of elements in queue</returns>
		public int ChekOneMethodFromMethodsCheckQueue(int currentFileIndex, int line, int col)
		{
			if (currentFileIndex <= 0)
				return ChekLastMethodFromMethodsCheckQueue();

			MethodBuilderEx mb = null;
			int count = 0;

			lock (_methodsCheckQueue) // lock only operations with queue (not compiling)!
			{
				var methodsFromCurrentView = _methodsCheckQueue.Where(m => m.Location.FileIndex == currentFileIndex); // lazy!
				var seq = methodsFromCurrentView.GetEnumerator(); // the collection not requre disposing... skip it

				if (seq.MoveNext()) // If is even one element exists...
				{
					// Try to find current method.
					var currMethod = methodsFromCurrentView.Where(m => m.Location.Contains(line, col)).GetEnumerator();
					if (currMethod.MoveNext())
						mb = currMethod.Current; // get it...
					else
						mb = seq.Current; // get it...

					_methodsCheckQueue.Remove(mb); // and remove from queue.
					count = _methodsCheckQueue.Count;
				}
			}

			if (mb == null) // if no not compiled method bodies
				return ChekLastMethodFromMethodsCheckQueue();

			mb.EnsureCompiled();
			return count;
		}

		public void ClearMethodsCheckQueue()
		{
			lock (_methodsCheckQueue)
			{
				_methodsCheckQueue.Clear();

			}
		}

		void FillMethodsCheckQueue()
		{
			// Get all MethodBuilders and add it into _methodsCheckQueue (for background check)
			if (Engine.IsProjectAvailable)
			{
				TypeBuilder[] typeBuilders = Engine.Project.NamespaceTree.GetTypeBuilders();
				var mbsForCompile = typeBuilders.SelectMany<TypeBuilder, MethodBuilderEx>(tb =>
					tb.GetMethods(BndFlgs.Public | BndFlgs.NonPublic
						| BndFlgs.Instance | BndFlgs.Static | BndFlgs.DeclaredOnly)
						.Cast<MethodBuilderEx>()
						.Where(mb => mb.IsBodyCompilable));

				lock (_methodsCheckQueue)
				{
					_methodsCheckQueue.Clear();
					_methodsCheckQueue.AddRange(mbsForCompile);
				}
			}
		}

		#region IEngineCallback Members

		public void ShowMessage(string message, MessageType messageType)
		{
			OLEMSGICON icon = OLEMSGICON.OLEMSGICON_CRITICAL;
			switch (messageType)
			{
				case MessageType.Error: icon = OLEMSGICON.OLEMSGICON_CRITICAL; break;
				case MessageType.Hint: icon = OLEMSGICON.OLEMSGICON_INFO; break;
				case MessageType.Info: icon = OLEMSGICON.OLEMSGICON_INFO; break;
				case MessageType.Warning: icon = OLEMSGICON.OLEMSGICON_WARNING; break;
			}
			OLEMSGBUTTON buttons = OLEMSGBUTTON.OLEMSGBUTTON_OK;
			OLEMSGDEFBUTTON defaultButton = OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST;
			VsShellUtilities.ShowMessageBox(ProjectNode.ProjectMgr.Site,
				message, NemerleConstants.ProductName, icon, buttons, defaultButton);
		}

		void IEngineCallback.SetTopLevelCompilerMessages(IEnumerable<CompilerMessage> messages)
		{
			SetCompilerMessages(messages, null);

			foreach (var source in GetSources())
				source.OnProjectReloaded();

			if (messages.Any())
				_errorList.Show();
		}

		void IEngineCallback.SetMethodCompilerMessages(MemberBuilder member, IEnumerable<CompilerMessage> messages)
		{
			if (!IsMemberVersionCorrect(member))
				return;

			messages = messages.Distinct(CompilerMessageEqComparer.Instance);

			// Find and clear existent error messages which associated with the 'member'.
			SetCompilerMessages(messages, task =>
			{
				var memberMsg = task.CompilerMessage as CompilerMessageForMethod;
				return memberMsg != null && memberMsg.Member == member;
			});
			// Following for debugging purpose:
			//var res = _errorList.Tasks.OfType<NemerleErrorTask>().GroupBy(x => x.ToString()).Where(g => g.Count() > 1).ToArray();
			//if (res.Length > 0)
			//	Test(res, Engine.TypesTreeVersion);
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

		public void SetStatusText(string text) // implemetn IEngineCallback
		{
			LanguageService.SetStatusBarText(text);
      Debug.WriteLine(text);
		}

		#region Implementation

		/// This method need only for debugging purpose.
		bool IsMemberVersionCorrect(MemberBuilder member)
		{
			var method = member as MethodBuilderEx;
			if (method == null)
			{
				Debug.Assert(false);
				return false;
			}

			var treeVer = Engine.TypesTreeVersion;
			var methodVer = method.TypesTreeVersion;

			if (methodVer != treeVer)
			{
				//Trace.Assert(false, "This should not happen. We have problem with multithreading!");
				return false;
			}

			return true;
		}

		class CompilerMessageEqComparer : IEqualityComparer<CompilerMessage>
		{
			#region IEqualityComparer<CompilerMessage> Members

			public bool Equals(CompilerMessage x, CompilerMessage y)
			{
				return x.Location.Equals(y.Location) && x.Kind == y.Kind && x.Msg == y.Msg;
			}

			public int GetHashCode(CompilerMessage obj)
			{
				return obj.Location.GetHashCode() ^ (obj.Msg == null ? 0 : obj.Msg.GetHashCode());
			}

			#endregion

			public static readonly CompilerMessageEqComparer Instance = new CompilerMessageEqComparer();
		}

		/// try add text markers (markers visualise errors in editor).
		void TryAddTextMarkers(IEnumerable<NemerleErrorTask> tasks)
		{
			var taskGroups = tasks.GroupBy(t => t.CompilerMessage.Location.File);

			foreach (var taskGroup in taskGroups)
			{
				var source = GetSource(taskGroup.Key);

				if (source != null)
				{
					IVsTextLines buffer = source.GetTextLines();
					foreach (var task in taskGroup)
						task.MakeTextMarker(buffer);
				}
			}
		}
		
		private void NavigateTo(object sender, EventArgs arguments)
		{
			var task = sender as NemerleErrorTask;

			if (task == null)
				throw new ArgumentException("sender");

			// Get the doc data for the task's document
			if (String.IsNullOrEmpty(task.Document))
				return;

			var serviceProvider = ProjectNode.GetServiceProvider();

			IVsUIShellOpenDocument openDoc = serviceProvider.GetService(
				typeof(IVsUIShellOpenDocument)) as IVsUIShellOpenDocument;

			if (openDoc == null)
				return;

			IVsWindowFrame frame;
			IOleServiceProvider sp;
			IVsUIHierarchy hier;
			uint itemid;
			Guid logicalView = VSConstants.LOGVIEWID_Code;

			if (ErrorHandler.Failed(openDoc.OpenDocumentViaProject(
				task.Document, ref logicalView, out sp, out hier, out itemid, out frame))
				|| frame == null
			)
				return;

			object docData;
			frame.GetProperty((int)__VSFPROPID.VSFPROPID_DocData, out docData);

			// Get the VsTextBuffer
			VsTextBuffer buffer = docData as VsTextBuffer;
			if (buffer == null)
			{
				IVsTextBufferProvider bufferProvider = docData as IVsTextBufferProvider;
				if (bufferProvider != null)
				{
					IVsTextLines lines;
					ErrorHandler.ThrowOnFailure(bufferProvider.GetTextBuffer(out lines));
					buffer = lines as VsTextBuffer;
					Debug.Assert(buffer != null, "IVsTextLines does not implement IVsTextBuffer");

					if (buffer == null)
						return;
				}
			}

			// Finally, perform the navigation.
			IVsTextManager mgr = serviceProvider.GetService(
				typeof(VsTextManagerClass)) as IVsTextManager;

			if (mgr == null)
				return;

			var span = task.Span;

			mgr.NavigateToLineAndColumn(buffer, ref logicalView,
				span.iStartLine, span.iStartIndex, span.iEndLine, span.iEndIndex);
		}
		
		/// <summary>Clear tasks generated by compiler messeges which associated with this project.</summary>
		void ClearCompilerMessgesTasks(Predicate<NemerleErrorTask> predicate)
		{
			var tasks = _errorList.Tasks;

			for (int i = 0; i < tasks.Count; i++)
			{
				var errorTask = tasks[i] as NemerleErrorTask;
				if (errorTask != null && errorTask.ProjectInfo == this)
					if (predicate == null || predicate(errorTask))
						tasks.RemoveAt(i--);
			}
		}

		void SetCompilerMessages(IEnumerable<CompilerMessage> messages, Predicate<NemerleErrorTask> predicate)
		{
			lock (_errorList.Tasks)
			{
				_errorList.SuspendRefresh();
				try
				{
					ClearCompilerMessgesTasks(predicate);
					AddNewCompilerMessages(messages);
				}
				finally { _errorList.ResumeRefresh(); }
			}
		}

		public bool IsMethodsCheckQueueEmpty { get { return _methodsCheckQueue.Count == 0; } }

		/// <summary>Chek one (last) method from methodscheck queue</summary>
		/// <returns>new count of elements in queue</returns>
		int ChekLastMethodFromMethodsCheckQueue()
		{
			MethodBuilderEx mb = null;
			var lastIndex = -1;

			lock (_methodsCheckQueue) // lock only when _methodsCheckQueue changed
			{
				Debug.WriteLine("_methodsCheckQueue.Count: " + _methodsCheckQueue.Count);
				// remove last MethodBuilder and compile it
				lastIndex = _methodsCheckQueue.Count - 1;

				if (lastIndex < 0)
					return 0;

				mb = _methodsCheckQueue[lastIndex];

				if (mb.ToString() == "method Tests.Test1.TestSourceTextManager.GetLine(line : int) : string")
				{
					;
				}
				_methodsCheckQueue.RemoveAt(lastIndex);
			}

			if (mb != null)
				mb.EnsureCompiled(); // no lock!

			return lastIndex;
		}

		void AddNewCompilerMessages(IEnumerable<CompilerMessage> messages)
		{
			var newTasks = messages.Select(message => new NemerleErrorTask(this, message, NavigateTo)).ToArray();

			foreach (var message in newTasks)
				_errorList.Tasks.Add(message);

			TryAddTextMarkers(newTasks);
		}

		//void Test(IGrouping<string, NemerleErrorTask>[] errGroups, int typeTreeVersion)
		//{
		//  Debug.WriteLine("  Engine.TypesTreeVersion: " + typeTreeVersion);
		//  foreach (var g in errGroups)
		//  {
		//    Debug.WriteLine(g.Key);
		//    foreach (var item in g)
		//    {
		//      var cm = (CompilerMessageForMethod)item.CompilerMessage;
		//      var method = (MethodBuilderEx)cm.Member;
		//      Debug.WriteLine("  " + method.TypesTreeVersion);
		//    }
		//  }
		//}

		#endregion

		#endregion
	}
}
