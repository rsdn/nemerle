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
using SourceMap = System.Collections.Generic.Dictionary<int, Nemerle.Completion2.IIdeSource>;
using Nemerle.VisualStudio.GUI;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Package;
using Nemerle.VisualStudio.Package;
using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;
using Microsoft.VisualStudio.TextManager.Interop;
using BndFlgs = System.Reflection.BindingFlags;
using MethodBuilderEx = Nemerle.Completion2.Factories.IntelliSenseModeMethodBuilder;
using Tuple = Nemerle.Builtins.Tuple<Nemerle.Compiler.Location, int>;

namespace Nemerle.VisualStudio.Project
{
	public class ProjectInfo : IIdeProject
	{
		private static Collection<ProjectInfo> _projects = new Collection<ProjectInfo>();

		public string ProjectFullPath { get; private set; }
		HierarchyListener _listener;
		public NemerleLanguageService LanguageService { get; private set; }
		readonly ErrorListProvider _errorList;
		public int ErrorCount { get { return _errorList.Tasks.Count; } }

		readonly SourceMap _sourceMap = new SourceMap();
		readonly List<IIdeSource> _sources = new List<IIdeSource>();

		private string _projectLocation;
		private bool _fileRenamingInProgress;

		public ProjectInfo(
			NemerleProjectNode projectNode,
			IVsHierarchy hierarchy,
			NemerleLanguageService languageService,
			string fileName,
			string location
		)
		{
			ErrorHelper.ThrowIsNull(languageService, "languageService");
			ErrorHelper.ThrowIsNull(projectNode, "projectNode");
			ErrorHelper.ThrowIsNull(hierarchy, "hierarchy");
			Debug.Assert(projectNode.Site != null);

			LanguageService = languageService;
			_errorList = new ErrorListProvider(languageService.Site);
			ProjectFullPath = Path.GetFullPath(fileName);
			_projectNode = projectNode;
			_hierarchy = hierarchy;

			_engine = EngineFactory.Create(this, new TraceWriter(), false); // it enables parser working.

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

		public void OpenAllFiles()
		{
			var nodes = new List<NemerleFileNode>();
			ProjectNode.FindNodesOfType<NemerleFileNode>(nodes);

			foreach (NemerleFileNode node in nodes)
			{
				var manager = (FileDocumentManager)node.GetDocumentManager();

				Debug.Assert(manager != null, "Could not get the FileDocumentManager");

				IVsWindowFrame frame;

				manager.Open(false, false, VSConstants.LOGVIEWID_Code, out frame, WindowFrameShowAction.Show);
			}
		}

		public void BeginReloadProject()
		{
			_engine.BeginReloadProject();
		}

		public bool IsCosed { get; private set; }

		public void Close()
		{
			if (IsCosed)
				return;

			ResetAssembleReferenceWatchers();
			Engine.Close();
			IsCosed = true;
			_projects.Remove(this);
			_errorList.Tasks.Clear();
			_sources.Clear();
			_sourceMap.Clear();
			//TODO: VladD2: Удалить из очередей AsyncWorker все запросы и ответы связанные с данным проектом!
		}

		public string ProjectName { get { return _projectNode.VSProject.Project.Name; } }

		public string ProjectFullName { get { return _projectNode.VSProject.Project.FullName; } }

		public string RootNamespace { get { return (_projectNode.VSProject.Project.Properties.Item("DefaultNamespace").Value ?? "").ToString(); } }

		int _buildTypedtreeCount;
		readonly List<MethodBuilderEx> _methodsCheckQueue = new List<MethodBuilderEx>(100);

		private IIdeEngine _engine;
		public IIdeEngine Engine
		{
			[DebuggerNonUserCode]
			get { ManagerClass.Instance = (ManagerClass)_engine; return _engine; }
		}

		private NemerleProjectNode _projectNode;
		public NemerleProjectNode ProjectNode { get { return _projectNode; } }

		bool GetBoolProp(string name) { return Utils.IsTrue(ProjectNode.BuildProject, name); }

		public bool RunDebugger { get { return GetBoolProp("RunDebugger"); } }

		public bool NoStdLib { get { return GetBoolProp("NoStdLib"); } }
		public bool NoStdMacros { get { return GetBoolProp("NoStdMacros"); } }

		IVsHierarchy _hierarchy;

		public void InitListener()
		{
			_listener = new HierarchyListener(_hierarchy);

			_listener.ItemAdded += FileAdded;
			_listener.ItemDeleted += FileDeleted;

			_listener.StartListening(true);
		}

		public static Collection<ProjectInfo> Projects
		{
			get { return _projects; }
		}

		#region Assemble Reference Managment

		#region Assembly References helpers

		public void AddAssembly(List<string> assemblies, ReferenceNode node, string type)
		{
			//TODO: Проследить что где-то добавляется вочер для сборки!
			var path = GetAssemblyReferencesString(node);

			if (!string.IsNullOrEmpty(path))
			{
				assemblies.Add(path);
				AddAssembleReferenceWatcher(path);
			}
			else
				Debug.Assert(false, "Can't add " + type + "assembly reference '" + node.Caption + "' (" + node.Url + ")");

			Engine.RequestOnReloadProject();
		}

		public void RemoveAssembly(List<string> assemblies, ReferenceNode node, string type)
		{
			//TODO: Проследить что где-то удаляется вочер для сборки!
			var path = GetAssemblyReferencesString(node);

			if (!string.IsNullOrEmpty(path))
			{
				RemoveAssembleReferenceWatcher(path);
				bool res = assemblies.Remove(path);

				Debug.Assert(res, "Can't remove " + type + "assembly reference '" + node.Caption + "' (" + node.Url + ")");
			}
			else
				Debug.Assert(false, "Can't remove " + type + "assembly reference '" + node.Caption + "' (" + node.Url + ")");

			Engine.RequestOnReloadProject();
		}

		#endregion

		#region Assembly References

		List<string> _assemblyReferences = new List<string>();

		IEnumerable<string> IIdeProject.GetAssemblyReferences()
		{
			return _assemblyReferences.ToArray();
		}

		public void AddAssembly(ReferenceNode node)
		{
			AddAssembly(_assemblyReferences, node, "");
		}

		public void RemoveAssembly(ReferenceNode node)
		{
			RemoveAssembly(_assemblyReferences, node, "");
		}

		#endregion

		#region MacroAssembly References

		List<string> _macroAssemblyReferences = new List<string>();

		IEnumerable<string> IIdeProject.GetMacroAssemblyReferences()
		{
			return _macroAssemblyReferences.ToArray();
		}

		public void AddMacroAssembly(ReferenceNode node)
		{
			AddAssembly(_macroAssemblyReferences, node, "macro ");
		}

		public void RemoveMacroAssembly(ReferenceNode node)
		{
			RemoveAssembly(_macroAssemblyReferences, node, "macro ");
		}

		#endregion

		#region Assemble Reference Watchers

		// Assemble Watcher-ы используются для слежением за изменением файлов сборок.
		// При изменении файла автоматически производится запрос на перезагрузку проекта.

		/// <summary>Watchers for project assemble reference.</summary>
		List<FileSystemWatcher> _assembleReferenceWatchers = new List<FileSystemWatcher>();

		private void AddAssembleReferenceWatcher(string filePath)
		{
			if (!File.Exists(filePath))
				Debug.WriteLine("Assemble " + filePath + " does not exists!");

			try
			{
				string path = Path.GetDirectoryName(filePath);

				if (!Directory.Exists(path))
					return;

				string name = Path.GetFileName(filePath);
				FileSystemWatcher watcher = new FileSystemWatcher(path, name);
				watcher.NotifyFilter = NotifyFilters.LastWrite;
				watcher.Changed += watcher_Changed;
				_assembleReferenceWatchers.Add(watcher);
				watcher.EnableRaisingEvents = true;
			}
			catch (Exception ex)
			{
				Debug.WriteLine(ex.ToString());
				SetCompilerMessages(new[] {new CompilerMessage(Location.Default, ex.Message, 
					MessageKind.Error, Engine, false)}, null);
				throw;
			}
		}

		void RemoveAssembleReferenceWatcher(string filePath)
		{
			if (!File.Exists(filePath))
			{
				Debug.WriteLine("Assemble " + filePath + " does not exists!");
			}
			string path = Path.GetDirectoryName(filePath);
			string name = Path.GetFileName(filePath);

			var index = _assembleReferenceWatchers.FindIndex(w => w.Path == path && w.Filter == name);

			if (index >= 0)
			{
				FileSystemWatcher watcher = _assembleReferenceWatchers[index];
				watcher.Dispose();
				_assembleReferenceWatchers.RemoveAt(index);
			}
		}

		void ResetAssembleReferenceWatchers()
		{
			foreach (FileSystemWatcher watcher in _assembleReferenceWatchers)
				watcher.Dispose();

			_assembleReferenceWatchers.Clear();
		}

		void watcher_Changed(object sender, FileSystemEventArgs e)
		{
			Engine.RequestOnReloadProject();
		}

		#endregion

		#endregion

		internal void AddEditableSource(NemerleSource source)
		{
			ReplaseOrAddSource(source);
		}

		internal void ReplaseOrAddSource(IIdeSource source)
		{
			var fileIndex = source.FileIndex;
			IIdeSource old;
			if (_sourceMap.TryGetValue(fileIndex, out old))
				_sources.Remove(old);

			_sourceMap[fileIndex] = source;
			_sources.Add(source);
			//Engine.RequestOnBuildTypesTree();
		}

		internal void RemoveEditableSource(NemerleSource source)
		{
			var fileIndex = source.FileIndex;
			var taksForSource = GetTaksForSource(source.FileIndex);

			foreach (var task in taksForSource)
				task.DisposeTextLineMarker();

			ReplaseOrAddSource(new FileNemerleSource(fileIndex));
		}

		internal void RemoveSource(IIdeSource source)
		{
			_sourceMap.Remove(source.FileIndex);
			_sources.Remove(source);

			Engine.RequestOnBuildTypesTree();
		}

		public void RemoveSource(string path)
		{
			if (_fileRenamingInProgress)
				return;

			var fileIndex = Location.GetFileIndex(path);

			if (IsFileInProject(fileIndex))
			{
				var source = GetSource(fileIndex);
				Trace.Assert(source != null);
				RemoveSource(source);
			}
		}

		public NemerleSource GetEditableSource(int fileIndex, WindowFrameShowAction action)
		{
			IIdeSource source;

			if (!_sourceMap.TryGetValue(fileIndex, out source))
				throw new ApplicationException("File not in project");

			var nemerleSource = source as NemerleSource;

			if (nemerleSource != null)
				return nemerleSource;

			return OpenEditableSource(fileIndex, action);
		}

		NemerleSource OpenEditableSource(int fileIndex, WindowFrameShowAction action)
		{
			IVsWindowFrame frame = OpenWindowFrame(fileIndex, action);

			if (frame == null)
				throw new ApplicationException("frame (IVsWindowFrame) is null");

			object docData;
			frame.GetProperty((int)__VSFPROPID.VSFPROPID_DocData, out docData);

			// Get the VsTextBuffer
			var buffer = docData as VsTextBuffer;

			if (buffer == null)
			{
				var bufferProvider = docData as IVsTextBufferProvider;

				if (bufferProvider != null)
				{
					IVsTextLines lines;
					ErrorHandler.ThrowOnFailure(bufferProvider.GetTextBuffer(out lines));

					var source = LanguageService.CreateSource(lines);

					return (NemerleSource)source;
				}
			}
			else
			{
				var lines = (IVsTextLines)buffer;
				var source = LanguageService.CreateSource(lines);
				return (NemerleSource)source;
			}


			throw new ApplicationException("can't retrive IVsTextLines from IVsWindowFrame");
		}

		IVsWindowFrame OpenWindowFrame(int fileIndex, WindowFrameShowAction action)
		{
			var path = Location.GetFileName(fileIndex);
			var node = (FileNode)_projectNode.FindChild(path);
			var manager = (FileDocumentManager)node.GetDocumentManager();

			IVsWindowFrame frame;
			// Open the document in rdt.
			ErrorHandler.ThrowOnFailure(manager.Open(false, false, VSConstants.LOGVIEWID_Code,
				out frame, action));

			return frame;
		}

		public IIdeSource GetSource(int fileIndex)
		{
			IIdeSource source;
			return _sourceMap.TryGetValue(fileIndex, out source) ? source : null;
		}

		public IIdeSource GetSource(string filePath)
		{
			return GetSource(Location.GetFileIndex(filePath));
		}

		public bool IsFileInProject(string filePath)
		{
			return IsFileInProject(Location.GetFileIndex(filePath));
		}

		public bool IsFileInProject(int fileIndex)
		{
			return _sourceMap.ContainsKey(fileIndex);
		}

		private static NemerleFileNodeProperties GetNodeProperties(IVsHierarchy hierarchy, uint itemID)
		{
			ErrorHelper.ThrowIsNull(hierarchy, "hierarchy");

			object propertyValue;
			int hr = hierarchy.GetProperty(itemID, (int)__VSHPROPID.VSHPROPID_BrowseObject, out propertyValue);

			if (hr != VSConstants.S_OK)
				throw new ArgumentException("Can't obtain VSHPROPID_BrowseObject for item with ID "
					+ itemID, "itemID", new Win32Exception(hr));

			NemerleFileNodeProperties properties = propertyValue as NemerleFileNodeProperties;

			if (properties == null)
				return new NemerleFileNodeProperties(((NodeProperties)propertyValue).Node);
			else
				return properties;
		}

		#region IIdeProject

		CompilationOptions IIdeProject.GetOptions()
		{
			var options = new CompilationOptions();

			string defineConstants = _projectNode.GetProjectProperty("DefineConstants", false);

			if (!string.IsNullOrEmpty(defineConstants))
			{
				var defines = defineConstants
					.Split(new char[]{';', ' ', '\r', '\n', '\t'}, StringSplitOptions.RemoveEmptyEntries);

				foreach (var define in defines)
					options.DefineConstant(define);
			}

			options.ColorMessages = false;
			options.IgnoreConfusion = true;
			options.GreedyReferences = GetBoolProp("GreedyReferences");
			options.DoNotLoadStdlib = NoStdLib;
			options.DoNotLoadMacros = NoStdMacros;
			options.OutputFileName = ProjectName;
			options.ProjectPath = ProjectFullName;
			options.RootNamespace = RootNamespace;

			return options;
		}

		IEnumerable<IIdeSource> IIdeProject.GetSources()
		{
			return _sources;
		}

		public GotoInfo[] LookupLocationsFromDebugInformation(GotoInfo info)
		{
			var vsSmartOpenScope = (IVsSmartOpenScope)ProjectNode.Site.GetService(typeof(SVsSmartOpenScope));

			return NemerleGoto.LookupLocationsFromPdb(info, vsSmartOpenScope);
		}

		#endregion


		internal void FileBuildActionPropertyChanged(HierarchyNode node, NemerleBuildAction oldAction, NemerleBuildAction newAction)
		{
			if (oldAction == NemerleBuildAction.Compile && newAction != NemerleBuildAction.Compile)
				RemoveSource(node.Url);
			else if (newAction == NemerleBuildAction.Compile && oldAction != NemerleBuildAction.Compile)
				AddSource(node.Url);
		}

		private void FileAdded(object sender, HierarchyEventArgs ergs)
		{
			Debug.Assert(ergs.TextBuffer == null);

			IVsHierarchy hierarchy = (IVsHierarchy)sender;
			NemerleFileNodeProperties nodeProps = GetNodeProperties(hierarchy, ergs.ItemID);

			if (nodeProps.BuildAction != BuildAction.Compile)
				return;

			string path = ergs.FileName;

			AddSource(path);
		}

		public void AddSource(string path)
		{
			if (_fileRenamingInProgress)
				return;

			IIdeSource source = (NemerleSource)LanguageService.GetSource(path); // TODO: VladD2: тут надо искать Source по иерархии, а не по пути!

			try
			{
				if (source == null)
					source = new FileNemerleSource(Location.GetFileIndex(path));
				else
					((NemerleSource)source).UpdateProjectInfo(this);

				_sourceMap.Add(source.FileIndex, source);
				_sources.Add(source);

				Engine.RequestOnBuildTypesTree();
			}
			catch (Exception ex)
			{
				SetCompilerMessages(new[] {new CompilerMessage(Location.Default, ex.Message, 
					MessageKind.Error, Engine, false)}, null);
			}
		}

		private void FileDeleted(object sender, HierarchyEventArgs ergs)
		{
			Debug.Assert(ergs.TextBuffer == null);

			string path = ergs.FileName;
			RemoveSource(path);
		}

		public static ProjectInfo FindProject(IVsHierarchy hierarchy)
		{
			foreach (ProjectInfo proj in _projects)
				if (Utilities.IsSameComObject(proj._hierarchy, hierarchy))
					return proj;

			return null;
		}

		public static ProjectInfo FindProjectByOutput(string fileName)
		{
			var fullPath = Path.GetFullPath(fileName).ToLower();
			ErrorHelper.ThrowIfPathNullOrEmpty(fileName, "fileName");

			foreach (ProjectInfo proj in _projects)
			{
				var outFile = (string)proj._projectNode.VSProject.Project.Properties.Item("OutputFileName").Value ?? "";
				var outPath = (string)proj._projectNode.VSProject.Project.Properties.Item("OutputPath").Value ?? "";
				var dir = (string)proj._projectNode.VSProject.Project.Properties.Item("FullPath").Value ?? "";
				var fullOutPath = Path.GetFullPath(Path.Combine(dir, Path.Combine(outPath, outFile))).ToLower();

				if (fullOutPath == fullPath)
					return proj;
			}

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

		public bool IsDocumentOpening { get; internal set; }

		#region HighlightUsages

		internal void RemoveLastHighlighting(IIdeSource source)
		{
			Debug.WriteLine(">>>> ##### RemoveLastHighlighting!");
			var nsource = source as NemerleSource;
			if (nsource == null)
				return;
			ScanLexer lexer = nsource.Scanner.GetLexer();
			if (lexer == null)
				return;
			lexer.RemoveLastHighlighting();
			nsource.Recolorize(1, source.LineCount);
			Debug.WriteLine("<<<< ##### HighlightUsages!");
		}

		#endregion

		public IEnumerable<NemerleErrorTask> FindTaks(Func<NemerleErrorTask, bool> predicate)
		{
			var tasks = _errorList.Tasks.OfType<NemerleErrorTask>();
			var taksForSource = tasks.Where(predicate);
			return taksForSource;
		}

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

		#region IIdeProject Members

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

		void IIdeProject.ClearAllCompilerMessages()
		{
			lock (_errorList.Tasks)
			{
				_errorList.SuspendRefresh();
				try
				{
					ClearCompilerMessgesTasks(x => true);
				}
				finally { _errorList.ResumeRefresh(); }
			}
		}

		void IIdeProject.SetTopLevelCompilerMessages(IEnumerable<CompilerMessage> messages)
		{
			SetCompilerMessages(messages, null);
		}

		void IIdeProject.SetCompilerMessageForCompileUnit(CompileUnit compileUnit)
		{
			var fileIndex = compileUnit.FileIndex;

			SetCompilerMessages(compileUnit.ParseCompilerMessages, task =>
			{
				var msg = task.CompilerMessage as CompilerMessageForCompileUnit;
				if (msg != null)
					return msg.CompileUnit.FileIndex == fileIndex;

				return false;
			});
		}

		void IIdeProject.ClearMethodCompilerMessages(MemberBuilder member)
		{
			lock (_errorList.Tasks)
			{
				_errorList.SuspendRefresh();
				try
				{
					ClearCompilerMessgesTasks(task =>
					{
						var memberMsg = task.CompilerMessage as CompilerMessageForMethod;
						var result = memberMsg != null && memberMsg.Member == member;
						return result;
					});
				}
				finally { _errorList.ResumeRefresh(); }
			}
		}

		Queue<IEnumerable<CompilerMessage>> _delayedMethodCompilerMessages = new Queue<IEnumerable<CompilerMessage>>();
		TimeSpan _lastTimeOfProcessDelayedMethodCompilerMessages;
		Stopwatch _processDelayedMethodCompilerMessagesTimer = Stopwatch.StartNew();
		
		/// <summary>
		/// Разбирвает очередь _delayedMethodCompilerMessages. В эту очередь помещаются сообщения компилятора
		/// возникающие при типизации тел методов при условии, что уже имеется много сообщений от компилятора.
		/// Этот трюк нужен так как в противном случае GUI VS начинает дико тормозить в случае наличия множества
		/// ошибок.
		/// </summary>
		internal void ProcessDelayedMethodCompilerMessages()
		{
			var current = _processDelayedMethodCompilerMessagesTimer.Elapsed;
			var delta   = current - _lastTimeOfProcessDelayedMethodCompilerMessages;

			if (delta < TimeSpan.FromSeconds(1))
				return;

			var queue = _delayedMethodCompilerMessages;

			if (queue.Count <= 0)
				return;

			_lastTimeOfProcessDelayedMethodCompilerMessages = current;

			_errorList.SuspendRefresh();
			try
			{
				while (queue.Count > 0)
				{
					var messages = queue.Dequeue(); 
					AddNewCompilerMessages(messages.Select(m => TranslateSecondarySourceMessage(m)).Where(m => m != null));
				}
			}
			finally { _errorList.ResumeRefresh(); }
		}
		void IIdeProject.SetMethodCompilerMessages(MemberBuilder member, IEnumerable<CompilerMessage> messages)
		{
			if (!IsMemberVersionCorrect(member))
				return;

			var count = _errorList.Tasks.Count;

			if (count > 100)
				_delayedMethodCompilerMessages.Enqueue(messages);
			else
			SetCompilerMessages(messages, null /* messges related to member was deleted be ClearMethodCompilerMessages() */);
			// Following for debugging purpose:
			//var res = _errorList.Tasks.OfType<NemerleErrorTask>().GroupBy(x => x.ToString()).Where(g => g.Count() > 1).ToArray();
			//if (res.Length > 0)
			//	Test(res, Engine.TypesTreeVersion);
		}

		void IIdeProject.TypesTreeCreated()
		{
			foreach (IIdeSource source in _sources)
			{
				var nemerleSource = source as NemerleSource;

				if (nemerleSource != null)
				{
					var lineCount = nemerleSource.GetLineCount();

					if (lineCount > 0)
						nemerleSource.Recolorize(0, lineCount - 1);
				}
			}
		}

		// Транслирует привязку сообщения к позиции в secondary файле в привязку к позиции в primary файле.
		// Primary файл - исходный aspx, secondary - автосгенерированный по нему исходный код на Немерле
		private CompilerMessage TranslateSecondarySourceMessage(CompilerMessage message)
		{
			var source = GetSource(message.Location.FileIndex) as NemerleSource;

			if (source != null && source.IsSecondarySource)
			{
				var primaryLocation = source.MapSecondaryToPrimaryLocation(message.Location);

				if (primaryLocation.Line == 1 && primaryLocation.Column == 1) // && message.Kind == MessageKind.Error)
				{
					// Исключим из списка сообщения, относящиеся исключительно к автосгенерированному (по исходному aspx) файлу.
					// Как правило такие сообщения вызваны не совсем корректной работой кодогенератора, 
					// использующего в design time упрощенный режим генерации
					return null;
				}

				CompilerMessage translatedMessage = null;

				if (message is CompilerMessageForMethod)
				{
					var methodMessage = (CompilerMessageForMethod)message;
					translatedMessage = new CompilerMessageForMethod(primaryLocation, methodMessage.Msg, methodMessage.Kind, methodMessage.Engin, methodMessage.IsRelated, methodMessage.Member);
				}
				else if (message is CompilerMessageForCompileUnit)
				{
					var unitMessage = (CompilerMessageForCompileUnit)message;
					translatedMessage = new CompilerMessageForCompileUnit(primaryLocation, unitMessage.Msg, unitMessage.Kind, unitMessage.Engin, unitMessage.IsRelated);
					((CompilerMessageForCompileUnit)translatedMessage).CompileUnit = unitMessage.CompileUnit;
				}
				else
					translatedMessage = new CompilerMessage(primaryLocation, message.Msg, message.Kind, message.Engin, message.IsRelated);

				// трансляции позиций для вложенных сообщений
				translatedMessage.RelatedMessages.AddRange(message.RelatedMessages.Select(m => TranslateSecondarySourceMessage(m)).Where(m => m != null));

				return translatedMessage;
			}
			else
				return message;
		}

		public static string GetAssemblyReferencesString(ReferenceNode node)
		{
			string assemblyId = null;
			var prjRef = node as NemerleProjectReferenceNode;
			if (prjRef != null) // Is project reference...
			{
				assemblyId = prjRef.ReferencedProjectOutputPath; // calc real assembly name.
			}
			else if (node.Url != null) //IT: if dll does not exist, the Url will be null.
				assemblyId = node.Url;
			else
			{
				//TODO: Notify user about reference does not exist. 
			}

			return assemblyId;
		}

		public void SetStatusText(string text) // implemetn IIdeProject
		{
			LanguageService.SetStatusBarText(text);
			Debug.WriteLine(text);
		}

		public void AddUnimplementedMembers(IIdeSource source, TypeBuilder ty,
			IEnumerable<IGrouping<FixedType.Class, IMember>> unimplementedMembers)
		{
			using (var form = new ImplementMembersForm((NemerleSource)source, ty, unimplementedMembers))
			{
				form.ShowDialog();
			}
		}

		public void AddOverrideMembers(IIdeSource source, TypeBuilder ty, IEnumerable<IMember> notOverriden)
		{
			var ty2 = ty.GetMemType();
			var notOverriden2 = notOverriden.Select(m => new { ty2, m }).GroupBy(x => x.ty2, x => x.m);
			using (var form = new ImplementMembersForm((NemerleSource)source, ty, notOverriden2))
			{
				form.ShowDialog();
			}
		}

		public void SetHighlights(IIdeSource source, IEnumerable<GotoInfo> highlights)
		{
			var isPermanent = false;

			var nsource = source as NemerleSource;

			if (nsource == null)
				return;

			ScanLexer lexer = nsource.Scanner.GetLexer();

			if (lexer == null)
				return;

			if (isPermanent)
				lexer.AddHighlighting(highlights);
			else
				lexer.SetHoverHighlights(highlights);

			nsource.Recolorize(1, source.LineCount);
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
			var taskGroups = tasks.GroupBy(t => t.CompilerMessage.Location.FileIndex);

			foreach (var taskGroup in taskGroups)
			{
				IVsTextLines buffer = null;

				// Маркеры для сообщений, сгенерированных при компиляции secondary 
				// файлов, должны устанавливаться в primary буфере
				if (NemerleSource.HasSecondarySource(taskGroup.Key))
				{
					var secondarySource = GetSource(NemerleSource.GetSecondaryFileIndex(taskGroup.Key)) as NemerleSource;
					if (secondarySource != null)
						buffer = secondarySource.GetPrimaryTextLines();
				}
				else
				{
					var source = GetSource(taskGroup.Key) as NemerleSource;
					if (source != null)
						buffer = source.GetTextLines();
				}

				if (buffer != null)
				{
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
				{
					if (predicate(errorTask))
						tasks.RemoveAt(i--);
					else
					{
					}
				}
			}
		}

		void SetCompilerMessages(IEnumerable<CompilerMessage> messages, Predicate<NemerleErrorTask> predicate)
		{
			lock (_errorList.Tasks)
			{
				_errorList.SuspendRefresh();
				try
				{
					if (predicate != null)
						ClearCompilerMessgesTasks(predicate);

					AddNewCompilerMessages(messages.Select(m => TranslateSecondarySourceMessage(m)).Where(m => m != null));
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

		internal void BeginRenameFile()
		{
			_fileRenamingInProgress = true;
		}

		internal void EndRenameFile()
		{
			_fileRenamingInProgress = false;
		}

		internal void RenameSource(string oldFileName, string newFileName)
		{
			var source = GetSource(oldFileName);
			RemoveSource(source);

			var nemerleSource = source as NemerleSource;

			if (nemerleSource != null)
				nemerleSource.Rename(newFileName);
			else
				AddSource(newFileName);
		}
	}
}
