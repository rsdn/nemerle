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
using SourceMap = System.Collections.Generic.Dictionary<int, Nemerle.Completion2.ISource>;
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

    readonly SourceMap _sourceMap = new SourceMap();
    readonly List<ISource> _sources = new List<ISource>();

    private string _projectLocation;

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

      _engine = new Engine(this, new TraceWriter(), false); // it enables parser working.

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
      Engine.Close();
      IsCosed = true;
      _projects.Remove(this);
      _errorList.Tasks.Clear();
      _sources.Clear();
      _sourceMap.Clear();
      //TODO: VladD2: Удалить из очередей AsyncWorker все запросы и ответы связанные с данным проектом!
    }

    public string ProjectName { get { return _projectNode.VSProject.Project.Name; } }

    int _buildTypedtreeCount;
    readonly List<MethodBuilderEx> _methodsCheckQueue = new List<MethodBuilderEx>(100);

    private Engine _engine;
    public Engine Engine
    {
      [DebuggerNonUserCode]
      get { ManagerClass.Instance = _engine; return _engine; }
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

    internal void AddEditableSource(NemerleSource source)
    {
      ReplaseOrAddSource(source);
    }

    void ReplaseOrAddSource(ISource source)
    {
      var fileIndex = source.FileIndex;
      ISource old;
      if (_sourceMap.TryGetValue(fileIndex, out old))
        _sources.Remove(old);

      _sourceMap[fileIndex] = source;
      _sources.Add(source);
    }

    internal void RemoveEditableSource(NemerleSource source)
    {
      var fileIndex = source.FileIndex;
      var taksForSource = GetTaksForSource(source.FileIndex);

      foreach (var task in taksForSource)
        task.DisposeTextLineMarker();

      ReplaseOrAddSource(new FileNemerleSource(fileIndex));
    }

    internal void RemoveSource(ISource source)
    {
      _sourceMap.Remove(source.FileIndex);
      _sources.Remove(source);
    }

    public ISource GetSource(int fileIndex)
    {
      ISource source;
      return _sourceMap.TryGetValue(fileIndex, out source) ? source : null;
    }

    public ISource GetSource(string filePath)
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
        var defines = defineConstants.Replace(" \t\r\n", String.Empty).Split(';');

        foreach (var define in defines)
          options.DefineConstant(define);
      }

      options.ColorMessages = false;
      options.IgnoreConfusion = true;
      options.GreedyReferences = GetBoolProp("GreedyReferences");
      options.DoNotLoadStdlib = NoStdLib;
      options.DoNotLoadMacros = NoStdMacros;

      return options;
    }

    IEnumerable<ISource> IIdeProject.GetSources()
    {
      return _sources;
    }

    public GotoInfo[] LookupLocationsFromDebugInformation(GotoInfo info)
    { 
      var vsSmartOpenScope = (IVsSmartOpenScope)ProjectNode.Site.GetService(typeof(SVsSmartOpenScope));

      return NemerleGoto.LookupLocationsFromPdb(info, vsSmartOpenScope);
    }

    #endregion

    private void FileAdded(object sender, HierarchyEventArgs ergs)
    {
      Debug.Assert(ergs.TextBuffer == null);

      IVsHierarchy hierarchy = (IVsHierarchy)sender;
      NemerleFileNodeProperties nodeProps = GetNodeProperties(hierarchy, ergs.ItemID);

      if (nodeProps.BuildAction != BuildAction.Compile)
        return;

      string path = ergs.FileName;

      ISource source = (NemerleSource)LanguageService.GetSource(path);

      try
      {
        if (source == null)
          source = new FileNemerleSource(Location.GetFileIndex(path));

        _sourceMap.Add(source.FileIndex, source);
        _sources.Add(source);
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
      var fileIndex = Location.GetFileIndex(path);

      if (IsFileInProject(fileIndex))
      {
        var source = GetSource(fileIndex);
        _sourceMap.Remove(fileIndex);
        _sources.Remove(source);
      }
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

    #region HighlightUsages

    internal void RemoveLastHighlighting(ISource source)
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
        return msg != null && msg.CompileUnit.FileIndex == fileIndex;
      });
    }

    void IIdeProject.SetMethodCompilerMessages(MemberBuilder member, IEnumerable<CompilerMessage> messages)
    {
      if (!IsMemberVersionCorrect(member))
        return;

      //messages = messages.Distinct(CompilerMessageEqComparer.Instance);

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

    IEnumerable<string> IIdeProject.GetAssemblyReferences()
    {
      ResetAssembleReferenceWatchers(); //TODO: Этот метод нельзя вызывать из рабочего потока!!!

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

    public void SetStatusText(string text) // implemetn IIdeProject
    {
      LanguageService.SetStatusBarText(text);
      Debug.WriteLine(text);
    }

    public void AddUnimplementedMembers(ISource source, TypeBuilder ty, IEnumerable<IMember> unimplementedMembers)
		{
			using (var form = new ImplementMembersForm((NemerleSource)source, ty, unimplementedMembers))
			{
				form.ShowDialog();
			}
		}

    public void AddOverrideMembers(ISource source, TypeBuilder ty, IEnumerable<IMember> notOverriden)
		{
			using (var form = new ImplementMembersForm((NemerleSource)source, ty, notOverriden))
			{
				form.ShowDialog();
			}
		}

    public void SetHighlights(ISource source, IEnumerable<GotoInfo> highlights)
    {
      var isPermanent = false;

      var nsource = source as NemerleSource;

      if (nsource == null)
        return;

      ScanLexer lexer = nsource.Scanner.GetLexer();

      if (lexer == null)
        return;

      if(isPermanent)
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
        var source = GetSource(taskGroup.Key) as NemerleSource;

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
          if (predicate(errorTask))
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
          if (predicate != null)
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
