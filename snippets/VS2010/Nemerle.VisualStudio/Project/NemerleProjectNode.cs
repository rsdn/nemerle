using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Windows.Design.Host;

using Nemerle.VisualStudio.LanguageService;
using Nemerle.VisualStudio.Project.PropertyPages;
using Nemerle.VisualStudio.Helpers;
using Nemerle.VisualStudio.WPFProviders;

using PkgUtils = Microsoft.VisualStudio.Project.Utilities;
using OleConstants = Microsoft.VisualStudio.OLE.Interop.Constants;
using MsBuildProject = Microsoft.Build.BuildEngine.Project;
using VsCommands = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;

using MSBuild = Microsoft.Build.Evaluation;
using OsProcess = System.Diagnostics.Process;
using System.Diagnostics.CodeAnalysis;
using Microsoft.VisualStudio.Shell;

namespace Nemerle.VisualStudio.Project
{
	internal enum GeneralPropertyPageTag
	{
		AssemblyName,
		OutputType,
		RootNamespace,
		StartupObject,
		ApplicationIcon,
		TargetPlatform,
		TargetPlatformLocation
	}

	[ComVisible(true)]
	[CLSCompliant(false)]
	[ClassInterface(ClassInterfaceType.AutoDual)]
	[Guid(NemerleConstants.ProjectNodeGuidString)]
	public class NemerleProjectNode : ProjectNode, IVsProjectSpecificEditorMap2
	{
		#region Fields

		NemerleLanguageService _languageService;

		#endregion

		#region Init

		public NemerleProjectNode(NemerlePackage pkg)
		{
			FileTemplateProcessor = new NemerleTokenProcessor();
			CanFileNodesHaveChilds = true;
			SupportsProjectDesigner = true;

			OleServiceProvider.AddService(typeof(VSLangProj.VSProject), VSProject, false);

			// Store the number of images in ProjectNode so we know the offset of the Nemerle icons.
			//
			_imageOffset = ImageHandler.ImageList.Images.Count;

			foreach (Image img in NemerleImageList.Images)
				ImageHandler.ImageList.Images.Add(img);

			InitializeCATIDs();

			CanProjectDeleteItems = true;
		}

		public override int SetSite(Microsoft.VisualStudio.OLE.Interop.IServiceProvider site)
		{
			var result = base.SetSite(site);
			_languageService = Utils.GetService<NemerleLanguageService>(Site);
			return result;
		}

		/// <summary>
		/// Provide mapping from our browse objects and automation objects to our CATIDs
		/// </summary>
		void InitializeCATIDs()
		{
			// The following properties classes are specific to Nemerle so we can use their GUIDs directly
			//
			AddCATIDMapping(typeof(NemerleProjectNodeProperties), typeof(NemerleProjectNodeProperties).GUID);
			AddCATIDMapping(typeof(NemerleFileNodeProperties), typeof(NemerleFileNodeProperties).GUID);
			AddCATIDMapping(typeof(NemerleOAFileItem), typeof(NemerleOAFileItem).GUID);

			// The following are not specific to Nemerle and as such we need a separate GUID
			// (we simply used guidgen.exe to create new guids)
			//
			AddCATIDMapping(typeof(FolderNodeProperties), new Guid(NemerleConstants.FolderNodePropertiesGuidString));

			// This one we use the same as Nemerle file nodes since both refer to files
			//
			AddCATIDMapping(typeof(FileNodeProperties), typeof(NemerleFileNodeProperties).GUID);

			// Because our property page pass itself as the object to display in its grid,
			// we need to make it have the same CATID as the browse object of the project node
			// so that filtering is possible.
			//
			AddCATIDMapping(typeof(NemerleGeneralPropertyPage), typeof(NemerleProjectNodeProperties).GUID);

			// We could also provide CATIDs for references and the references container node, if we wanted to.
		}

		private static ImageList LoadProjectImageList()
		{
			// Make the name of resource bitmap.
			// It's bitmap used in project ImageList.
			//
			Type type = typeof(NemerleProjectNode);
			Assembly assembly = type.Assembly;
			Stream imageStream = assembly.GetManifestResourceStream(
				NemerleConstants.ProjectImageListName);

			Debug.Assert(imageStream != null);

			return PkgUtils.GetImageList(imageStream);
		}

		#endregion

		#region Properties

		public new NemerlePackage Package
		{
			get { return (NemerlePackage)base.Package; }
		}

		private bool _showAllFilesEnabled;
		/// <summary>
		/// Gets if the ShowAllFiles is enabled or not.
		/// </summary>
		/// <value>true if the ShowAllFiles option is enabled, false otherwise.</value>
		public bool ShowAllFilesEnabled
		{
			get { return _showAllFilesEnabled; }
			private set { _showAllFilesEnabled = value; }
		}

		public string OutputFileName
		{
			get
			{
				string assemblyName =
					ProjectMgr.GetProjectProperty(
						GeneralPropertyPageTag.AssemblyName.ToString(), true);

				string outputTypeAsString =
					ProjectMgr.GetProjectProperty(
						GeneralPropertyPageTag.OutputType.ToString(), false);

				OutputType outputType =
					(OutputType)Enum.Parse(typeof(OutputType), outputTypeAsString);

				return assemblyName + GetOutputExtension(outputType);
			}
		}

		private VSLangProj.VSProject _vsProject;
		protected internal VSLangProj.VSProject VSProject
		{
			get
			{
				if (_vsProject == null)
					_vsProject = new OAVSProject(this);
				return _vsProject;
			}
		}

		System.IServiceProvider _serviceProvider;

		public IServiceProvider GetServiceProvider()
		{
			if (_serviceProvider == null)
			{
				Microsoft.VisualStudio.OLE.Interop.IServiceProvider site;
				ErrorHandler.ThrowOnFailure(InteropSafeHierarchy.GetSite(out site));
				_serviceProvider = new ServiceProvider(site);
			}

			return _serviceProvider;
		}

		public IVsHierarchy InteropSafeHierarchy
		{
			get
			{
				IntPtr unknownPtr = PkgUtils.QueryInterfaceIUnknown(this);

				if (unknownPtr == IntPtr.Zero)
					return null;

				return (IVsHierarchy)Marshal.GetObjectForIUnknown(unknownPtr);
			}
		}

		private ProjectInfo _projectInfo;
		public ProjectInfo ProjectInfo
		{
			get { return _projectInfo; }
		}

		private static ImageList _nemerleImageList = LoadProjectImageList();

		static int _imageOffset;

		public static ImageList NemerleImageList
		{
			get { return _nemerleImageList; }
		}

		private DesignerContext _designerContext;

		protected internal DesignerContext DesignerContext
		{
			get
			{
				//Set the RuntimeNameProvider so the XAML designer will call it when items are added to
				//a design surface. Since the provider does not depend on an item context, we provide it at 
				//the project level.

				if (_designerContext != null)
					return _designerContext;


				_designerContext =
					new DesignerContext()
					{
						RuntimeNameProvider = new NemerleRuntimeNameProvider(),
						//EventBindingProvider = new NemerleEventBindingProvider()

					};

				return _designerContext;
			}
		}

		#endregion

		public override object GetProperty(int propId)
		{
			// Fix MS bug
			switch ((__VSHPROPID)propId)
			{
				case __VSHPROPID.VSHPROPID_DefaultNamespace:
					TokenProcessor processor = new TokenProcessor();
					return processor.GetFileNamespace(this.Url, this);

				case __VSHPROPID.VSHPROPID_DefaultEnableBuildProjectCfg:
					return base.GetProperty(propId);

				default: return base.GetProperty(propId);
			}
		}

		/// <summary>
		/// Creates and returns the ProjectElement for a folder item.
		/// </summary>
		/// <param name="folder">Path of the folder.</param>
		/// <returns>ProjectElement for the folder item.</returns>
		internal ProjectElement CreateMsBuildFolderProjectElement(string folder)
		{
			return AddFolderToMsBuild(folder);
		}

		/// <summary>
		/// Creates and returns the ProjectElement for a file item.
		/// </summary>
		/// <param name="file">Path of the file.</param>
		/// <returns>ProjectElement for the file item.</returns>
		internal ProjectElement CreateMsBuildFileProjectElement(string file)
		{
			return AddFileToMsBuild(file);
		}

		/// <summary>
		/// Toggles the state of Show all files
		/// </summary>
		/// <returns>S_OK if it's possible to toggle the state, OLECMDERR_E_NOTSUPPORTED if not</returns>
		internal int ToggleShowAllFiles()
		{
			if (ProjectMgr == null || ProjectMgr.IsClosed)
				return (int)OleConstants.OLECMDERR_E_NOTSUPPORTED;

			((NemerlePackage)ProjectMgr.Package).SetWaitCursor();

			ShowAllFilesEnabled = !ShowAllFilesEnabled;

			if (ShowAllFilesEnabled)
				NemerleProjectMembers.AddNonMemberItems(this);
			else
				NemerleProjectMembers.RemoveNonMemberItems(this);

			return NativeMethods.S_OK;
		}

		#region Overridden Properties

		public override int ImageIndex { get { return _imageOffset + NemerleConstants.ImageListIndex.NemerleProject; } }
		public override Guid ProjectGuid { get { return typeof(NemerleProjectFactory).GUID; } }
		public override string ProjectType { get { return NemerleConstants.LanguageName; } }
		internal override object Object { get { return VSProject; } }

		protected override ReferenceContainerNode CreateReferenceContainerNode()
		{
			return new NemerleReferenceContainerNode(this);
		}

		/// <summary>
		/// Creates and returns the folder node object for Wix projects.
		/// </summary>
		/// <param name="path">Folder path.</param>
		/// <param name="element">MSBuild element.</param>
		/// <returns>Returns newly created Folder Node object.</returns>
		[SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "1#")]
		protected internal override FolderNode CreateFolderNode(string path, ProjectElement element)
		{
			ErrorHelper.ThrowIsNull(element, "element");

			return new NemerleFolderNode(this, path, element, element.IsVirtual);
		}

		#endregion

		#region Overridden Methods

		public override ProjectOptions GetProjectOptions(string config)
		{
			// Woraround of bug in ProjectNode.GetProjectOptions()
			// This reset "options" field.
			SetProjectFileDirty(IsProjectFileDirty);

			return base.GetProjectOptions(config);
		}

		/// <summary>Run project output without debugging.</summary>
		/// <returns>true - if it's OK</returns>
		static bool StartNoDebug(EnvDTE.DTE dte)
		{
			var startupProjects = (object[])dte.Solution.SolutionBuild.StartupProjects;

			if (startupProjects.Length < 1)
				throw new ApplicationException("No startup projects.");

			var startupProjectFullName = Path.GetFullPath(Path.Combine(Path.GetDirectoryName(dte.Solution.FullName), (string)startupProjects[0]));
			NemerleOAProject nemerleOAProject = GetProject(dte, startupProjectFullName) as NemerleOAProject;
			
			if (nemerleOAProject == null)
				return false;

			var projectNode = nemerleOAProject.Project.ProjectMgr;
			var currentConfigName = Utilities.GetActiveConfigurationName(nemerleOAProject);

			projectNode.SetConfiguration(currentConfigName);

			dte.ExecuteCommand("File.SaveAll", "");

			bool ok;
			projectNode.BuildTarget("Build", out ok);

			if (!ok)
			{
				var message = "There were build errors. Would you like to continue and run the last successful build?";
				OLEMSGICON icon = OLEMSGICON.OLEMSGICON_QUERY;
				OLEMSGBUTTON buttons = OLEMSGBUTTON.OLEMSGBUTTON_YESNO;
				OLEMSGDEFBUTTON defaultButton = OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST;
				var res = VsShellUtilities.ShowMessageBox(projectNode.Site,
					message, NemerleConstants.ProductName, icon, buttons, defaultButton);

				if (res == NativeMethods.IDNO)
					return true;
			}

			var path = projectNode.GetProjectProperty("StartProgram");

			if (string.IsNullOrEmpty(path))
				path = projectNode.GetOutputAssembly(currentConfigName);

			if (!File.Exists(path))
				throw new ApplicationException("Visual Studio cannot start debugging because the debug target '"
					+ path + "' is missing. Please build the project and retry, or set the OutputPath and AssemblyName properties appropriately to point at the correct location for the target assembly.");

			if (string.Compare(Path.GetExtension(path), ".dll", StringComparison.InvariantCultureIgnoreCase) == 0)
				throw new ApplicationException("A project with an Output Type of Class Library cannot be started directly.\nAlso you can't use DLL as debug target.\n\nIn order to debug this project, add an executable project to this solution which references the library project. Set the executable project as the startup project.");

			var isConsole = PEReader.IsConsole(path);

			var cmdArgs = projectNode.GetProjectProperty("CmdArgs");
			var workingDirectory = projectNode.GetProjectProperty("WorkingDirectory");

			if (!string.IsNullOrEmpty(workingDirectory) && !Path.IsPathRooted(workingDirectory))
				workingDirectory = Path.Combine(projectNode.BaseURI.AbsoluteUrl, workingDirectory);

			if (string.IsNullOrEmpty(workingDirectory))
				workingDirectory = Path.GetDirectoryName(path);

			if (!Directory.Exists(workingDirectory))
				throw new ApplicationException("The working directory '" + workingDirectory + "' not exists.");

			var psi = new ProcessStartInfo();
			string cmdFilePath = null;

			if (isConsole)
			{
				// Make temp cmd-file and run it...
				cmdFilePath = Path.Combine(Path.GetDirectoryName(path), Path.GetTempFileName());
				cmdFilePath = Path.ChangeExtension(cmdFilePath, "cmd");
				var oemCodePage = CultureInfo.CurrentCulture.TextInfo.OEMCodePage;
				var consoleEncoding = Encoding.GetEncoding(oemCodePage);
				File.WriteAllText(cmdFilePath, "@echo off\n\"" + path + "\" " + cmdArgs + "\npause",
						consoleEncoding);
				psi.FileName = cmdFilePath;
			}
			else
			{
				psi.FileName = path;
				psi.Arguments = cmdArgs;
			}

			psi.WorkingDirectory = workingDirectory;

			var process = OsProcess.Start(psi);

			if (isConsole)
				DeleteTempCmdFile(process, cmdFilePath);

			return true;
		}

		private static EnvDTE.Project FindProject(EnvDTE.ProjectItem project, string projectFullName)
		{
			EnvDTE.Project subProject = project.Object as EnvDTE.Project;

			if (subProject == null)
				return null;

			if (!string.IsNullOrWhiteSpace(subProject.FullName) && Utils.Eq(Path.GetFullPath(subProject.FullName), projectFullName))
				return subProject;


			if (subProject.ProjectItems == null)
				return null;

			foreach (EnvDTE.ProjectItem project2 in subProject.ProjectItems)
			{
				Debug.WriteLine(project2.Name);
				var res = FindProject(project2, projectFullName);
				if (res != null)
					return res;
			}

			return null;
		}

		private static EnvDTE.Project GetProject(EnvDTE.DTE dte, string startupProjectFullName)
		{
			// FIXME! Следующая строка перестала раблотать в VS 2010!
			//var nemerleOAProject = dte.Solution.FindProjectItem(startupProjectFullName);
			//nemerleOAProject = dte.Solution.FindProjectItem(startupProjectFullName) as NemerleOAProject; 
			foreach (EnvDTE.Project project in dte.Solution.Projects)
			{
					foreach (var project2 in project.ProjectItems)
					{
						EnvDTE.ProjectItem project3 = project2 as EnvDTE.ProjectItem;
						var res = FindProject(project3, startupProjectFullName);
						if (res != null)
							return res;
					}
			}

			return null;
		}

		static void DeleteTempCmdFile(OsProcess process, string cmdFilePath)
		{
			Action action = () => { process.WaitForExit(20000); File.Delete(cmdFilePath); };
			action.BeginInvoke(null, null);
		}

		protected override int InternalExecCommand(Guid cmdGroup, uint cmdId, uint cmdExecOpt, IntPtr vaIn, IntPtr vaOut, CommandOrigin commandOrigin)
		{
			if (cmdGroup == Microsoft.VisualStudio.Shell.VsMenus.guidStandardCommandSet97)
			{
				switch ((VsCommands)cmdId)
				{
					case VsCommands.DebugProcesses:
						break;
					case VsCommands.Start:
						break;
					case VsCommands.StartNoDebug:
						EnvDTE.DTE dte = (EnvDTE.DTE)ProjectMgr.GetService(typeof(EnvDTE.DTE));
						if (StartNoDebug(dte))
							return VSConstants.S_OK;
						else
							break;
					default:
						break;
				}
			}

			return base.InternalExecCommand(cmdGroup, cmdId, cmdExecOpt, vaIn, vaOut, commandOrigin);
		}

		protected internal override void ProcessReferences()
		{
			IReferenceContainer container = GetReferenceContainer();
			if (null == container)
			{
				// Process References
				ReferenceContainerNode referencesFolder = CreateReferenceContainerNode();
				if (null == referencesFolder)
				{
					// This project type does not support references or there is a problem
					// creating the reference container node.
					// In both cases there is no point to try to process references, so exit.
					return;
				}
				this.AddChild(referencesFolder);
				container = referencesFolder;

				var macroReferencesFolder = new NemerleMacroReferenceContainerNode(this);
				this.AddChild(macroReferencesFolder);

				macroReferencesFolder.LoadReferencesFromBuildProject(BuildProject);
			}

			// Load the referernces.
			container.LoadReferencesFromBuildProject(BuildProject);
		}

		protected internal override void ProcessFolders()
		{
			// Process Folders (useful to persist empty folder)
			var folders = BuildProject.GetItems(ProjectFileConstants.Folder);
			foreach (var folder in folders)
			{
				string strPath = folder.EvaluatedInclude;

				// We do not need any special logic for assuring that a folder is only added once to the ui hierarchy.
				// The below method will only add once the folder to the ui hierarchy
				this.CreateFolderNodes(strPath);
			}
		}

		/// <summary>
		/// Walks the subpaths of a project relative path and checks if the folder nodes 
		/// hierarchy is already there, if not creates it.
		/// </summary>
		/// <param name="path">Path of the folder, can be relative to project or absolute</param>
		public override HierarchyNode CreateFolderNodes(string path)
		{
			ErrorHelper.ThrowIsNullOrEmpty(path, "path");

			if (Path.IsPathRooted(path))
			{
				// Ensure we are using a relative path
				if (String.Compare(ProjectFolder, 0, path, 0, ProjectFolder.Length, StringComparison.OrdinalIgnoreCase) == 0)
					path = path.Substring(ProjectFolder.Length);
				else
				{
					Debug.Assert(false, "Folder path is rooted, but not subpath of ProjectFolder.");
				}
			}

			string[] parts;
			HierarchyNode curParent;

			parts = path.Split(Path.DirectorySeparatorChar);
			path = String.Empty;
			curParent = this;

			// now we have an array of subparts....
			for (int i = 0; i < parts.Length; i++)
			{
				if (parts[i].Length > 0)
				{
					path += parts[i] + "\\";
					curParent = VerifySubFolderExists(path, curParent);
				}
			}

			return curParent;
		}

		/// <summary>
		/// Loads file items from the project file into the hierarchy.
		/// </summary>
		protected internal override void ProcessFiles()
		{
			List<String> subitemsKeys = new List<String>();
			var subitems = new Dictionary<String, MSBuild.ProjectItem>();

			// Define a set for our build items. The value does not really matter here.
			var items = new Dictionary<String, MSBuild.ProjectItem>();

			// Process Files
			var projectFiles = BuildProject.AllEvaluatedItems;

			foreach (var item in projectFiles)
			{
				// Ignore the item if it is a reference or folder
				if (this.FilterItemTypeToBeAddedToHierarchy(item.ItemType))
					continue;

				// MSBuilds tasks/targets can create items (such as object files),
				// such items are not part of the project per say, and should not be displayed.
				// so ignore those items.
				if (!this.IsItemTypeFileType(item.ItemType))
					continue;

				// If the item is already contained do nothing.
				// TODO: possibly report in the error list that the the item is already contained in the project file similar to Language projects.
				if (items.ContainsKey(item.EvaluatedInclude.ToUpperInvariant()))
					continue;

				// Make sure that we do not want to add the item, dependent, or independent twice to the ui hierarchy
				items.Add(item.EvaluatedInclude.ToUpperInvariant(), item);

				string dependentOf = item.GetMetadataValue(ProjectFileConstants.DependentUpon);

				if (!this.CanFileNodesHaveChilds || String.IsNullOrEmpty(dependentOf))
					AddIndependentFileNode(item);
				else
				{
					// We will process dependent items later.
					// Note that we use 2 lists as we want to remove elements from
					// the collection as we loop through it
					subitemsKeys.Add(item.EvaluatedInclude);
					subitems.Add(item.EvaluatedInclude, item);
				}
			}

			// Now process the dependent items.
			if (this.CanFileNodesHaveChilds)
				ProcessDependentFileNodes(subitemsKeys, subitems);
		}

		/// <summary>
		/// Add an item to the hierarchy based on the item path
		/// </summary>
		/// <param name="item">Item to add</param>
		/// <returns>Added node</returns>
		private HierarchyNode AddIndependentFileNode(MSBuild.ProjectItem item)
		{
			return AddFileNodeToNode(item, GetItemParentNode(item));
		}

		/// <summary>
		/// Add a file node to the hierarchy
		/// </summary>
		/// <param name="item">msbuild item to add</param>
		/// <param name="parentNode">Parent Node</param>
		/// <returns>Added node</returns>
		private HierarchyNode AddFileNodeToNode(MSBuild.ProjectItem item, HierarchyNode parentNode)
		{
			FileNode node = this.CreateFileNode(new ProjectElement(this, item, false));
			parentNode.AddChild(node);
			return node;
		}

		static bool IsLinkNode(MSBuild.ProjectItem item)
		{
			return item.HasMetadata("Link");
		}

		/// <summary>
		/// Get the parent node of an msbuild item
		/// </summary>
		/// <param name="item">msbuild item</param>
		/// <returns>parent node</returns>
		private HierarchyNode GetItemParentNode(MSBuild.ProjectItem item)
		{
			var isLink = IsLinkNode(item);
			var path = isLink ? item.GetMetadataValue("Link") : item.EvaluatedInclude;
			var dir = Path.GetDirectoryName(path);
			return Path.IsPathRooted(dir) || string.IsNullOrEmpty(dir)
				? this : CreateFolderNodes(dir);
		}

		NemerleOAProject _automationObject;
		/// <summary>
		/// Gets the automation object for the project node.
		/// </summary>
		/// <returns>An instance of an EnvDTE.Project implementation object representing the automation object for the project.</returns>
		public override object GetAutomationObject()
		{
			if (_automationObject == null)
				_automationObject = new NemerleOAProject(this);
			return _automationObject;
		}

		protected override void SetOutputLogger(IVsOutputWindowPane output)
		{
			//base.SetOutputLogger(output);
			// Create our logger, if it was not specified
			if (BuildLogger == null)
			{
				// Because we may be aggregated, we need to make sure to get the outer IVsHierarchy
				IntPtr unknown = IntPtr.Zero;
				IVsHierarchy hierarchy = null;
				try
				{
					unknown = Marshal.GetIUnknownForObject(this);
					hierarchy = Marshal.GetTypedObjectForIUnknown(unknown, typeof(IVsHierarchy)) as IVsHierarchy;
				}
				finally
				{
					if (unknown != IntPtr.Zero)
						Marshal.Release(unknown);
				}
				// Create the logger
				BuildLogger = new NemerleIdeBuildLogger(output, this.TaskProvider, hierarchy);

				// To retrive the verbosity level, the build logger depends on the registry root 
				// (otherwise it will used an hardcoded default)
				ILocalRegistry2 registry = this.GetService(typeof(SLocalRegistry)) as ILocalRegistry2;
				if (null != registry)
				{
					string registryRoot;
					registry.GetLocalRegistryRoot(out registryRoot);
					IDEBuildLogger logger = this.BuildLogger as IDEBuildLogger;
					if (!String.IsNullOrEmpty(registryRoot) && (null != logger))
					{
						logger.BuildVerbosityRegistryRoot = registryRoot;
						logger.ErrorString = this.ErrorString;
						logger.WarningString = this.WarningString;
					}
				}
			}
			else
			{
				((NemerleIdeBuildLogger)this.BuildLogger).OutputWindowPane = output;
			}

			if (BuildEngine != null)
			{
				BuildEngine.UnregisterAllLoggers();
				BuildEngine.RegisterLogger(BuildLogger);
			}
		}

		bool _suppressDispose;
		protected override void Dispose(bool disposing)
		{
			if (!_suppressDispose)
			{
				if (_projectInfo != null)
				{
					_projectInfo.Close();
					_projectInfo = null;
				}
				base.Dispose(disposing);
			}
		}

		public override int Close()
		{
			if (Site != null)
			{
				INemerleLibraryManager libraryManager =
					Site.GetService(typeof(INemerleLibraryManager)) as INemerleLibraryManager;

				if (null != libraryManager)
					libraryManager.UnregisterHierarchy(InteropSafeHierarchy);
			}

			int result;

			// Prevent early disposing...
			_suppressDispose = true;

			try { result = base.Close(); }
			catch (COMException ex) { result = ex.ErrorCode; }
			finally
			{
				_suppressDispose = false;
				Dispose(true);
			}

			return result;
		}

	  public override int OpenItem(uint itemId, ref Guid logicalView, IntPtr punkDocDataExisting, out IVsWindowFrame frame)
	  {
      // Init output params
      frame = null;

      HierarchyNode n = this.NodeFromItemId(itemId);
      if (n == null)
      {
        throw new ArgumentException(Microsoft.VisualStudio.Project.SR.GetString(Microsoft.VisualStudio.Project.SR.ParameterMustBeAValidItemId, CultureInfo.CurrentUICulture), "itemId");
      }

      // Delegate to the document manager object that knows how to open the item
      DocumentManager documentManager = n.GetDocumentManager();
      if (documentManager != null)
      {
        ////HACK: VladD2: If view opened by double click on the Output window, VS try open 
        //// LOGVIEWID_TextView instead of LOGVIEWID_Code. It result in open double view for 
        //// single file. Substituting it by LOGVIEWID_Code...
        //Guid newLogicalView = logicalView == VSConstants.LOGVIEWID_TextView && n is NemerleFileNode
        //  ? VSConstants.LOGVIEWID_Code : logicalView;
        return documentManager.Open(ref logicalView, punkDocDataExisting, out frame, WindowFrameShowAction.DoNotShow);
      }

      // This node does not have an associated document manager and we must fail
      return VSConstants.E_FAIL;
    }

    public override void Load(
			string filename,
			string location,
			string name,
			uint flags,
			ref Guid iidProject,
			out int canceled)
		{
			Debug.Assert(BuildEngine != null);
			Debug.Assert(BuildProject != null);
			Debug.Assert(BuildProject.FullPath == Path.GetFullPath(filename));

			// IT: ProjectInfo needs to be created before loading
			// as we will catch assembly reference adding.
			//
			var langService = Utils.GetService<NemerleLanguageService>(Site);

			Debug.Assert(langService != null);

			_projectInfo = new ProjectInfo(this, InteropSafeHierarchy, langService, filename, location);

			ProjectInfo.Projects.Add(_projectInfo);

			base.Load(filename, location, name, flags, ref iidProject, out canceled);

			// WAP ask the designer service for the CodeDomProvider corresponding to the project node.
			//

			// AKhropov: now NemerleFileNode provides these services
			//OleServiceProvider.AddService(typeof(SVSMDCodeDomProvider), this.CodeDomProvider, false);
			//OleServiceProvider.AddService(typeof(CodeDomProvider), CodeDomProvider.CodeDomProvider, false);

			// IT: Initialization sequence is important.
			//
			ProjectInfo.InitListener();

			INemerleLibraryManager libraryManager = Utils.GetService<INemerleLibraryManager>(Site);

			if (libraryManager != null)
				libraryManager.RegisterHierarchy(InteropSafeHierarchy);

			// If this is a WPFFlavor-ed project, then add a project-level DesignerContext service to provide
			// event handler generation (EventBindingProvider) for the XAML designer.
			OleServiceProvider.AddService(typeof(DesignerContext), DesignerContext, false);
		}

		public override void PrepareBuild(string config, bool cleanBuild)
		{
			if (HasProjectOpened)
				base.PrepareBuild(config, cleanBuild);
		}

		/// <summary>
		/// Overriding to provide project general property page
		/// </summary>
		/// <returns></returns>
		protected override Guid[] GetConfigurationIndependentPropertyPages()
		{
			return new Guid[] { typeof(NemerleGeneralPropertyPage).GUID };
		}

		/// <summary>
		/// Returns the configuration dependent property pages.
		/// Specify here a property page. By returning no property page the 
		/// configuartion dependent properties will be neglected. Overriding, but 
		/// current implementation does nothing. To provide configuration specific
		/// page project property page, this should return an array bigger then 0
		/// (you can make it do the same as 
		/// GetConfigurationIndependentPropertyPages() to see its 
		/// impact)
		/// </summary>
		/// <returns></returns>
		protected override Guid[] GetConfigurationDependentPropertyPages()
		{
			return new Guid[]
			{
				typeof(NemerleBuildPropertyPage).GUID,
				typeof(NemerleDebugPropertyPage).GUID,
			};
		}

		/// <summary>
		/// Overriding to provide customization of files on add files.
		/// This will replace tokens in the file with actual value (namespace, 
		/// class name,...)
		/// </summary>
		/// <param name="source">Full path to template file</param>
		/// <param name="target">Full path to destination file</param>
		public override void AddFileFromTemplate(string source, string target)
		{
			if (!File.Exists(source))
				throw new FileNotFoundException(
					String.Format("Template file not found: {0}", source));

			// We assume that there is no token inside the file because the only
			// way to add a new element should be through the template wizard that
			// take care of expanding and replacing the tokens.
			// The only task to perform is to copy the source file in the
			// target location.
			string targetFolder = Path.GetDirectoryName(target);
			if (!Directory.Exists(targetFolder))
			{
				Directory.CreateDirectory(targetFolder);
			}

			File.Copy(source, target);
		}

		// KLiss: body of this method is copy/pasted from base one (and modified),
		// as base implementation does not allow changing parent node on-the-fly.
		protected override void AddNewFileNodeToHierarchy(HierarchyNode parentNode, string fileName)
		{
			HierarchyNode child;
			HierarchyNode newParent;

			// KLiss: try to find possible parent file (ie, Form3.n would be parent for Form3.designer.n
			bool parentFound = TryFindParentFileNode(parentNode, fileName, out newParent);
			if (parentFound)
			{
				parentNode = newParent;

				// KLiss: when file is added to project, it is treated as code file,
				// regardless of SubType value, specified in the template.
				// SubType is assigned correct value later, and now we will make another 
				// attempt to find out, whether it is OK for an item to have designer, or not.
				var nemerleParent = parentNode as NemerleFileNode;
				if (nemerleParent != null)
				{
					nemerleParent.InferHasDesignerFromSubType();
				}
			}

			// In the case of subitem, we want to create dependent file node
			// and set the DependentUpon property
			if (parentFound || parentNode is FileNode || parentNode is DependentFileNode)
			{
				child = this.CreateDependentFileNode(fileName);

				child.ItemNode.SetMetadata(ProjectFileConstants.DependentUpon, parentNode.ItemNode.GetMetadata(ProjectFileConstants.Include));

				// Make sure to set the HasNameRelation flag on the dependent node if it is related to the parent by name
				if (!child.HasParentNodeNameRelation && string.Compare(child.GetRelationalName(), parentNode.GetRelationalName(), StringComparison.OrdinalIgnoreCase) == 0)
				{
					child.HasParentNodeNameRelation = true;
				}
			}
			else
			{
				//Create and add new filenode to the project
				child = this.CreateFileNode(fileName);
			}

			parentNode.AddChild(child);

			var projectInfo = ProjectInfo;

			if (projectInfo != null)
				projectInfo.Engine.RequestOnBuildTypesTree();

			//// TODO : Revisit the VSADDFILEFLAGS here. Can it be a nested project?
			this.Tracker.OnItemAdded(fileName, VSADDFILEFLAGS.VSADDFILEFLAGS_NoFlags);
		}

				private bool TryFindParentFileNode(HierarchyNode root, string child, out HierarchyNode parent)
		{
			parent = null;

			var childName = Path.GetFileName(child);
			var childDirectory = Path.GetDirectoryName(child);

			// the standart layout, used for most file types (aspx, ashx, master, xaml etc)
			// + - page.aspx
			// |   + - page.aspx.n
			// |   + - page.aspx.designer.n
			var parentName = Path.GetFileNameWithoutExtension(childName);
			while (parentName.IndexOf('.') > 0 && parent == null)
			{
				parent = root.FindChild(Path.Combine(childDirectory, parentName));
				parentName = Path.GetFileNameWithoutExtension(parentName);
			}

			if (parent == null)
			{
				// Windows forms layout:
				// + - form.n
				// |   + - form.designer.n

				var childNameWithoutExtension = Path.GetFileNameWithoutExtension(child);

				// look for suffix position (".Designer", etc)
				var suffixIndex = childNameWithoutExtension.LastIndexOf(root.NameRelationSeparator);
				if (suffixIndex < 0)
					return false;

				parentName = string.Format("{0}.n", childNameWithoutExtension.Substring(0, suffixIndex));

				var parentPath = Path.Combine(childDirectory, parentName);

				parent = root.FindChild(parentPath);
			}

			return parent != null;
		}

		/// <summary>
		/// Evaluates if a file is an Nemerle code file based on is extension
		/// </summary>
		/// <param name="strFileName">The filename to be evaluated</param>
		/// <returns>true if is a code file</returns>
		public override bool IsCodeFile(string strFileName)
		{
			// We do not want to assert here, just return silently.
			//
			if (string.IsNullOrEmpty(strFileName))
				return false;

			var extension = Path.GetExtension(strFileName);
			var isNemerleFile = string.Compare(extension, NemerleConstants.FileExtension, StringComparison.OrdinalIgnoreCase) == 0;
			return isNemerleFile || (ProjectInfo != null && ProjectInfo.Engine.IsExtensionRegistered(extension));
		}


		public override bool IsEmbeddedResource(string strFileName)
		{
			return base.IsEmbeddedResource(strFileName) ||
				StringComparer.OrdinalIgnoreCase.Compare(Path.GetExtension(strFileName), ".licx") == 0;
		}

		/// <summary>
		/// Create a file node based on an msbuild item.
		/// </summary>
		/// <param name="item">The msbuild item to be analyzed</param>
		/// <returns>NemerleFileNode or FileNode</returns>
		public override FileNode CreateFileNode(ProjectElement item)
		{
			ErrorHelper.ThrowIsNull(item, "item");

			NemerleFileNode newNode = new NemerleFileNode(this, item, item.IsVirtual);

			string include = item.GetMetadata(ProjectFileConstants.Include);

			var provider = newNode.OleServiceProvider;

			provider.AddService(typeof(EnvDTE.Project), ProjectMgr.GetAutomationObject(), false);
			provider.AddService(typeof(EnvDTE.ProjectItem), newNode.ServiceCreator, false);
			provider.AddService(typeof(VSLangProj.VSProject), this.VSProject, false);

			if (!string.IsNullOrEmpty(include) && Path.GetExtension(include).Equals(".xaml", StringComparison.OrdinalIgnoreCase))
			{
				//Create a DesignerContext for the XAML designer for this file
				newNode.OleServiceProvider.AddService(typeof(DesignerContext), newNode.ServiceCreator, false);
			}

			if (newNode.IsFormSubType)
			{
				newNode.OleServiceProvider.AddService(typeof(DesignerContext), newNode.ServiceCreator, false);
			}

			if (item.ItemName == "Compile") // IsCodeFile(include) && 
				provider.AddService(typeof(SVSMDCodeDomProvider), new NemerleVSMDProvider(newNode), false);

			return newNode;
		}

		/// <summary>
		/// Create dependent file node based on an msbuild item
		/// </summary>
		/// <param name="item">msbuild item</param>
		/// <returns>dependent file node</returns>
		public override FileNode CreateDependentFileNode(ProjectElement item)
		{
			ErrorHelper.ThrowIsNull(item, "item");

			NemerleDependentFileNode newNode = new NemerleDependentFileNode(this, item);
			string include = item.GetMetadata(ProjectFileConstants.Include);

			var provider = newNode.OleServiceProvider;

			provider.AddService(typeof(EnvDTE.Project), ProjectMgr.GetAutomationObject(), false);
			provider.AddService(typeof(EnvDTE.ProjectItem), newNode.GetAutomationObject(), false);
			provider.AddService(typeof(VSLangProj.VSProject), this.VSProject, false);

			if (IsCodeFile(include) && item.ItemName == "Compile")
				newNode.OleServiceProvider.AddService(typeof(SVSMDCodeDomProvider),
					new NemerleVSMDProvider(newNode), false);

			return newNode;
		}

		/// <summary>
		/// Creates the format list for the open file dialog
		/// </summary>
		/// <param name="formatlist">The formatlist to return</param>
		/// <returns>Success</returns>
		public override int GetFormatList(out string formatlist)
		{
			formatlist = string.Format(
				CultureInfo.CurrentCulture, SR.GetString(SR.ProjectFileExtensionFilter), "\0", "\0");

			return VSConstants.S_OK;
		}

		/// <summary>
		/// This overrides the base class method to show the VS 2005 style Add 
		/// reference dialog. The ProjectNode implementation shows the VS 2003 
		/// style Add Reference dialog.
		/// </summary>
		/// <returns>S_OK if succeeded. Failure other wise</returns>
		public override int AddProjectReference()
		{
			IVsComponentSelectorDlg2 componentDialog;
			Guid startOnTab = Guid.Empty;
			VSCOMPONENTSELECTORTABINIT[] tabInit = new VSCOMPONENTSELECTORTABINIT[5];
			string browseLocations = Path.GetDirectoryName(BaseURI.Uri.LocalPath);
			Guid GUID_MruPage = new Guid("{19B97F03-9594-4c1c-BE28-25FF030113B3}");

			// Add the .NET page.
			//
			tabInit[0].dwSize = (uint)Marshal.SizeOf(typeof(VSCOMPONENTSELECTORTABINIT));
			tabInit[0].varTabInitInfo = 0;
			tabInit[0].guidTab = VSConstants.GUID_COMPlusPage;

			// Add the COM page.
			//
			tabInit[1].dwSize = (uint)Marshal.SizeOf(typeof(VSCOMPONENTSELECTORTABINIT));
			tabInit[1].varTabInitInfo = 0;
			tabInit[1].guidTab = VSConstants.GUID_COMClassicPage;

			// Add the Project page.
			//
			tabInit[2].dwSize = (uint)Marshal.SizeOf(typeof(VSCOMPONENTSELECTORTABINIT));
			// Tell the Add Reference dialog to call hierarchies GetProperty with 
			// the following propID to enable filtering out ourself from the Project
			// to Project reference
			tabInit[2].varTabInitInfo = (int)__VSHPROPID.VSHPROPID_ShowProjInSolutionPage;
			tabInit[2].guidTab = VSConstants.GUID_SolutionPage;

			// Add the Browse page.
			//
			tabInit[3].dwSize = (uint)Marshal.SizeOf(typeof(VSCOMPONENTSELECTORTABINIT));
			tabInit[3].varTabInitInfo = 0;
			tabInit[3].guidTab = VSConstants.GUID_BrowseFilePage;

			// Add the Recent page.
			//
			tabInit[4].dwSize = (uint)Marshal.SizeOf(typeof(VSCOMPONENTSELECTORTABINIT));
			tabInit[4].varTabInitInfo = 0;
			tabInit[4].guidTab = GUID_MruPage;

			uint pX = 0, pY = 0;

			startOnTab = tabInit[2].guidTab;

			componentDialog = GetService(typeof(SVsComponentSelectorDlg)) as IVsComponentSelectorDlg2;

			try
			{
				// Call the container to open the add reference dialog.
				//
				if (componentDialog != null)
				{
					// Let the project know not to show itself in the Add Project Reference Dialog page
					//
					ShowProjectInSolutionPage = false;

					// Call the container to open the add reference dialog.
					//
					ErrorHandler.ThrowOnFailure(
						componentDialog.ComponentSelectorDlg2(
							(UInt32)
								(__VSCOMPSELFLAGS.VSCOMSEL_MultiSelectMode |
								 __VSCOMPSELFLAGS.VSCOMSEL_IgnoreMachineName),
							this,
							0,
							null,
						// Title
							Microsoft.VisualStudio.Project.SR.GetString(
								Microsoft.VisualStudio.Project.SR.AddReferenceDialogTitle),
							"VS.AddReference", // Help topic
							ref pX,
							ref pY,
							(uint)tabInit.Length,
							tabInit,
							ref startOnTab,
							"*.dll",
							ref browseLocations));
				}
			}
			catch (COMException e)
			{
				Trace.WriteLine("Exception : " + e.Message);
				return e.ErrorCode;
			}
			finally
			{
				// Let the project know it can show itself in the Add Project Reference Dialog page
				//
				ShowProjectInSolutionPage = true;
			}

			return VSConstants.S_OK;
		}

		#region IVsComponentUser methods

		public IReferenceContainer GetMacroReferenceContainer()
		{
			return (IReferenceContainer)this.FindChild(NemerleMacroReferenceContainerNode.MacroReferencesNodeVirtualName);
		}

		/// <summary>
		/// Add Components to the Project.
		/// Used by the environment to add components specified by the user in the Component Selector dialog 
		/// to the specified project
		/// </summary>
		/// <param name="dwAddCompOperation">The component operation to be performed.</param>
		/// <param name="cComponents">Number of components to be added</param>
		/// <param name="rgpcsdComponents">array of component selector data</param>
		/// <param name="hwndDialog">Handle to the component picker dialog</param>
		/// <param name="pResult">Result to be returned to the caller</param>
		public override int AddComponent(VSADDCOMPOPERATION dwAddCompOperation, uint cComponents, System.IntPtr[] rgpcsdComponents, System.IntPtr hwndDialog, VSADDCOMPRESULT[] pResult)
		{
			//initalize the out parameter
			pResult[0] = VSADDCOMPRESULT.ADDCOMPRESULT_Success;

			var selectedNodes = GetSelectedNodes();

			IReferenceContainer references = selectedNodes.Count == 1 && selectedNodes[0] is NemerleMacroReferenceContainerNode
																				? GetMacroReferenceContainer()
																				: GetReferenceContainer();

			if (null == references)
			{
				// This project does not support references or the reference container was not created.
				// In both cases this operation is not supported.
				return VSConstants.E_NOTIMPL;
			}
			for (int cCount = 0; cCount < cComponents; cCount++)
			{
				VSCOMPONENTSELECTORDATA selectorData = new VSCOMPONENTSELECTORDATA();
				IntPtr ptr = rgpcsdComponents[cCount];
				selectorData = (VSCOMPONENTSELECTORDATA)Marshal.PtrToStructure(ptr, typeof(VSCOMPONENTSELECTORDATA));
				if (null == references.AddReferenceFromSelectorData(selectorData))
				{
					//Skip further proccessing since a reference has to be added
					pResult[0] = VSADDCOMPRESULT.ADDCOMPRESULT_Failure;
					return VSConstants.S_OK;
				}
			}
			return VSConstants.S_OK;
		}

		#endregion


		protected override ConfigProvider CreateConfigProvider()
		{
			return new NemerleConfigProvider(this);
		}

		protected override NodeProperties CreatePropertiesObject()
		{
			return new NemerleProjectNodeProperties(this);
		}

		public override int AddItem(uint itemIdLoc, VSADDITEMOPERATION op, string itemName, uint filesToOpen, string[] files, IntPtr dlgOwner, VSADDRESULT[] result)
		{
			return AddManyItemsHelper(itemIdLoc, op, itemName, filesToOpen, files, dlgOwner, result);
		}

		/// <summary>
		/// Allows you to query the project for special files and optionally create them. 
		/// </summary>
		/// <param name="fileId">__PSFFILEID of the file</param>
		/// <param name="flags">__PSFFLAGS flags for the file</param>
		/// <param name="itemid">The itemid of the node in the hierarchy</param>
		/// <param name="fileName">The file name of the special file.</param>
		/// <returns></returns>
		public override int GetFile(int fileId, uint flags, out uint itemid, out string fileName)
		{
			const string propertiesFolderPath = "Properties\\";
			HierarchyNode propertiesNode = FindChildEx(this, "Properties");

			if (propertiesNode == null)
				propertiesNode = CreateFolderNode("Properties");

			HierarchyNode parent = this;

			switch (fileId)
			{
				case (int)__PSFFILEID.PSFFILEID_AppConfig:
					fileName = "app.config";
					break;
				case (int)__PSFFILEID.PSFFILEID_Licenses:
					fileName = "licenses.licx";
					break;

				case (int)__PSFFILEID2.PSFFILEID_WebSettings:
					fileName = "web.config";
					break;
				case (int)__PSFFILEID2.PSFFILEID_AppManifest:
					fileName = "app.manifest";
					break;
				case (int)__PSFFILEID2.PSFFILEID_AppSettings:
					fileName = propertiesFolderPath + "Settings.settings";
					break;
				case (int)__PSFFILEID2.PSFFILEID_AssemblyResource:
					fileName = propertiesFolderPath + "Resources.resx";
					break;
				case (int)__PSFFILEID2.PSFFILEID_AssemblyInfo:
					fileName = propertiesFolderPath + "AssemblyInfo.cs";
					break;
				default:
					return base.GetFile(fileId, flags, out itemid, out fileName);
			}

			if (fileName.StartsWith(propertiesFolderPath, StringComparison.InvariantCulture))
				parent = propertiesNode;

			HierarchyNode fileNode = FindChildEx(parent, Path.GetFileName(fileName));
			string fullPath = fileNode == null
				? Path.Combine(ProjectFolder, fileName)
				: fileNode.Url;

			if (fileNode == null && (flags & (uint)__PSFFLAGS.PSFF_CreateIfNotExist) != 0)
			{
				// Create a zero-length file if not exist already.
				//
				if (!File.Exists(fullPath))
					File.WriteAllText(fullPath, string.Empty);

				fileNode = CreateFileNode(fileName);
				parent.AddChild(fileNode);
			}

			itemid = fileNode != null ? fileNode.ID : 0;

			if ((flags & (uint)__PSFFLAGS.PSFF_FullPath) != 0)
				fileName = fullPath;

			return VSConstants.S_OK;
		}

		internal static HierarchyNode FindChildEx(HierarchyNode it, string name)
		{
			if (String.IsNullOrEmpty(name))
				return null;

			HierarchyNode result;
			for (HierarchyNode child = it.FirstChild; child != null; child = child.NextSibling)
			{
				if (!String.IsNullOrEmpty(child.VirtualNodeName) && String.Compare(child.VirtualNodeName, name, StringComparison.OrdinalIgnoreCase) == 0)
					return child;

				if (String.Equals(child.Caption, name, StringComparison.OrdinalIgnoreCase))
					return child;
				
				// If it is a foldernode then it has a virtual name but we want to find folder nodes by the document moniker or url
				else if ((String.IsNullOrEmpty(child.VirtualNodeName) || (child is FolderNode)) &&
						(NativeMethods.IsSamePath(child.GetMkDocument(), name) || NativeMethods.IsSamePath(child.Url, name)))
				{
					return child;
				}

				result = FindChildEx(child, name);
				if (result != null)
				{
					return result;
				}
			}
			return null;
		}


		protected override void OnHandleConfigurationRelatedGlobalProperties(object sender, ActiveConfigurationChangedEventArgs eventArgs)
		{
			base.OnHandleConfigurationRelatedGlobalProperties(sender, eventArgs);

			_projectInfo.Engine.RequestOnReloadProject();
		}

		public override int GetGuidProperty(int propid, out Guid guid)
		{
			if ((__VSHPROPID)propid == __VSHPROPID.VSHPROPID_PreferredLanguageSID)
			{
				guid = typeof(NemerleLanguageService).GUID;
			}
			else
			{
				return base.GetGuidProperty(propid, out guid);
			}
			return VSConstants.S_OK;
		}

		protected override bool IsItemTypeFileType(string type)
		{
			if (base.IsItemTypeFileType(type))
				return true;

			return
				String.Compare(type, "Page", StringComparison.OrdinalIgnoreCase) == 0 ||
				String.Compare(type, "ApplicationDefinition", StringComparison.OrdinalIgnoreCase) == 0 ||
				String.Compare(type, "Resource", StringComparison.OrdinalIgnoreCase) == 0;
		}

		/// <summary>
		/// Enables / Disables the ShowAllFileMode.
		/// </summary>
		/// <returns>S_OK if it's possible to toggle the state, OLECMDERR_E_NOTSUPPORTED if not</returns>
		protected internal override int ShowAllFiles()
		{
			return ToggleShowAllFiles();
		}

		protected override void AddWizardCustomParams(HierarchyNode parent, string file, Dictionary<string, string> customParams)
		{
			customParams["itemnamespace"] = GetNamespace(parent);
		}

			#endregion

		#region IVsProjectSpecificEditorMap2 Members

		public int GetSpecificEditorProperty(string mkDocument, int propid, out object result)
		{
			// Initialize output params.
			//
			result = null;

			// Validate input.
			//
			if (string.IsNullOrEmpty(mkDocument))
				throw new ArgumentException("Was null or empty", "mkDocument");

			// Make sure that the document moniker passed to us is part of this project.
			// We also don't care if it is not a nemerle file node.
			//
			uint itemid;

			ErrorHandler.ThrowOnFailure(ParseCanonicalName(mkDocument, out itemid));

			HierarchyNode hierNode = NodeFromItemId(itemid);

			if (hierNode == null || ((hierNode as NemerleFileNode) == null))
				return VSConstants.E_NOTIMPL;

			switch (propid)
			{
				case (int)__VSPSEPROPID.VSPSEPROPID_UseGlobalEditorByDefault:
					// We do not want to use global editor for form files.
					//
					result = true;
					break;

				case (int)__VSPSEPROPID.VSPSEPROPID_ProjectDefaultEditorName:
					result = "Nemerle Form Editor";
					break;
			}

			return VSConstants.S_OK;
		}

		public int GetSpecificEditorType(string mkDocument, out Guid guidEditorType)
		{
			// Ideally we should at this point initalize a File extension to 
			// EditorFactory guid Map e.g. in the registry hive so that more 
			// editors can be added without changing this part of the code. 
			// Nemerle only makes usage of one Editor Factory and therefore 
			// we will return that guid.
			//
			guidEditorType = NemerleEditorFactory.EditorFactoryGuid;
			return VSConstants.S_OK;
		}

		int IVsProjectSpecificEditorMap2.GetSpecificLanguageService(string mkDocument, out Guid guidLanguageService)
		{
			guidLanguageService = Guid.Empty;
			return VSConstants.E_NOTIMPL;

			// AKhropov: looks like it's more appropriate
			// IT: No, it is not. It makes the integration not working.
			//guidLanguageService = Utils.GetService<NemerleLanguageService>(Site).GetLanguageServiceGuid();
			//return VSConstants.S_OK;
		}

		public int SetSpecificEditorProperty(string mkDocument, int propid, object value)
		{
			return VSConstants.E_NOTIMPL;
		}

		#endregion

		#region Static Methods

		public static string GetOutputExtension(OutputType outputType)
		{
			switch (outputType)
			{
				case OutputType.Library: return ".dll";
				case OutputType.Exe:
				case OutputType.WinExe: return ".exe";
			}

			throw new InvalidOperationException();
		}

		#endregion

		#region Helper methods

		private int AddManyItemsHelper(uint itemIdLoc, VSADDITEMOPERATION op, string itemName, uint filesToOpen, string[] files, IntPtr dlgOwner, VSADDRESULT[] result)
		{
			List<string> actualFiles = new List<string>(files.Length);
			List<string> dirs = new List<string>();
			foreach (string file in files)
			{
				if (File.Exists(file))
					actualFiles.Add(file);
				else
				{
					if (Directory.Exists(file))
					{
						dirs.Add(file);
					}
				}
			}

			if (actualFiles.Count > 0)
				ErrorHandler.ThrowOnFailure(base.AddItem(itemIdLoc, op, itemName, (uint)actualFiles.Count, actualFiles.ToArray(), dlgOwner, result));

			foreach (string directory in dirs)
			{
				HierarchyNode folderNode = CreateFolderNodeHelper(directory, itemIdLoc);
				List<string> directoryEntries = new List<string>();
				directoryEntries.AddRange(Directory.GetFiles(directory));
				directoryEntries.AddRange(Directory.GetDirectories(directory));
				AddManyItemsHelper(folderNode.ID, op, null, (uint)directoryEntries.Count, directoryEntries.ToArray(), dlgOwner, result);
			}

			return VSConstants.S_OK;
		}

		private HierarchyNode CreateFolderNodeHelper(string directory, uint parentID)
		{
			if (!Path.GetFullPath(directory).Contains(this.ProjectFolder))
			{
				HierarchyNode parent = this.NodeFromItemId(parentID);
				if (parent is FolderNode)
					return CreateFolderNodeHelper(directory, parent.Url);

				if (parent is ProjectNode)
					return CreateFolderNodeHelper(directory, Path.GetDirectoryName(parent.Url));
			}

			return base.CreateFolderNodes(directory);
		}

		private HierarchyNode CreateFolderNodeHelper(string sourcePath, string parentPath)
		{
			string newFolderUrl = Path.Combine(parentPath, Path.GetFileName(sourcePath));
			Directory.CreateDirectory(newFolderUrl);
			return base.CreateFolderNodes(newFolderUrl);
		}

		private string GetNamespace(HierarchyNode node)
		{
			var namespaces = new List<string>();
			for (var currentNode = node; currentNode != this; currentNode = currentNode.Parent)
				namespaces.Add(currentNode.NodeProperties.Name);
			var rootNamespace = GetProjectProperty(ProjectFileConstants.RootNamespace, false);
			if (!string.IsNullOrEmpty(rootNamespace))
				namespaces.Add(rootNamespace);
			namespaces.Reverse();
			return string.Join(".", namespaces);
		}

			#endregion

		#region Methods

		//TODO: VladD2: Реализовать код-дом-провайдер для проекта.

		//private Microsoft.VisualStudio.Designer.Interfaces.IVSMDCodeDomProvider _codeDomProvider;
		//
		///// <summary>
		///// Retreive the CodeDOM provider
		///// </summary>
		//protected internal Microsoft.VisualStudio.Designer.Interfaces.IVSMDCodeDomProvider CodeDomProvider
		//{
		//  get
		//  {
		//    if (_codeDomProvider == null)
		//      _codeDomProvider = new VSMDPythonProvider(this.VSProject);
		//    return _codeDomProvider;
		//  }
		//}

		internal OleServiceProvider.ServiceCreatorCallback ServiceCreator
		{
			get { return new OleServiceProvider.ServiceCreatorCallback(this.CreateServices); }
		}

		/// <summary>
		/// Creates the services exposed by this project.
		/// </summary>
		private object CreateServices(Type serviceType)
		{
			object service = null;
			//if (typeof(SVSMDCodeDomProvider) == serviceType)
			//  service = this.CodeDomProvider;
			//else if (typeof(System.CodeDom.Compiler.CodeDomProvider) == serviceType)
			//  service = this.CodeDomProvider.CodeDomProvider;
			//else 
			if (typeof(DesignerContext) == serviceType)
				service = this.DesignerContext;
			else if (typeof(VSLangProj.VSProject) == serviceType)
				service = this.VSProject;
			else if (typeof(EnvDTE.Project) == serviceType)
				service = this.GetAutomationObject();

			return service;
		}

		#endregion
	}
}
