using System;
using System.ComponentModel.Design;
using System.Diagnostics;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Project;
using System.Linq;

using Nemerle.VisualStudio.LanguageService;
using Nemerle.VisualStudio.Project;

using Nemerle.VisualStudio.GUI;
using Nemerle.VisualStudio.Properties;
using Nemerle.VisualStudio.Project.PropertyPages;
using Nemerle.VisualStudio.WPFProviders;
using Nemerle.VisualStudio.GUI.SourceOutliner;
using Microsoft.VisualStudio.TextManager.Interop;
using System.Collections.Generic;
using System.Windows.Forms;
using Nemerle.Compiler;
using System.IO;
using Nemerle.VisualStudio.RegistrationAttributes;

namespace Nemerle.VisualStudio
{
	#region Registration Attributes

	[RegistrationAttributes.SingleFileGeneratorSupportRegistrationAttribute(typeof(NemerleProjectFactory))]

	[Guid(NemerleConstants.PackageGuidString)]
	[ComVisible(true)]
	[PackageRegistration(UseManagedResourcesOnly = true)]
	[ProvideLoadKey(
		NemerleConstants.PLKMinEdition,
		NemerleConstants.PLKProductVersion,
		NemerleConstants.PLKProductName,
		NemerleConstants.PLKCompanyName,
		NemerleConstants.PLKResourceId)]

	[DefaultRegistryRoot(NemerleConstants.VisualStudioRegistryRoot)]
	[ProvideService(typeof(NemerleLanguageService), ServiceName = NemerleConstants.LanguageName)]
	[ProvideService(typeof(INemerleLibraryManager))]
	[ProvideLanguageService(
		typeof(NemerleLanguageService),
		NemerleConstants.LanguageName,
		100,
		DefaultToInsertSpaces = true,
		AutoOutlining         = true,
		CodeSense             = true,
		EnableCommenting      = true,
		MatchBraces           = true,
		ShowCompletion        = true,
		ShowMatchingBrace     = true,
		ShowDropDownOptions   = true,
		EnableFormatSelection = true,
		ShowSmartIndent       = true
		)]
	[ProvideLanguageExtension(typeof(NemerleLanguageService), NemerleConstants.FileExtension)]
	[ProvideProjectFactory(typeof(NemerleProjectFactory),
		NemerleConstants.LanguageName,
		NemerleConstants.LanguageName + " Project Files (*." +
			NemerleConstants.ProjectExtension + ");*." + NemerleConstants.ProjectExtension,
		NemerleConstants.ProjectExtension,
		NemerleConstants.ProjectExtension,
		// Set the projectsTemplatesDirectory to a non-existant path to prevent VS 
		// from including the working directory as a valid template path
		@".\NullPath",
		LanguageVsTemplate = NemerleConstants.LanguageName)]
	[ProvideCodeDomProvider(typeof(NemerleProjectFactory), typeof(Nemerle.Compiler.Utils.NemerleCodeDomProvider))]
	[ProvideProjectFactory(typeof(NemerleWPFProjectFactory),
		null,
		null,
		null,
		null,
		null,
		LanguageVsTemplate = NemerleConstants.LanguageName,
		TemplateGroupIDsVsTemplate = "WPF",
		ShowOnlySpecifiedTemplatesVsTemplate = false)]
	// ContainedLanguage support.
	//
	[Nemerle.VisualStudio.RegistrationAttributes.ProvideIntellisenseProvider(typeof(NemerleIntellisenseProvider),
		NemerleConstants.LanguageId + "CodeProvider",
		NemerleConstants.LanguageName,
		NemerleConstants.FileExtension, NemerleConstants.LanguageId, NemerleConstants.LanguageId)]
	[ProvideObject(typeof(NemerleIntellisenseProvider))]

	[ProvideMenuResource(1000, 1)]
	[ProvideObject           (typeof(NemerleGeneralPropertyPage))]
	[ProvideObject           (typeof(NemerleDebugPropertyPage))]
	[ProvideObject           (typeof(NemerleBuildPropertyPage))]
	[ProvideObject           (typeof(NemerleWACodeBehindEventBinding))]
	[ProvideEditorExtension(typeof(NemerleEditorFactory), NemerleConstants.FileExtension, 32)]
	
	// Attention! These guids are magic numbers provided by Microsoft. Don't change them.
	//
	[ProvideEditorLogicalView(typeof(NemerleEditorFactory), "{7651a702-06e5-11d1-8ebd-00a0c90f26ea}")]  //LOGVIEWID_Designer
	[ProvideEditorLogicalView(typeof(NemerleEditorFactory), "{7651a701-06e5-11d1-8ebd-00a0c90f26ea}")]  //LOGVIEWID_Code

	// Showing the splash screen requires "devenv /rootsuffix Exp /setup" during the installation.
	// For more information please see: http://blogs.msdn.com/jim_glass/archive/2005/05/23/421152.aspx
	[InstalledProductRegistration(
		true, 
		NemerleConstants.LanguageName, 
		NemerleConstants.ProductDetails, 
		NemerleConstants.PLKProductVersion, 
		IconResourceID=300)]
	[RegistrationAttributes.RegisterSnippets(
		NemerleConstants.LanguageServiceGuidString,
		false,
		100,
		NemerleConstants.LanguageName,
		@"CodeSnippets\SnippetsIndex.xml",
		@"CodeSnippets\Snippets\",
		@"CodeSnippets\Snippets\")]
	//phantom: try to play with it only after menu will be fixed (handlers leashing)
	//[ProvideKeyBindingTable(NemerleConstants.EditorFactoryGuidString, 102)]

	// This attribute registers a tool window exposed by this package.
	[ProvideToolWindow(typeof(AstToolWindow))]
	[ProvideToolWindow(typeof(SourceOutlinerToolWindow))]

	//The following attributes are specific to supporting Web Application Projects
	//
	[WebSiteProject(NemerleConstants.LanguageId , NemerleConstants.LanguageName)]
	[WebSiteProjectRelatedFiles("aspx",   NemerleConstants.FileExtensionOnly)]
	[WebSiteProjectRelatedFiles("master", NemerleConstants.FileExtensionOnly)]
	[WAProvideProjectFactory(typeof(WANemerleProjectFactory), NemerleConstants.LanguageName + " Web Application Project Templates", NemerleConstants.LanguageId, false, "Web", null)]
	[WAProvideProjectFactoryTemplateMapping("{" + NemerleConstants.ProjectFactoryGuidString + "}", typeof(WANemerleProjectFactory))]
	[WAProvideLanguageProperty(typeof(WANemerleProjectFactory), "CodeFileExtension",       NemerleConstants.FileExtension)]
	[WAProvideLanguageProperty(typeof(WANemerleProjectFactory), "TemplateFolder",          NemerleConstants.LanguageId)]
	[WAProvideLanguageProperty(typeof(WANemerleProjectFactory), "CodeBehindCodeGenerator", NemerleConstants.WACodeBehindCodeGeneratorGuidString)]
	[WAProvideLanguageProperty(typeof(WANemerleProjectFactory), "CodeBehindEventBinding", typeof(NemerleWACodeBehindEventBinding))]

	#endregion

	public class NemerlePackage : ProjectPackage, IOleComponent, IVsInstalledProduct
	{
		#region Fields

		private uint                  _componentID;
		private NemerleLibraryManager _libraryManager;
		private SourceOutlinerToolWindow _sourceOutlinerToolWindow;
		public OleMenuCommand RefactoringMenu { get; private set; }
		public OleMenuCommand SelectionExtend { get; private set; }
		public OleMenuCommand SelectionShrink { get; private set; }

		#endregion

		#region Initialize

		public NemerlePackage()
		{
			//Settings.Default.Reload();
			IServiceContainer container = this;

			container.AddService(typeof(NemerleLanguageService), CreateService, true);
			container.AddService(typeof(INemerleLibraryManager), CreateService, true);
		}

    class MySolutionListener : SolutionListener
    {
      public MySolutionListener(System.IServiceProvider serviceProvider) : base(serviceProvider) { }

      public override int OnAfterOpenSolution(object pUnkReserved, int fNewSolution)
      {
        EnvDTE.DTE dte = (EnvDTE.DTE)ServiceProvider.GetService(typeof(EnvDTE.DTE));

        // Из-за того, что при создании NemerleMacroProjectReferenceNode и NemerleMacroAssemblyReferenceNode
        // при открытии проектов dte.Solution.Projects еще не содержал проектов добавление ссылок 
        // на проекты того же солюшена закончились неудачей. Производим повторную попытку добавить 
        // ссылки на проекты в ProjectInfo...

        foreach (EnvDTE.Project prj in dte.Solution.Projects)
        {
          //foreach (EnvDTE.ProjectItem item in prj.ProjectItems)
          //  System.Diagnostics.Trace.WriteLine(item.Name);

          var oaProj = prj as Microsoft.VisualStudio.Project.Automation.OAProject;

          if (oaProj == null)
            continue;

          var nProj = oaProj.Project as NemerleProjectNode;

          if (nProj == null)
            continue;

          var mrc = (NemerleMacroReferenceContainerNode)nProj.GetMacroReferenceContainer();
          var projectInfo = nProj.ProjectInfo;

          var macroProjectRefs = new List<NemerleMacroProjectReferenceNode>();
          mrc.FindNodesOfType(macroProjectRefs);
          foreach (var item in macroProjectRefs)
            projectInfo.AddMacroAssembly(item);

          var rc = (NemerleReferenceContainerNode)nProj.GetReferenceContainer();
          var macroRefs = new List<NemerleProjectReferenceNode>();
          rc.FindNodesOfType(macroRefs);
          foreach (var item in macroRefs)
            projectInfo.AddAssembly(item);

          System.Diagnostics.Trace.WriteLine(nProj.Caption);
        }

        return base.OnAfterOpenSolution(pUnkReserved, fNewSolution);
      }
    }

    MySolutionListener _mySolutionListener;
    
		protected override void Initialize()
		{
			base.Initialize();

      _mySolutionListener = new MySolutionListener(this);
      _mySolutionListener.Init();

      foreach (var listener in Trace.Listeners.OfType<DefaultTraceListener>())
        listener.AssertUiEnabled = false;

      foreach (var listener in Debug.Listeners.OfType<DefaultTraceListener>())
        listener.AssertUiEnabled = false;

		  //Debug.Assert(false);
		  //Trace.Listeners.Clear();
		  //Trace.Listeners.Clear();

			RegisterProjectFactory(new NemerleProjectFactory(this));
			RegisterEditorFactory (new NemerleEditorFactory (this));
			RegisterProjectFactory(new NemerleWPFProjectFactory(this));
			RegisterNemerleCommands();
		}

		private void RegisterNemerleCommands()
		{
			// Add our command handlers for menu (commands must exist in the .vsct file)
			OleMenuCommandService menuService = GetService(typeof(IMenuCommandService)) as OleMenuCommandService;

			//if (menuService != null)
			//{
			//  RegisterCommand(menuService, MenuCmd.AstToolWindow, OnAstToolWindowShow);
			//  RegisterCommand(menuService, MenuCmd.SourceOutlinerWindow, OnSourceOutlinerWindowShow);
			//}
			//else
			//  Trace.WriteLine("Command Service is null!");

			if (menuService != null)
			{
				// Create the command for the menu item.
				RefactoringMenu = RegisterOleMenuCommand(menuService, MenuCmd.CmdId.RefactoringTopMenu, false, null);
				SelectionExtend = RegisterOleMenuCommand(menuService, MenuCmd.CmdId.ExtendSelection, false, null);
				SelectionShrink = RegisterOleMenuCommand(menuService, MenuCmd.CmdId.ShrinkSelection, false, null);
				SelectionExtend.Enabled = false;
				SelectionShrink.Enabled = false;
				RegisterOleMenuCommand(menuService, MenuCmd.CmdId.GoToFile, true, (o, e) => GotoFile());
			}
		}

		private static OleMenuCommand RegisterOleMenuCommand(
			OleMenuCommandService service, 
			MenuCmd.CmdId id, 
			bool visible, 
			EventHandler handler)
		{
			var commandID = new CommandID(MenuCmd.guidNemerleProjectCmdSet, (int)id);
			var command = new OleMenuCommand(handler, commandID);
			command.Visible = visible;
			service.AddCommand(command);
			//Debug.WriteLine(string.Format("Menu command {0} added", command));
			return command;
		}

    //private static void RegisterCommand(OleMenuCommandService service, CommandID commandId, EventHandler handler)
    //{
    //  var command = new MenuCommand(handler, commandId);
    //  service.AddCommand(command);
    //  //Debug.WriteLine(string.Format("Menu command {0} added", command));
    //}

		//public IWin32Window TextEditorWindow
		//{
		//  get { return null; }
		//}

		private void GotoFile()
		{
			using (var frm = new GoToFileForm())
			{
				var dte = GetService(typeof(EnvDTE.DTE)) as EnvDTE.DTE;
				if (dte != null)
				{
					var names = CollectFileNames(dte);

					frm.SetFiles(names);
					if (frm.ShowDialog() == DialogResult.OK)
					{
						var srv = (NemerleLanguageService)GetService(typeof(NemerleLanguageService));
						if (srv != null)
						{
							// illegal location prevent change of caret position
							var loc = new Location(frm.SelectedFileName, 0, 0, 0, 0);
							srv.GotoLocation(loc);
						}
					}
				}
			}
		}

		private IEnumerable<string> CollectFileNames(EnvDTE.DTE dte)
		{
			List<string> result = new List<string>();

			foreach (EnvDTE.Project prj in dte.Solution.Projects)
				result.AddRange(CollectFileNamesRecursively(prj.ProjectItems));

			return result;
		}

		private IEnumerable<string> CollectFileNamesRecursively(EnvDTE.ProjectItems items)
		{
			if (items != null)
			{
				foreach (EnvDTE.ProjectItem item in items)
				{
					//if (!(item is OAFileItem || item is OAFolderItem))
					//	continue;

					for (short i = 1; i <= item.FileCount; ++i)
					{
						var path = item.get_FileNames(i);
						if (!Path.GetExtension(path).Equals(".dll", StringComparison.OrdinalIgnoreCase) && File.Exists(path))
							yield return item.get_FileNames(i);
					}
					//yield return fileItem..Name;
					foreach (var s in CollectFileNamesRecursively(item.ProjectItems))
						yield return s;
				}
			}
		}

		private object CreateService(IServiceContainer container, Type serviceType)
		{
			if (serviceType == typeof(NemerleLanguageService))
			{
				NemerleLanguageService language = new NemerleLanguageService(this);

				language.SetSite(this);
				RegisterForIdleTime();

				return language;
			}
			else if (serviceType == typeof(INemerleLibraryManager))
			{
				return _libraryManager = new NemerleLibraryManager(this);
			}

			return null;
		}

		private void RegisterForIdleTime()
		{
			IOleComponentManager mgr = GetIOleComponentManager();

			if (_componentID == 0)
			{
				OLECRINFO[] crinfo = new OLECRINFO[1];

				crinfo[0].cbSize   = (uint)Marshal.SizeOf(typeof(OLECRINFO));
				crinfo[0].grfcrf   = (uint)_OLECRF.olecrfNeedIdleTime | (uint)_OLECRF.olecrfNeedPeriodicIdleTime;
				crinfo[0].grfcadvf = (uint)
					(_OLECADVF.olecadvfModal | _OLECADVF.olecadvfRedrawOff | _OLECADVF.olecadvfWarningsOff);
				crinfo[0].uIdleTimeInterval = 1000;

				ErrorHandler.ThrowOnFailure(mgr.FRegisterComponent(this, crinfo, out _componentID));
			}
		}

		#endregion

		#region Commands

		/// <summary>
		/// This function is called when the user clicks the menu item that shows the 
		/// tool window. See the Initialize method to see how the menu item is associated to 
		/// this function using the OleMenuCommandService service and the MenuCommand class.
		/// </summary>
		internal void OnAstToolWindowShow(object sender, EventArgs e)
		{
			// Get the instance number 0 of this tool window. This window is single instance so this instance
			// is actually the only one.
			// The last flag is set to true so that if the tool window does not exists it will be created.
			ToolWindowPane window = FindToolWindow(typeof(AstToolWindow), 0, true);
			if (window == null || window.Frame == null)
				throw new COMException(Resources.CannotCreateWindow);

			IVsWindowFrame windowFrame = (IVsWindowFrame)window.Frame;
			ErrorHandler.ThrowOnFailure(windowFrame.Show());
		}

		internal void OnSourceOutlinerWindowShow(object sender, EventArgs e)
		{
			ToolWindowPane window = FindToolWindow(typeof(SourceOutlinerToolWindow), 0, true);
			if(window == null || window.Frame == null)
				throw new COMException(Resources.CannotCreateWindow);

			_sourceOutlinerToolWindow = (SourceOutlinerToolWindow)window;
	
			IVsWindowFrame windowFrame = (IVsWindowFrame)window.Frame;
			ErrorHandler.ThrowOnFailure(windowFrame.Show());
		}

		#endregion

		#region Methods

		/// <summary>
		/// Changes the cursor to the hourglass cursor. 
		/// </summary>
		/// <returns>A return code or S_OK.</returns>
		public int SetWaitCursor()
		{
			int hr = VSConstants.S_OK;

			IVsUIShell VsUiShell = GetService(typeof(SVsUIShell)) as IVsUIShell;
			if(VsUiShell != null)
			{
				// There is no check for return code because 
				// any failure of this call is ignored.
				hr = VsUiShell.SetWaitCursor();
			}

			return hr;
		}

		/// <summary>
		/// Queries the environment to determine if an in-place active object can continue idle time processing.
		/// </summary>
		/// <returns>If idle processing can continue, the method returns <value>true</value>. If idle processing must terminate, it returns <value>false</value>.</returns>
		public bool CanContinueIdle
		{
			get
			{
				IOleComponentManager cm = GetIOleComponentManager();
				return ((cm != null) && (cm.FContinueIdle() != 0));
			}
		}

		#endregion

		#region GetService()

		internal T GetService<T>(bool throwIfFail)
			where T : class
		{
			return GetService<T, T>(throwIfFail);
		}

		internal T GetService<T>()
			where T : class
		{
			return GetService<T, T>(false);
		}

		internal TItf GetService<TItf, TSrv>()
			where TItf : class
		{
			return GetService<TItf, TSrv>(false);
		}

		internal void GetService<T>(out T itf)
			where T : class
		{
			itf = GetService<T, T>(false);
		}

		internal TItf GetService<TItf, TSrv>(bool throwIfFail)
			where TItf : class
		{
			TItf service = GetService(typeof(TSrv)) as TItf;

			if (service == null && throwIfFail)
				throw new ArgumentException("GetService() failed for query " + typeof(TSrv).Name);

			return service;
		}

		IOleComponentManager GetIOleComponentManager()
		{
			return GetService<IOleComponentManager, SOleComponentManager>(true);
		}

		#endregion

		#region Dispose()

		protected override void Dispose(bool disposing)
		{
			try
			{
        _mySolutionListener.Dispose();

				if (_componentID != 0)
				{
					IOleComponentManager mgr = GetIOleComponentManager();

					mgr.FRevokeComponent(_componentID);
					_componentID = 0;
				}

				if (_libraryManager != null)
				{
					_libraryManager.Dispose();
					_libraryManager = null;
				}
			}
			finally
			{
				base.Dispose(disposing);
			}
		}
		
		#endregion

		#region Overrides

		protected override int QueryClose(out bool canClose)
		{
			int res = base.QueryClose(out canClose);

			if (canClose)
			{
				Settings.Default.Save();
			}

			return  res;
		}

		#endregion

		#region IOleComponent Members

		public int FContinueMessageLoop(uint uReason, IntPtr pvLoopData, MSG[] pMsgPeeked)
		{
			return 1;
		}

		public int FDoIdle(uint grfidlef)
		{
			NemerleLanguageService lang = GetService(typeof(NemerleLanguageService)) as NemerleLanguageService;

			var isPeriodic = (grfidlef & (uint)_OLEIDLEF.oleidlefPeriodic) != 0;

			if (lang != null)
				lang.OnIdle(isPeriodic);

			if (_libraryManager != null)
				_libraryManager.OnIdle();

			if(_sourceOutlinerToolWindow != null)
				_sourceOutlinerToolWindow.OnIdle();

			return 0;
		}

		public int  FPreTranslateMessage(MSG[] pMsg)                        { return 0; }
		public int  FQueryTerminate     (int fPromptUser)                   { return 1; }

		public void OnAppActivate       (int fActive, uint dwOtherThreadID) {}
		public void OnEnterState        (uint uStateID, int fEnter)         {}
		public void OnLoseActivation    () {}
		public void Terminate           () {}

		public int FReserved1(uint dwReserved, uint message, IntPtr wParam, IntPtr lParam)
		{
			return 1;
		}

		public IntPtr HwndGetWindow(uint dwWhich, uint dwReserved)
		{
			return IntPtr.Zero;
		}

		public void OnActivationChange(
			IOleComponent  pic,
			int            fSameComponent,
			OLECRINFO[]    pcrinfo,
			int            fHostIsActivating, 
			OLECHOSTINFO[] pchostinfo,
			uint           dwReserved)
		{
		}

		#endregion

		#region IVsInstalledProduct Members

		public int IdBmpSplash(out uint pIdBmp)
		{
			pIdBmp = 300; // nevermind, does not called by VS2005
			return VSConstants.S_OK;
		}

		public int IdIcoLogoForAboutbox(out uint pIdIco)
		{
			pIdIco = 300; // used for splash screen also
			return VSConstants.S_OK;
		}

		public int OfficialName(out string pbstrName)
		{
			pbstrName = NemerleConstants.LanguageName;
			return VSConstants.S_OK;
		}

		public int ProductDetails(out string pbstrProductDetails)
		{
			pbstrProductDetails = NemerleConstants.ProductDetails;
			return VSConstants.S_OK;
		}

		public int ProductID(out string pbstrPID)
		{
			pbstrPID = NemerleConstants.PLKProductVersion;
			return VSConstants.S_OK;
		}

		#endregion
	}
}
