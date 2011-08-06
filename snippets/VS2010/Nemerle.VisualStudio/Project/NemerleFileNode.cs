using System;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Windows.Design.Host;
using VSLangProj;

using Nemerle.VisualStudio.WPFProviders;
using Nemerle.VisualStudio.Package;

using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;
using VsCommands = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsMenus = Microsoft.VisualStudio.Shell.VsMenus;
using OleConstants = Microsoft.VisualStudio.OLE.Interop.Constants;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;

namespace Nemerle.VisualStudio.Project
{
	class NemerleFileNode : FileNode, IProjectSourceNode
	{

		#region ctors

		internal NemerleFileNode(ProjectNode root, ProjectElement e)
			: this(root, e, false) { }

		/// <summary>
		/// Initializes a new instance of the <see cref="NemerleFileNode"/> class.
		/// </summary>
		/// <param name="root">The root <see cref="NemerleProjectNode"/> that contains this node.</param>
		/// <param name="element">The element that contains MSBuild properties.</param>
		/// <param name="isNonMemberItem">Flag that indicates if the file is not part of the project.</param>
		[SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
		public NemerleFileNode(ProjectNode root, ProjectElement element, bool isNonMemberItem)
			: base(root, element)
		{
			IsNonMemberItem = isNonMemberItem;

			_selectionChangedListener =
					new SelectionElementValueChangedListener(
							new ServiceProvider((IOleServiceProvider)root.GetService(typeof(IOleServiceProvider))), root);

			_selectionChangedListener.Init();

			//((FileNodeProperties)NodeProperties).OnCustomToolChanged		  += OnCustomToolChanged;
			//((FileNodeProperties)NodeProperties).OnCustomToolNameSpaceChanged += OnCustomToolNameSpaceChanged;

			// HasDesigner property is not virtual, so we have to set it up in the ctor.
			InferHasDesignerFromSubType();

			var url = this.Url;
			var ext = Path.GetExtension(url);

			//if (ext.Equals(".resx", StringComparison.InvariantCultureIgnoreCase))
			//{
			//  // TODO: Удалить это дело, так как теперь оповещения должны быть реализованы в Engine.
			//  url = Path.GetFullPath(this.Url);
			//  var path = Path.GetDirectoryName(url);
			//  var name = Path.GetFileName(url);
			//  _watcher = new FileSystemWatcher(path, name);
			//  _watcher.NotifyFilter = NotifyFilters.LastWrite;
			//  _watcher.Changed += watcher_Changed;
			//  _watcher.EnableRaisingEvents = true;
			//}
		}

		#endregion

		#region Fields

		bool _isDisposed;
		FileSystemWatcher _watcher;
		SelectionElementValueChangedListener _selectionChangedListener;
		NemerleOAFileItem _automationObject;

		#endregion

		#region Properties

		/// <summary>
		/// Returns the SubType of an Nemerle FileNode. It is 
		/// </summary>
		public string SubType
		{
			get { return ItemNode.GetMetadata(ProjectFileConstants.SubType); }
			set { ItemNode.SetMetadata(ProjectFileConstants.SubType, value); }
		}

		private OAVSProjectItem _vsProjectItem;
		protected internal VSProjectItem VSProjectItem
		{
			get { return _vsProjectItem ?? (_vsProjectItem = new OAVSProjectItem(this)); }
		}

		private NemerleFileNodeCodeDomProvider _codeDomProvider;
		protected internal NemerleFileNodeCodeDomProvider CodeDomProvider
		{
			get { return _codeDomProvider ?? (_codeDomProvider = new NemerleFileNodeCodeDomProvider(this)); }
		}

		private DesignerContext _designerContext;
		protected internal virtual DesignerContext DesignerContext
		{
			get
			{
				if (_designerContext == null)
				{
					NemerleFileNode nFile = Parent.FindChild(Url.Replace(".xaml", ".xaml.n")) as NemerleFileNode;

					if (nFile == null)
						nFile = Parent.FindChild(Path.ChangeExtension(Url, ".Designer.n")) as NemerleFileNode;


					//Set the EventBindingProvider for this XAML file so the designer will call it
					//when event handlers need to be generated
					_designerContext = new DesignerContext { EventBindingProvider = new NemerleEventBindingProvider(nFile) };
				}

				return _designerContext;
			}
		}

		#endregion

		#region IProjectSourceNode members

		private bool _isNonMemberItem;
		/// <summary>
		/// Flag that indicates if this node is not a member of the project.
		/// </summary>
		/// <value>true if the item is not a member of the project build, false otherwise.</value>
		public bool IsNonMemberItem
		{
			get { return _isNonMemberItem; }
			private set { _isNonMemberItem = value; }
		}

		/// <summary>
		/// Exclude the item from the project system.
		/// </summary>
		/// <returns>Returns success or failure code.</returns>
		[SuppressMessage("Microsoft.Design", "CA1033:InterfaceMethodsShouldBeCallableByChildTypes")]
		int IProjectSourceNode.ExcludeFromProject()
		{
			if (ProjectMgr == null || ProjectMgr.IsClosed)
				return (int)OleConstants.OLECMDERR_E_NOTSUPPORTED;

			if (IsNonMemberItem)
				return VSConstants.S_OK; // do nothing, just ignore it.

			((NemerlePackage)ProjectMgr.Package).SetWaitCursor();
			// Ask Document tracker listeners if we can remove the item.
			{ // just to limit the scope.
				string documentToRemove = GetMkDocument();
				string[] filesToBeDeleted = new[] { documentToRemove };
				VSQUERYREMOVEFILEFLAGS[] queryRemoveFlags = GetQueryRemoveFileFlags(filesToBeDeleted);
				if (!ProjectMgr.Tracker.CanRemoveItems(filesToBeDeleted, queryRemoveFlags))
					return (int)OleConstants.OLECMDERR_E_CANCELED;

				// Close the document if it has a manager.
				DocumentManager manager = GetDocumentManager();
				if (manager != null)
				{
					if (manager.Close(__FRAMECLOSE.FRAMECLOSE_PromptSave) == VSConstants.E_ABORT)
						return VSConstants.OLE_E_PROMPTSAVECANCELLED;
				}

				if (!ProjectMgr.QueryEditProjectFile(false))
					throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);
			}

			// close the document window if open.
			CloseDocumentWindow(this);

			NemerleProjectNode projectNode = ProjectMgr as NemerleProjectNode;

			if (projectNode != null && projectNode.ShowAllFilesEnabled && File.Exists(Url))
			{
				string url = Url; // need to store before removing the node.
				ItemNode.RemoveFromProjectFile();
				ProjectMgr.Tracker.OnItemRemoved(url, VSREMOVEFILEFLAGS.VSREMOVEFILEFLAGS_NoFlags);
				SetProperty((int)__VSHPROPID.VSHPROPID_IsNonMemberItem, true); // Set it as non member item
				ItemNode = new ProjectElement(ProjectMgr, null, true); // now we have to set a new ItemNode to indicate that this is virtual node.
				ItemNode.Rename(url);
				ItemNode.SetMetadata(ProjectFileConstants.Name, url);

				//ProjectMgr.OnItemDeleted();
				var proj = ProjectInfo.FindProject(ProjectMgr);
				if (proj != null)
					proj.RemoveSource(this.Url);

				ReDraw(UIHierarchyElement.Icon); // We have to redraw the icon of the node as it is now not a member of the project and should be drawn using a different icon.
				ReDraw(UIHierarchyElement.SccState); // update the SCC state icon.
			}
			else if (Parent != null) // the project node has no parentNode
			{
				// Remove from the Hierarchy
				OnItemDeleted();
				Parent.RemoveChild(this);
				ItemNode.RemoveFromProjectFile();
			}

			ResetProperties();

			HierarchyHelpers.RefreshPropertyBrowser(this);

			return VSConstants.S_OK;
		}

		/// <summary>
		/// Include the item into the project system.
		/// </summary>
		/// <returns>Returns success or failure code.</returns>
		[SuppressMessage("Microsoft.Design", "CA1033:InterfaceMethodsShouldBeCallableByChildTypes")]
		int IProjectSourceNode.IncludeInProject()
		{
			NemerleProjectNode projectNode = ProjectMgr as NemerleProjectNode;
			if (projectNode == null || projectNode.IsClosed)
				return (int)OleConstants.OLECMDERR_E_NOTSUPPORTED;

			if (!IsNonMemberItem)
				return VSConstants.S_OK;

			((NemerlePackage)ProjectMgr.Package).SetWaitCursor();

			// Check out the project file.
			if (!projectNode.QueryEditProjectFile(false))
				throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);

			HierarchyHelpers.EnsureParentFolderIncluded(this);

			SetProperty((int)__VSHPROPID.VSHPROPID_IsNonMemberItem, false);

			ItemNode = projectNode.CreateMsBuildFileProjectElement(Url);
			ProjectMgr.Tracker.OnItemAdded(Url, VSADDFILEFLAGS.VSADDFILEFLAGS_NoFlags);

			var proj = ProjectInfo.FindProject(ProjectMgr);
			if (proj != null)
				proj.AddSource(this.Url);

			//projectNode.OnItemAdded(Parent, this);

			ReDraw(UIHierarchyElement.Icon); // We have to redraw the icon of the node as it is now a member of the project and should be drawn using a different icon.
			ReDraw(UIHierarchyElement.SccState); // update the SCC state icon.

			ResetProperties();

			HierarchyHelpers.RefreshPropertyBrowser(this);

			return VSConstants.S_OK;
		}

		/// <summary>
		/// Resets the Node properties for file node item.
		/// </summary>
		protected void ResetProperties()
		{
			bool changed = false;

			if (IsNonMemberItem)
			{
				if (!(_currentNodeProperties is NemerleFileNodeNonMemberProperties))
				{
					_currentNodeProperties = new NemerleFileNodeNonMemberProperties(this);
					changed = true;
				}
			}
			else
			{
				if (!(_currentNodeProperties is NemerleFileNodeProperties))
				{
					_currentNodeProperties = new NemerleFileNodeProperties(this);
					changed = true;
				}
			}

			if (changed)
				OnPropertyChanged(this, (int)__VSHPROPID.VSHPROPID_BrowseObject, 0);
		}

		/// <summary>
		/// Include the item into the project system recursively.
		/// </summary>
		/// <param name="recursive">Flag that indicates if the inclusion should be recursive or not.</param>
		/// <returns>Returns success or failure code.</returns>
		[SuppressMessage("Microsoft.Design", "CA1033:InterfaceMethodsShouldBeCallableByChildTypes")]
		int IProjectSourceNode.IncludeInProject(bool recursive)
		{
			// recursive doesn't make any sense in case of a file item. so just include this item.
			return ((IProjectSourceNode)this).IncludeInProject();
		}

		#endregion

		#region Overridden Properties

		protected internal override DocumentManager GetDocumentManager()
		{
			return new NemerleFileDocumentManager(this);
		}

		internal override object Object { get { return VSProjectItem; } }

		public bool IsFormSubType
		{
			get
			{
				string result = this.ItemNode.GetMetadata(ProjectFileConstants.SubType);
				if (!String.IsNullOrEmpty(result) && string.Compare(result, ProjectFileAttributeValue.Form, true, CultureInfo.InvariantCulture) == 0)
					return true;
				else
					return false;
			}
		}

		private NodeProperties _currentNodeProperties;
		/// <summary>
		/// Defines the properties attached to this node.
		/// </summary>
		/// <value>Defines the properties attached to this node.</value>
		public override NodeProperties NodeProperties
		{
			// This should be removed once we have setter in NodeProperties property in HierarchyNode (MPF).
			get
			{
				if (_currentNodeProperties == null)
					_currentNodeProperties = CreatePropertiesObject();
				return _currentNodeProperties;
			}
		}


		/// <summary>
		/// Gets an index into the default <b>ImageList</b> of the icon to show for this file.
		/// </summary>
		/// <value>An index into the default  <b>ImageList</b> of the icon to show for this file.</value>
		public override int ImageIndex
		{
			get
			{
				if (IsNonMemberItem)
					return (int)ProjectNode.ImageName.ExcludedFile;

				if (!File.Exists(Url))
					return (int)ProjectNode.ImageName.MissingFile;

				return base.ImageIndex;
			}
		}

		#endregion

		#region overridden methods

		protected override FileNode RenameFileNode(string oldFileName, string newFileName, uint newParentId)
		{
			var projectInfo = ProjectInfo.FindProject(oldFileName);

			if (projectInfo == null)
				return base.RenameFileNode(oldFileName, newFileName, newParentId);

			projectInfo.RenameSource(oldFileName, newFileName);

			FileNode result = null;
			projectInfo.BeginRenameFile();
			try { result = base.RenameFileNode(oldFileName, newFileName, newParentId); }
			finally { projectInfo.EndRenameFile(); }

			return result;
		}

		protected override void Dispose(bool disposing)
		{
			if (_isDisposed)
				return;

			try
			{
				if (_watcher != null)
				{
					_watcher.Changed -= watcher_Changed;
					_watcher.Dispose();
					_watcher = null;
				}
			}
			catch
			{
				base.Dispose(disposing);
				_isDisposed = true;
			}
		}

		public override int Close()
		{
			if (_selectionChangedListener != null)
				_selectionChangedListener.Dispose();

			return base.Close();
		}

		public override object GetAutomationObject()
		{
			if (_automationObject == null)
			{
				_automationObject = new NemerleOAFileItem((OAProject)ProjectMgr.GetAutomationObject(), this);
			}
			return _automationObject;
		}


		// Called since the FileNode.ImageIndex returns -1 by default.
		//
		public override object GetIconHandle(bool open)
		{
			if (FileName.EndsWith(NemerleConstants.FileExtension, StringComparison.InvariantCultureIgnoreCase))
			{
				int imageIndex = HasDesigner ?
					NemerleConstants.ImageListIndex.NemerleForm :
					NemerleConstants.ImageListIndex.NemerleSource;
				return
					PackageUtilities.GetIntPointerFromImage(NemerleProjectNode.NemerleImageList.Images[imageIndex]);
			}

			return base.GetIconHandle(open);
		}

		/// <summary>
		/// Open a file depending on the SubType property associated with the file item in the project file
		/// </summary>
		protected override void DoDefaultAction()
		{
			var manager = (FileDocumentManager)GetDocumentManager();

			Debug.Assert(manager != null, "Could not get the FileDocumentManager");

			Guid viewGuid = HasDesigner ? VSConstants.LOGVIEWID_Designer : VSConstants.LOGVIEWID_Code;

			IVsWindowFrame frame;

			manager.Open(false, false, viewGuid, out frame, WindowFrameShowAction.Show);
		}

		protected override int ExecCommandOnNode(Guid guidCmdGroup, uint cmd, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
		{
			Debug.Assert(ProjectMgr != null, "The NemerleFileNode has no project manager");

			if (ProjectMgr == null)
				throw new InvalidOperationException();

			int returnCode;

			if (HierarchyHelpers.ExecCommandOnProjectSourceNode(this, guidCmdGroup, cmd, nCmdexecopt, pvaIn, pvaOut, out returnCode))
				return returnCode;

			if (guidCmdGroup == MenuCmd.guidNemerleProjectCmdSet && cmd == (uint)MenuCmd.SetAsMain.ID)
			{
				// Set the MainFile project property to the Filename of this Node.
				//
				ProjectMgr.SetProjectProperty(NemerleProjectNodeProperties.MainFilePropertyName, GetRelativePath());

				return VSConstants.S_OK;
			}

			return base.ExecCommandOnNode(guidCmdGroup, cmd, nCmdexecopt, pvaIn, pvaOut);
		}

		/// <summary>
		/// Handles the menuitems
		/// </summary>
		protected override int QueryStatusOnNode(Guid guidCmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
		{
			int returnCode;
			if (HierarchyHelpers.QueryStatusOnProjectSourceNode(this, guidCmdGroup, cmd, ref result, out returnCode))
				return returnCode;

			if (guidCmdGroup == VsMenus.guidStandardCommandSet97)
			{
				switch ((VsCommands)cmd)
				{
					case VsCommands.AddNewItem:
					case VsCommands.AddExistingItem:
					case VsCommands.ViewCode:
						result |= QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
						return VSConstants.S_OK;

					case VsCommands.ViewForm:
						if (HasDesigner)
							result |= QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
						return VSConstants.S_OK;
				}
			}
			else if (guidCmdGroup == MenuCmd.guidNemerleProjectCmdSet)
				if (cmd == (uint)MenuCmd.SetAsMain.ID)
				{
					result |= QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
					return VSConstants.S_OK;
				}

			return base.QueryStatusOnNode(guidCmdGroup, cmd, pCmdText, ref result);
		}

		protected override NodeProperties CreatePropertiesObject()
		{
			if (IsNonMemberItem)
				return new NemerleFileNodeNonMemberProperties(this);

			return new NemerleFileNodeProperties(this);
		}

		/// <summary>
		/// Sets the node property.
		/// </summary>
		/// <param name="propid">Property id.</param>
		/// <param name="value">Property value.</param>
		/// <returns>Returns success or failure code.</returns>
		public override int SetProperty(int propid, object value)
		{
			if ((__VSHPROPID)propid == __VSHPROPID.VSHPROPID_IsNonMemberItem)
			{
				ErrorHelper.ThrowIsNull(value, "value");

				IsNonMemberItem = bool.Parse(value.ToString());
				ExcludeNodeFromScc = IsNonMemberItem;

				return VSConstants.S_OK;
			}

			return base.SetProperty(propid, value);
		}

		/// <summary>
		/// Gets the node property.
		/// </summary>
		/// <param name="propId">Property id.</param>
		/// <returns>The property value.</returns>
		public override object GetProperty(int propId)
		{
			switch ((__VSHPROPID)propId)
			{
				case __VSHPROPID.VSHPROPID_DefaultNamespace:
					//this.Parent;

					return null;
				case __VSHPROPID.VSHPROPID_IsNonMemberItem:
					return IsNonMemberItem;
				default:
					return base.GetProperty(propId);
			}
		}

		/// <summary>
		/// Provides the node name for inline editing of caption. 
		/// Overriden to diable this fuctionality for non member fodler node.
		/// </summary>
		/// <returns>Caption of the file node if the node is a member item, null otherwise.</returns>
		public override string GetEditLabel()
		{
			if (IsNonMemberItem)
				return null;

			return base.GetEditLabel();
		}

		#endregion

		#region Helper Methods

		public void InferHasDesignerFromSubType()
		{
			switch (SubType)
			{
				case ProjectFileAttributeValue.Component:
				case ProjectFileAttributeValue.Form:
				case ProjectFileAttributeValue.UserControl:
					HasDesigner = true;
					break;
				default:
					HasDesigner = false;
					break;
			}
		}

		protected string GetRelativePath()
		{
			string relativePath = Path.GetFileName(ItemNode.GetMetadata(ProjectFileConstants.Include));
			HierarchyNode parent = Parent;

			while (parent != null && !(parent is ProjectNode))
			{
				relativePath = Path.Combine(parent.Caption, relativePath);
				parent = parent.Parent;
			}

			return relativePath;
		}

		#endregion

		#region Members

		private void watcher_Changed(object sender, FileSystemEventArgs e)
		{
			var nProj = (NemerleProjectNode)ProjectMgr;
			Trace.Assert(nProj != null);
			nProj.ProjectInfo.Engine.RequestOnBuildTypesTree();
		}

		internal OleServiceProvider.ServiceCreatorCallback ServiceCreator
		{
			get { return new OleServiceProvider.ServiceCreatorCallback(this.CreateServices); }
		}

		private object CreateServices(Type serviceType)
		{
			object service = null;

			if (typeof(EnvDTE.ProjectItem) == serviceType)
				service = GetAutomationObject();
			else if (typeof(DesignerContext) == serviceType)
				service = this.DesignerContext;

			return service;
		}

		#endregion
	}
}
