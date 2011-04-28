using System;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Project;

using VsCommands = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;
using OleConstants = Microsoft.VisualStudio.OLE.Interop.Constants;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Represents a Folder node in a Nemerle project.
	/// </summary>
	public class NemerleFolderNode : FolderNode, IProjectSourceNode
	{
		#region ctors

		/// <summary>
		/// Initializes a new instance of the <see cref="NemerleFolderNode"/> class.
		/// </summary>
		/// <param name="root">The root <see cref="NemerleProjectNode"/> that contains this node.</param>
		/// <param name="directoryPath">Root of the hierarchy.</param>
		/// <param name="element">The element that contains MSBuild properties.</param>
		public NemerleFolderNode(ProjectNode root, string directoryPath, ProjectElement element)
			: this(root, directoryPath, element, false)
		{
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="NemerleFolderNode"/> class.
		/// </summary>
		/// <param name="root">The root <see cref="NemerleProjectNode"/> that contains this node.</param>
		/// <param name="directoryPath">Root of the hierarchy</param>
		/// <param name="element">The element that contains MSBuild properties.</param>
		/// <param name="isNonMemberItem">Indicates if this node is not a member of the project.</param>
		[SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
		public NemerleFolderNode(ProjectNode root, string directoryPath, ProjectElement element, bool isNonMemberItem)
			: base(root, directoryPath, element)
		{
			IsNonMemberItem = isNonMemberItem;

			// Folders do not participate in SCC.
			ExcludeNodeFromScc = true;
		}

		#endregion

		#region IProjectSourceNode Members

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
			NemerleProjectNode projectNode = ProjectMgr as NemerleProjectNode;
			if (projectNode == null || projectNode.IsClosed)
				return (int)OleConstants.OLECMDERR_E_NOTSUPPORTED;

			if (IsNonMemberItem)
				return VSConstants.S_OK; // do nothing, just ignore it.

			((NemerlePackage)ProjectMgr.Package).SetWaitCursor();

			// Check out the project file.
			if (!projectNode.QueryEditProjectFile(false))
				throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);

			// remove children, if any, before removing from the hierarchy
			for (HierarchyNode child = FirstChild; child != null; child = child.NextSibling)
			{
				IProjectSourceNode node = child as IProjectSourceNode;
				if (node == null) continue;

				int result = node.ExcludeFromProject();
				if (result != VSConstants.S_OK)
					return result;
			}

			if (projectNode.ShowAllFilesEnabled && Directory.Exists(Url))
			{
				string url = Url;
				SetProperty((int)__VSHPROPID.VSHPROPID_IsNonMemberItem, true);
				ItemNode.RemoveFromProjectFile();
				ItemNode = new ProjectElement(ProjectMgr, null, true);  // now we have to create a new ItemNode to indicate that this is virtual node.
				ItemNode.Rename(url);
				ItemNode.SetMetadata(ProjectFileConstants.Name, Url);
				ReDraw(UIHierarchyElement.Icon); // we have to redraw the icon of the node as it is now not a member of the project and shoul be drawn using a different icon.
			}
			else if (Parent != null) // the project node has no parentNode
			{
				// this is important to make it non member item. otherwise, the multi-selection scenario would
				// not work if it has any parent child relation.
				SetProperty((int)__VSHPROPID.VSHPROPID_IsNonMemberItem, true);

				// remove from the hierarchy
				OnItemDeleted();
				Parent.RemoveChild(this);
				ItemNode.RemoveFromProjectFile();
			}

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
			return ((IProjectSourceNode)this).IncludeInProject(true);
		}

		/// <summary>
		/// Include the item into the project system recursively.
		/// </summary>
		/// <param name="recursive">Flag that indicates if the inclusion should be recursive or not.</param>
		/// <returns>Returns success or failure code.</returns>
		[SuppressMessage("Microsoft.Design", "CA1033:InterfaceMethodsShouldBeCallableByChildTypes")]
		int IProjectSourceNode.IncludeInProject(bool recursive)
		{
			if (ProjectMgr == null || ProjectMgr.IsClosed)
				return (int)OleConstants.OLECMDERR_E_NOTSUPPORTED;

			if (!IsNonMemberItem)
				return VSConstants.S_OK; // do nothing, just ignore it.

			((NemerlePackage)ProjectMgr.Package).SetWaitCursor();

			// Check out the project file.
			if (!ProjectMgr.QueryEditProjectFile(false))
				throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);

			// make sure that all parent folders are included in the project
			HierarchyHelpers.EnsureParentFolderIncluded(this);

			// now add this node to the project.
			AddToMSBuild(recursive);
			ReDraw(UIHierarchyElement.Icon);

			HierarchyHelpers.RefreshPropertyBrowser(this);

			return VSConstants.S_OK;
		}

		#endregion

		#region Public Methods

		/// <summary>
		/// Expands the folder.
		/// </summary>
		public void ExpandFolder()
		{
			SetExpanded(true);
		}

		/// <summary>
		/// Collapses the folder.
		/// </summary>
		public void CollapseFolder()
		{
			SetExpanded(false);
		}

		#endregion

		#region Overrides

		/// <summary>
		/// Menu Command Id for Folder item.
		/// </summary>
		/// <value>Menu Command Id for Folder item.</value>
		public override int MenuCommandId
		{
			get
			{
				return IsNonMemberItem ? VsMenus.IDM_VS_CTXT_XPROJ_MULTIITEM : base.MenuCommandId;
			}
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
			if ((__VSHPROPID)propId == __VSHPROPID.VSHPROPID_IsNonMemberItem)
				return IsNonMemberItem;

			return base.GetProperty(propId);
		}

		/// <summary>
		/// Provides the node name for inline editing of caption. 
		/// Overriden to diable this fuctionality for non member fodler node.
		/// </summary>
		/// <returns>Caption of the folder node if the node is a member item, null otherwise.</returns>
		public override string GetEditLabel()
		{
			return IsNonMemberItem ? null : base.GetEditLabel();
		}

		// Because the IsExpanded is not working properly (as of this date, 10/18/2007), that's why we are using the 
		// GetIconHandle method. When we fix the IsExpanded property, we should switch to ImageIndex property instead
		// of this method.

		/// <summary>
		/// Gets the image icon handle for the folder node.
		/// </summary>
		/// <param name="open">Flag that indicated if the folder is in expanded (opened) mode.</param>
		/// <returns>Image icon handle.</returns>
		public override object GetIconHandle(bool open)
		{
			if (IsNonMemberItem)
				return ProjectMgr.ImageHandler.GetIconHandle(open ?
						(int)ProjectNode.ImageName.OpenExcludedFolder :
						(int)ProjectNode.ImageName.ExcludedFolder);

			return base.GetIconHandle(open);
		}

		/// <summary>
		/// Creates an object derived from <see cref="NodeProperties"/> that will be used to expose
		/// properties specific for this object to the property browser.
		/// </summary>
		/// <returns>A new <see cref="NemerleFileNodeProperties"/> object.</returns>
		protected override NodeProperties CreatePropertiesObject()
		{
			if (IsNonMemberItem)
				return new NemerleFolderNodeNonMemberProperties(this);

			return new NemerleFolderNodeProperties(this);
		}

		/// <summary>
		/// Handles command status on a node. Should be overridden by descendant nodes. If a command cannot be handled then the base should be called.
		/// </summary>
		/// <param name="cmdGroup">A unique identifier of the command group. The pguidCmdGroup parameter can be NULL to specify the standard group.</param>
		/// <param name="cmd">The command to query status for.</param>
		/// <param name="pCmdText">Pointer to an OLECMDTEXT structure in which to return the name and/or status information of a single command. Can be NULL to indicate that the caller does not require this information.</param>
		/// <param name="result">An out parameter specifying the QueryStatusResult of the command.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		[SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#", Justification = "Suppressing to avoid conflict with style cop.")]
		protected override int QueryStatusOnNode(Guid cmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
		{
			int returnCode;
			if (HierarchyHelpers.QueryStatusOnProjectSourceNode(this, cmdGroup, cmd, ref result, out returnCode))
				return returnCode;

			return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
		}

		/// <summary>
		/// Handles command execution.
		/// </summary>
		/// <param name="cmdGroup">Unique identifier of the command group</param>
		/// <param name="cmd">The command to be executed.</param>
		/// <param name="cmdexecopt">Values describe how the object should execute the command.</param>
		/// <param name="pvaIn">Pointer to a VARIANTARG structure containing input arguments. Can be NULL</param>
		/// <param name="pvaOut">VARIANTARG structure to receive command output. Can be NULL.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		[SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#", Justification = "Suppressing to avoid conflict with style cop.")]
		[SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "2#", Justification = "Suppressing to avoid conflict with style cop.")]
		protected override int ExecCommandOnNode(Guid cmdGroup, uint cmd, uint cmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
		{
			if (cmdGroup == VsMenus.guidStandardCommandSet2K)
			{
				switch ((VsCommands2K)cmd)
				{
					case VsCommands2K.INCLUDEINPROJECT:
						return ((IProjectSourceNode)this).IncludeInProject();

					case VsCommands2K.EXCLUDEFROMPROJECT:
						return ((IProjectSourceNode)this).ExcludeFromProject();
				}
			}

			return base.ExecCommandOnNode(cmdGroup, cmd, cmdexecopt, pvaIn, pvaOut);
		}

		#endregion

		#region Methods

		/// <summary>
		/// Adds the this node to the build system.
		/// </summary>
		/// <param name="recursive">Flag to indicate if the addition should be recursive.</param>
		protected virtual void AddToMSBuild(bool recursive)
		{
			NemerleProjectNode projectNode = ProjectMgr as NemerleProjectNode;
			if (projectNode == null || projectNode.IsClosed)
				return;

			ItemNode = projectNode.CreateMsBuildFolderProjectElement(Url);
			SetProperty((int)__VSHPROPID.VSHPROPID_IsNonMemberItem, false);
			if (recursive)
			{
				for (HierarchyNode node = FirstChild; node != null; node = node.NextSibling)
				{
					IProjectSourceNode sourceNode = node as IProjectSourceNode;
					if (sourceNode != null)
						sourceNode.IncludeInProject(true);
				}
			}
		}

		/// <summary>
		/// Sets the expanded state of the folder.
		/// </summary>
		/// <param name="expanded">Flag that indicates the expanded state of the folder.
		/// This should be 'true' for expanded and 'false' for collapsed state.</param>
		protected void SetExpanded(bool expanded)
		{
			IsExpanded = expanded;
			SetProperty((int)__VSHPROPID.VSHPROPID_Expanded, expanded);

			// If we are in automation mode then skip the ui part
			if (Utilities.IsInAutomationFunction(ProjectMgr.Site))
				return;

			IVsUIHierarchyWindow uiWindow = UIHierarchyUtilities.GetUIHierarchyWindow(ProjectMgr.Site, SolutionExplorer);
			int result = uiWindow.ExpandItem(ProjectMgr, ID, expanded ? EXPANDFLAGS.EXPF_ExpandFolder : EXPANDFLAGS.EXPF_CollapseFolder);
			ErrorHandler.ThrowOnFailure(result);

			// then post the expand command to the shell. Folder verification and creation will
			// happen in the setlabel code...
			IVsUIShell shell = (IVsUIShell)ProjectMgr.GetService(typeof(IVsUIShell));

			object dummy = null;
			Guid cmdGroup = VsMenus.guidStandardCommandSet97;
			result = shell.PostExecCommand(ref cmdGroup, (uint)(expanded ? VsCommands.Expand : VsCommands.Collapse), 0, ref dummy);
			ErrorHandler.ThrowOnFailure(result);
		}

		#endregion
	}
}
