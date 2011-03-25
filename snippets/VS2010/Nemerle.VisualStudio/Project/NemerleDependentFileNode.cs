using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.Windows.Design.Host;
using Nemerle.VisualStudio.WPFProviders;
using System.Diagnostics.CodeAnalysis;
using Microsoft.VisualStudio;

using OleConstants = Microsoft.VisualStudio.OLE.Interop.Constants;
using VsCommands = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;

namespace Nemerle.VisualStudio.Project
{
	class NemerleDependentFileNode : NemerleFileNode
	{
		#region ctor

		/// <summary>
		/// Constructor for the NemerleDependentFileNode
		/// </summary>
		/// <param name="root">Root of the hierarchy</param>
		/// <param name="e">Associated project element</param>
		public NemerleDependentFileNode(ProjectNode root, ProjectElement e)
			: base(root, e)
		{
			HasParentNodeNameRelation = false;
		}

		#endregion

		#region overridden methods
		/// <summary>
		/// Disable rename
		/// </summary>
		/// <param name="label">new label</param>
		/// <returns>E_NOTIMPLE in order to tell the call that we do not support rename</returns>
		public override string GetEditLabel()
		{
			throw new NotImplementedException();
		}

		// Called since the FileNode.ImageIndex returns -1 by default.
		//
		public override object GetIconHandle(bool open)
		{
			if (FileName.EndsWith(NemerleConstants.FileExtension, StringComparison.InvariantCultureIgnoreCase))
				return PackageUtilities.GetIntPointerFromImage(
					NemerleProjectNode.NemerleImageList.Images[(int)NemerleConstants.ImageListIndex.NemerleSource]);

			return this.ProjectMgr.ImageHandler.GetIconHandle(this.ImageIndex);
		}

		/// <summary>
		/// Disable certain commands for dependent file nodes 
		/// </summary>
		protected override int QueryStatusOnNode(Guid cmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
		{
			if (cmdGroup == Microsoft.VisualStudio.Project.VsMenus.guidStandardCommandSet97)
			{
				switch ((VsCommands)cmd)
				{
					case VsCommands.Copy:
					case VsCommands.Paste:
					case VsCommands.Cut:
					case VsCommands.Rename:
						result |= QueryStatusResult.NOTSUPPORTED;
						return VSConstants.S_OK;

					case VsCommands.ViewCode:
					case VsCommands.Open:
					case VsCommands.OpenWith:
						result |= QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
						return VSConstants.S_OK;
				}
			}
			else if (cmdGroup == Microsoft.VisualStudio.Project.VsMenus.guidStandardCommandSet2K)
			{
				if ((VsCommands2K)cmd == VsCommands2K.EXCLUDEFROMPROJECT)
				{
					result |= QueryStatusResult.NOTSUPPORTED;
					return VSConstants.S_OK;
				}
			}
			else
			{
				return (int)OleConstants.OLECMDERR_E_UNKNOWNGROUP;
			}
			return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
		}

		/// <summary>
		/// DependentFileNodes node cannot be dragged.
		/// </summary>
		/// <returns>null</returns>
		protected internal override StringBuilder PrepareSelectedNodesForClipBoard()
		{
			return null;
		}

		/// <summary>
		/// Redraws the state icon if the node is not excluded from source control.
		/// </summary>
		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Scc")]
		protected internal override void UpdateSccStateIcons()
		{
			if (!this.ExcludeNodeFromScc)
			{
				this.Parent.ReDraw(UIHierarchyElement.SccState);
			}
		}

		protected override NodeProperties CreatePropertiesObject()
		{
			return new NemerleFileNodeProperties(this);
		}

		#endregion

		#region Members

		private DesignerContext _designerContext;
		protected internal override DesignerContext DesignerContext
		{
			get
			{
				if (_designerContext == null)
				{
					//Set the EventBindingProvider for this XAML file so the designer will call it
					//when event handlers need to be generated
					_designerContext = new DesignerContext { EventBindingProvider = new NemerleEventBindingProvider(this) };
				}

				return _designerContext;
			}
		}

		#endregion

	}
}
