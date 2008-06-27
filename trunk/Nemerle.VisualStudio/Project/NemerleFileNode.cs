using System;
using System.Diagnostics;
using System.Globalization;
using System.IO;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Package.Automation;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Windows.Design.Host;
using VSLangProj;

using Nemerle.VisualStudio.WPFProviders;
using Nemerle.VisualStudio.Package;

using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;
using VsCommands          = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsMenus             = Microsoft.VisualStudio.Shell.VsMenus;

namespace Nemerle.VisualStudio.Project
{
	class NemerleFileNode : FileNode
	{
		#region ctors

		internal NemerleFileNode(ProjectNode root, ProjectElement e) : base(root, e)
		{
			_selectionChangedListener =
				new SelectionElementValueChangedListener(
					new ServiceProvider((IOleServiceProvider)root.GetService(typeof (IOleServiceProvider))), root);

			_selectionChangedListener.Init();

			//((FileNodeProperties)NodeProperties).OnCustomToolChanged		  += OnCustomToolChanged;
			//((FileNodeProperties)NodeProperties).OnCustomToolNameSpaceChanged += OnCustomToolNameSpaceChanged;

			// HasDesigner property is not virtual, so we have to set it up in the ctor.
			InferHasDesignerFromSubType();
		}

		#endregion

		#region Fields

		SelectionElementValueChangedListener _selectionChangedListener;
		NemerleOAFileItem                    _automationObject;

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

		private          OAVSProjectItem _vsProjectItem;
		protected internal VSProjectItem  VSProjectItem
		{
			get { return _vsProjectItem ?? (_vsProjectItem = new OAVSProjectItem(this)); }
		}

		private            NemerleFileNodeCodeDomProvider _codeDomProvider;
		protected internal NemerleFileNodeCodeDomProvider  CodeDomProvider
		{
			get { return _codeDomProvider ?? (_codeDomProvider = new NemerleFileNodeCodeDomProvider(this)); }
		}

		private            DesignerContext _designerContext;
		protected internal DesignerContext  DesignerContext
		{
			get
			{
				if (_designerContext == null)
				{
					NemerleFileNode nFile = Parent.FindChild(Url.Replace(".xaml", ".n")) as NemerleFileNode;

					//Set the EventBindingProvider for this XAML file so the designer will call it
					//when event handlers need to be generated
					_designerContext = new DesignerContext { EventBindingProvider = new NemerleEventBindingProvider(nFile) };
				}

				return _designerContext;
			}
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
		/*
		public override int ImageIndex
		{
			get
			{
				if (IsFormSubType)
					return (int)NemerleConstants.ImageListIndex.NemerleForm;

				if (FileName.ToLower().EndsWith(NemerleConstants.FileExtension))
					return (int)NemerleConstants.ImageListIndex.NemerleSource;

				return base.ImageIndex;
			}
		} */
		

		#endregion

		#region overridden methods

		public override int Close()
		{
			if (_selectionChangedListener != null)
				_selectionChangedListener.Dispose();

			return base.Close();
		}

		public override object GetAutomationObject()
		{
			if(_automationObject == null)
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
				int imageIndex = HasDesigner?
					NemerleConstants.ImageListIndex.NemerleForm:
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
			FileDocumentManager manager = GetDocumentManager() as FileDocumentManager;

			Debug.Assert(manager != null, "Could not get the FileDocumentManager");

			Guid viewGuid = HasDesigner ? VSConstants.LOGVIEWID_Designer : VSConstants.LOGVIEWID_Code;

			IVsWindowFrame frame;

			manager.Open(false, false, viewGuid, out frame, WindowFrameShowAction.Show);
		}

		protected override int ExecCommandOnNode(
			Guid guidCmdGroup, uint cmd, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
		{
			Debug.Assert(ProjectMgr != null, "The NemerleFileNode has no project manager");

			if (ProjectMgr == null)
				throw new InvalidOperationException();

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
		protected override int QueryStatusOnNode(
			Guid guidCmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
		{
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
			return new NemerleFileNodeProperties(this);
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
			string		relativePath = Path.GetFileName(ItemNode.GetMetadata(ProjectFileConstants.Include));
			HierarchyNode parent	   = Parent;

			while (parent != null && !(parent is ProjectNode))
			{
				relativePath = Path.Combine(parent.Caption, relativePath);
				parent	   = parent.Parent;
			}

			return relativePath;
		}

		#endregion
	}
}
