using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.Windows.Design.Host;
using Nemerle.VisualStudio.WPFProviders;

namespace Nemerle.VisualStudio.Project
{
	class NemerleDependentFileNode : DependentFileNode
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

		// Called since the FileNode.ImageIndex returns -1 by default.
		//
		public override object GetIconHandle(bool open)
		{
			if (FileName.EndsWith(NemerleConstants.FileExtension, StringComparison.InvariantCultureIgnoreCase))
				return PackageUtilities.GetIntPointerFromImage(
					NemerleProjectNode.NemerleImageList.Images[(int)NemerleConstants.ImageListIndex.NemerleSource]);

			return base.GetIconHandle(open);
		}

		#region CodeDomProvider

		NemerleFileNodeCodeDomProvider _codeDomProvider;

		protected internal NemerleFileNodeCodeDomProvider CodeDomProvider
		{
			get
			{
				if (_codeDomProvider == null)
					_codeDomProvider = new NemerleFileNodeCodeDomProvider(this);

				return _codeDomProvider;
			}
		}

		#endregion

		#region Members

		private DesignerContext _designerContext;

		protected internal DesignerContext DesignerContext
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

		protected override NodeProperties CreatePropertiesObject()
		{
			return new NemerleFileNodeProperties(this);
		}
	}
}
