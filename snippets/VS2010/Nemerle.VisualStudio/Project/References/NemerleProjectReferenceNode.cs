using System;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project;
using System.IO;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;

namespace Nemerle.VisualStudio.Project
{
	[CLSCompliant(false), ComVisible(true)]
	public class NemerleProjectReferenceNode : ProjectReferenceNode
	{
		#region ctors
    readonly NemerleProjectNode _projectNode;
		/// <summary>
		/// Constructor for the ReferenceNode. It is called when the project is reloaded, when the project element representing the refernce exists. 
		/// </summary>
		public NemerleProjectReferenceNode(ProjectNode root, ProjectElement element)
			: base(root, element)
		{
      root = _projectNode;
		}

		protected override void BindReferenceData()
		{
			base.BindReferenceData();
		}

		/// <summary>
		/// constructor for the NemerleProjectReferenceNode
		/// </summary>
		public NemerleProjectReferenceNode(ProjectNode root, string referencedProjectName, string projectPath, string projectReference)
			: base(root, referencedProjectName, projectPath, projectReference)
		{
      root = _projectNode;
		}

		#endregion

    public override void Remove(bool removeFromStorage)
    {
      base.Remove(removeFromStorage);
      ((NemerleOAProject)_projectNode.GetAutomationObject()).PersistProjectFile();
      
    }
		protected override NodeProperties CreatePropertiesObject()
		{
			return new NemerleProjectReferencesProperties(this, "Project Reference Properties");
		}

		public override object GetIconHandle(bool open)
		{
			//TODO: Shou special icon for Nemerle (and other) project reference
			return base.GetIconHandle(open);
		}
	}
}
