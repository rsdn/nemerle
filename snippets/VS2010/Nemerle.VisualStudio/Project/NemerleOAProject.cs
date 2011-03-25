using System;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using System.Runtime.InteropServices;

namespace Nemerle.VisualStudio.Project
{
	[ComVisible(true), CLSCompliant(false)]
	public class NemerleOAProject : OAProject
	{
		#region ctor

		public NemerleOAProject(ProjectNode project) : base(project)
		{
		}

		#endregion

		/// <summary>
		/// Gets a ProjectItems collection for the Project object.
		/// </summary>
		public override EnvDTE.ProjectItems ProjectItems
		{
			get { return new OAProjectItems(this, Project); }
		}

		public override EnvDTE.Properties Properties
		{
			get
			{
				return new NemerleOAProperties(this.Project.NodeProperties);
			}
		}
	}
}
