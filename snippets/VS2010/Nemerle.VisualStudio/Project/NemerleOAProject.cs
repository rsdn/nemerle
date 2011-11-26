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

		/// <summary>
		/// Returns the name of project as a relative path from the directory containing the solution file to the project file
		/// </summary>
		/// <value>Unique name if project is in a valid state. Otherwise null</value>
		public override string UniqueName
		{
			get
			{
				if (this.Project == null || this.Project.IsClosed)
					return null;
				else
					return Utils.CalcSyncInUIThread(() => base.UniqueName);
			}
		}
	}
}
