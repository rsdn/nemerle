using System;
using Microsoft.VisualStudio.Package.Automation;
using Microsoft.VisualStudio.Package;
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
	}
}
