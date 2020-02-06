using System;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Enables the Any CPU Platform form name for Nemerle Projects
	/// </summary>
	[ComVisible(true), CLSCompliant(false)]
	public class NemerleConfigProvider : ConfigProvider
	{
		#region ctors

		public NemerleConfigProvider(ProjectNode manager)
			: base(manager)
		{
		}

		#endregion

		#region Overridden Methods

		protected override ProjectConfig CreateProjectConfiguration(string canonicalName)
		{
			return new NemerleProjectConfig(ProjectMgr, canonicalName);
		}

		#endregion
	}
}
