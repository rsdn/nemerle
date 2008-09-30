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

		public override int GetPlatformNames(uint celt, string[] names, uint[] actual)
		{
			if (names != null)
				names[0] = "Any CPU";

			if (actual != null)
				actual[0] = 1;

			return VSConstants.S_OK;
		}

		public override int GetSupportedPlatformNames(uint celt, string[] names, uint[] actual)
		{
			if (names != null)
				names[0] = "Any CPU";

			if (actual != null)
				actual[0] = 1;

			return VSConstants.S_OK;
		}

		protected override ProjectConfig CreateProjectConfiguration(string configName)
		{
			return new NemerleProjectConfig(ProjectMgr, configName);
		}

		#endregion
	}
}
