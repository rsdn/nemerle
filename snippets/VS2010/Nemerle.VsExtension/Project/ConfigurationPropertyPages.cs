using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing.Design;
using System.Runtime.InteropServices;
using System.Windows.Forms.Design;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Enumerated list of the properties shown on the build property page
	/// </summary>
	internal enum NemerleBuildPropertyPageTag
	{
		DefineConstants,
	}

	/// <summary>
	/// Enumerated list of the properties shown on the debug property page
	/// </summary>
	internal enum DebugPropertyPageTag
	{
		StartProgram,
		WorkingDirectory,
		CmdArgs,
	}

}
