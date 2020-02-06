using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Shell;
using System.Runtime.InteropServices;
using System.ComponentModel;

namespace Nemerle.VisualStudio.GUI
{
	[ClassInterface(ClassInterfaceType.AutoDual)]
	[Guid("6A43A835-AD7E-475B-83D1-908AA20C1CC0")]
	public class NemerleOptionsPage : DialogPage
	{
		public NemerleOptionsPage()
		{
			UseSmartTab = true; // true by default
		}

		[Category("Editor")]
		[Description("Use smart behavior of Tab key to provide table formatting of code.")]
		[DefaultValue(true)]
		[DisplayName("Use smart Tab")]
		public bool UseSmartTab { get; set; }
	}
}
