using System;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.OLE.Interop;

namespace Nemerle.VisualStudio
{
	class NemerleNativeMethods
	{
		[DllImport("user32.dll")]
		public static extern bool ClientToScreen(IntPtr hWnd, POINT[] lpPoint);
		[DllImport("user32.dll")]
		public static extern bool ClientToScreen(IntPtr hWnd, ref POINT lpPoint);
	}
}
