using System;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Security;

namespace Nemerle.VisualStudio.GUI.Native
{
	[SuppressUnmanagedCodeSecurity]
	internal static class NativeMethods
	{
		public const int LVM_GETHEADER = 0x1000 + 0x1F;

		#region RECT

		[StructLayout(LayoutKind.Sequential)]
		public struct RECT
		{
			public int left;
			public int top;
			public int right;
			public int bottom;

			public RECT(int left, int top, int right, int bottom)
			{
				this.left = left;
				this.top = top;
				this.right = right;
				this.bottom = bottom;
			}

			public RECT(Rectangle r)
			{
				left = r.Left;
				top = r.Top;
				right = r.Right;
				bottom = r.Bottom;
			}

			public static RECT FromXYWH(int x, int y, int width, int height)
			{
				return new RECT(x, y, x + width, y + height);
			}

			public int Width
			{
				get { return right - left; }
			}

			public int Height
			{
				get { return bottom - top; }
			}

			public Size Size
			{
				get { return new Size(Width, Height); }
			}

			public static implicit operator Rectangle(RECT r)
			{
				return new Rectangle(r.left, r.top, r.Width, r.Height);
			}

			public static implicit operator RECT(Rectangle r)
			{
				return new RECT(r.Left, r.Top, r.Right, r.Bottom);
			}
		}

		#endregion

		[DllImport("user32.dll", CharSet = CharSet.Auto, ExactSpelling = true)]
		public static extern bool GetWindowRect(HandleRef hWnd, [In, Out] ref RECT rect);
	}
}

