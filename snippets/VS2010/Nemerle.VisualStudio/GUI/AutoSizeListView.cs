using System;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using Nemerle.VisualStudio.GUI.Native;

namespace Nemerle.VisualStudio.GUI
{
	public class AutoSizeListView : ListView
	{
		public Rectangle HeaderRect
		{
			get
			{
				NativeMethods.RECT r = new NativeMethods.RECT();
				Message            m = Message.Create(Handle,
					NativeMethods.LVM_GETHEADER, IntPtr.Zero, IntPtr.Zero);

				DefWndProc(ref m);
				if (NativeMethods.GetWindowRect(new HandleRef(this, m.Result), ref r))
					return r;

				return Rectangle.Empty;
			}
		}

		// This method is not imlemented by winforms for no good reason.
		//
		public override Size GetPreferredSize(Size _)
		{	
			int width = SystemInformation.VerticalScrollBarWidth + SystemInformation.Border3DSize.Width;
			foreach (ColumnHeader hdr in Columns)
				width += hdr.Width;
			
			int height = View == View.Details? HeaderRect.Height: 0;
			if (Items.Count > 0)
				height += Items.Count * GetItemRect(0).Height;

			return new Size(width, height);
		}
	}
}
