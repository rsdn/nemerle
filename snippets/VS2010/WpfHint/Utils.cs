using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;

namespace WpfHint
{
	static class Utils
	{
		public static bool Contains(this Rect rect, Point pt)
		{
			if (pt.X < rect.Left)
				return false;

			if (pt.X > rect.Right - 1)
				return false;

			if (pt.Y < rect.Top)
				return false;

			if (pt.Y > rect.Bottom - 1)
				return false;

			return true;
		}
	}
}
