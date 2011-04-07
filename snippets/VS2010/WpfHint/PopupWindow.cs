using System;
using System.Windows;
using System.Windows.Interop;
using System.Windows.Media;

namespace WpfHint
{
	internal class PopupWindow : Window
	{
		public PopupWindow()
		{
			AllowsTransparency = true;
			WindowStyle = WindowStyle.None;
			Background = Brushes.Transparent;
			FontSize = 12.0d * (96d / 72d);

			Topmost = true;
			ShowActivated = false;
			ShowInTaskbar = false;

			Loaded += InvisibleWindow_Loaded;
		}

		void InvisibleWindow_Loaded(object sender, RoutedEventArgs e)
		{
			var source = HwndSource.FromHwnd(new WindowInteropHelper(this).Handle);
			if (source != null)
				source.AddHook(WndProc);
		}

		// Prevent activation of window
		private static IntPtr WndProc(IntPtr hwnd, int msg, IntPtr wParam, IntPtr lParam, ref bool handled)
		{
			// ReSharper disable InconsistentNaming
			const int WM_MOUSEACTIVATE = 0x0021;
			const int MA_NOACTIVATE = 3;
			//const int MA_NOACTIVATEANDEAT = 4;
			// ReSharper restore InconsistentNaming

			if (msg == WM_MOUSEACTIVATE)
			{
				handled = true;
				return new IntPtr(MA_NOACTIVATE);
			}

			return IntPtr.Zero;
		}
	}
}