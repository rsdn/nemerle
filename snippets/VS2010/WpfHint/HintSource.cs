using System;
using System.Diagnostics;

namespace WpfHint
{
	internal class HintSource : IDisposable
	{
		private Win32.Callback _ownerWndProc;
		private Win32.Callback _rootWndProc;

		private IntPtr _oldRoot;
		private IntPtr _oldOwner;

		private IntPtr _owner;   // text editor window handle
		private IntPtr _root;    // main window handle (visual studio)

		public event Action Activate;
		public event Action MouseMove;
		public event Action MouseLeave;

		public IntPtr Owner { get { return _owner; } }

		public void SubClass(IntPtr owner)
		{
			Debug.WriteLine("SubClass(): " + owner);

			var pt = Win32.GetCursorPos();
			var pt1 = new Win32.POINT(pt.X, pt.Y);

			_owner = owner == IntPtr.Zero ? Win32.WindowFromPoint(pt1) : owner;

			if (_owner == IntPtr.Zero)
				throw new NullReferenceException("Can't find owner");

			_root = Win32.GetAncestor(_owner, Win32.GA_ROOT);
			if (_root == IntPtr.Zero)
				throw new NullReferenceException("Can't find root");

			_ownerWndProc = WndProc;
								_oldOwner = Win32.SetWindowProc(_owner, _ownerWndProc);

			if (_oldOwner == IntPtr.Zero)
				throw new InvalidOperationException("Failed subclass");

			_rootWndProc = RootWndProc;
								_oldRoot = Win32.SetWindowProc(_root, _rootWndProc);

			if (_oldRoot == IntPtr.Zero)
				throw new InvalidOperationException("Failed subclass");
		}

		public void UnSubClass()
		{
			Debug.WriteLine("UnSubClass(): " + _owner);
			if (_owner != IntPtr.Zero && _oldOwner != IntPtr.Zero)
			{
				Win32.SetWindowProc(_owner, _oldOwner);
				_oldOwner = IntPtr.Zero;
			}

			if (_root != IntPtr.Zero && _oldRoot != IntPtr.Zero)
			{
				Win32.SetWindowProc(_root, _oldRoot);
				_oldRoot = IntPtr.Zero;
			}
		}

		private int WndProc(IntPtr hwnd, int msg, int wParam, int lParam)
		{
			// We mast pracess window messages before our logic will be done.
			// Otherwise UnSubClass() will be remove (hide) current messege.
			int result = Win32.CallWindowProc(_oldOwner, hwnd, msg, wParam, lParam);

			if ((msg == Win32.WM_KEYDOWN || msg == Win32.WM_MOUSEWHEEL) && Activate != null)
				Activate();

			if ((msg == Win32.WM_LBUTTONDOWN || msg == Win32.WM_RBUTTONDOWN) && Activate != null)
				Activate();

			if (msg == Win32.WM_MOUSEMOVE && MouseMove != null)
				MouseMove();

			if (msg == Win32.WM_MOUSELEAVE && MouseLeave != null)
				MouseLeave();

			return result;
		}

		private int RootWndProc(IntPtr hwnd, int msg, int wParam, int lParam)
		{
			// We mast pracess window messages before our logic will be done.
			// Otherwise UnSubClass() will be remove (hide) current messege.
			int result = Win32.CallWindowProc(_oldRoot, hwnd, msg, wParam, lParam);

			if ((msg == Win32.WM_ACTIVATE || msg == Win32.WM_MOVE || msg == Win32.WM_ACTIVATEAPP) && 
				Activate != null)
			{
				Activate();
			}
			return result;
		}

		#region Implementation of IDisposable


		~HintSource()
		{
			Dispose();
		}

		public void Dispose()
		{
			UnSubClass();
		}

		#endregion
	}
}
