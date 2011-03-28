using System;
using System.Diagnostics;
using System.Windows.Input;
using System.Windows;
using System.Windows.Threading;
using System.Linq;

namespace WpfHint
{
	internal class HintSource : IDisposable
	{
		public event Action Activate;
		public HintWindow HintWindow { get; set; }

		/// text editor window handle
		public IntPtr Owner { get; private set; }

		private InputManager _inputManager;
		private Dispatcher   _dispatcher;

		public void SubClass(IntPtr owner)
		{
			Owner = owner;
			_inputManager = InputManager.Current;
			_dispatcher   = _inputManager.Dispatcher;

			_inputManager.PreProcessInput += OnPreProcessInputEventHandler;
			_inputManager.EnterMenuMode   += OnEnterMenuMode;

			//Debug.WriteLine("SubClass(): " + owner);
		}

		void Unsubscribe()
		{
			_inputManager.PreProcessInput -= OnPreProcessInputEventHandler;
			_inputManager.EnterMenuMode   -= OnEnterMenuMode;
		}

		public void UnSubClass()
		{
			if (_dispatcher != null)
				_dispatcher.BeginInvoke((Action)Unsubscribe);

			//Debug.WriteLine("UnSubClass(): " + _owner);
		}

		#region Dispatcher handlers

		bool MouseHoverHintWindow()
		{
			var pos = Win32.GetCursorPos();

			Func<Window, bool> process = null; process = wnd => // local funtion :)
			{
				if (wnd.RestoreBounds.Contains(pos))
					return true;

				foreach (Window win in wnd.OwnedWindows)
					if (process(win))
						return true;

				return false;
			};

			Trace.Assert(HintWindow != null);
			return process(HintWindow);
		}

		void OnPreProcessInputEventHandler(object sender, PreProcessInputEventArgs e)
		{
			var name = e.StagingItem.Input.RoutedEvent.Name;

			switch (name)
			{
				case "PreviewMouseDown":
				case "PreviewMouseUp":
				case "MouseDown":
				case "MouseUp":
					if (!MouseHoverHintWindow())
						CollActivate();
					break;

				case "PreviewKeyDown":
				case "PreviewKeyUp":
				case "KeyDown":
				case "KeyUp":
				case "LostKeyboardFocus":
					CollActivate();
					break;

				case "PreviewInputReport":
				case "InputReport":
				case "QueryCursor":
				case "PreviewMouseMove":
				case "MouseMove":
					break;
				default:
					Debug.WriteLine(name);
					break;
			}
		}

		void CollActivate()
		{
			if (Activate != null)
				Activate();
		}

		void OnEnterMenuMode(object sender, EventArgs e)
		{
			CollActivate();
		}

		#endregion Dispatcher handlers

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
