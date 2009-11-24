using System;
using System.Windows;
using System.Windows.Interop;
using System.Diagnostics;

namespace WpfHint
{
	public class Hint
	{
		private HintWindow _hintWindow;
		private HintSource _hintSource;
		private string _text;
		private double _wrapWidth = 1200.0;
		private Func<string, string> _getHintContent;

		public event Action<Hint, string> Click;
		public event Action<Hint> Closed;

		/// <summary>
		/// Deafult = 400.0
		/// </summary>      
		public double WrapWidth
		{
			get { return _wrapWidth; }
			set
			{
				_wrapWidth = value;

				if (_hintWindow != null)
					_hintWindow.WrapWidth = value;
			}
		}

		/// <summary>
		/// Placment rect, relative to the screen
		/// (in screen coord)
		/// </summary>
		public Rect PlacementRect { get; set; }

		/// <summary>
		/// Hint text, could be change any time
		/// </summary>
		public string Text
		{
			get { return _text; }
			set
			{
				_text = value;
				
				if (_hintWindow == null)
					return;

				foreach (Window window in _hintWindow.OwnedWindows)
					window.Close();

				_hintWindow.Text = _text;
			}
		}

		public bool IsOpen { get { return _hintWindow != null; } }

		public void Close()
		{
			if (_hintWindow != null)
				_hintWindow.Close();
		}

		public void Show(IntPtr owner, Rect placementRect, Func<string, string> getHintContent, string text)
		{
			PlacementRect   = placementRect;
			Text            = text;
			_getHintContent = getHintContent;

			try
			{
				Show(owner);
			}
			catch
			{
			}
		}

		private void Show(IntPtr owner)
		{
			if (_hintWindow != null)
				throw new NotSupportedException("Hint already shown");

			// subclass
			_hintSource = new HintSource();
			_hintSource.Activate += Close;
			_hintSource.SubClass(owner);

			// create hint window
			var ht = HintRoot.Create(PlacementRect, _hintSource);
			_hintWindow = new HintWindow(this, ht) { Text = _text };
			new WindowInteropHelper(_hintWindow) { Owner = _hintSource.Owner };
			_hintWindow.Closed += HintWindowClosed;
			_hintWindow.MaxHeight = 1200.0;//System.Windows.Forms.Screen.FromRectangle(PlacementRect).WorkingArea.
      _wrapWidth = 1200.0;

			_hintWindow.WrapWidth = _wrapWidth;
			_hintWindow.Show();
		}

		void HintWindowClosed(object sender, EventArgs e)
		{
			_hintSource.UnSubClass();
			_hintSource = null;
			_hintWindow = null;
			if (Closed != null) Closed(this);
		}

		internal string RaiseGetHintContent(string key)
		{
			try
			{
				if (_getHintContent != null)
					return _getHintContent(key);
			}
			catch (Exception ex)
			{
				Trace.WriteLine(ex);
				return "<hint><font color=\"Red\"><b>Exception throw when do hint text lookup:</b></font><lb/>" + ex.Message + "</hint>";
			}

			return key;
		}
		
		internal void RaiseClick(string handler)
		{
			if (Click != null)
				Click(this, handler);
		}
	}
}
