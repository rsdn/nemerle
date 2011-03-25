using System;
using System.Windows;
using System.Windows.Input;

namespace WpfHint
{
	internal abstract class HintRoot : IDisposable
	{
		public event Action MouseLeave;
		public Rect ActiveRect { get; protected set; }

		public abstract bool IsMouseOver { get; }
		protected void RaiseEvent()
		{
			if (MouseLeave != null) MouseLeave();
		}

		#region IDisposable Members

		public virtual void Dispose() { }

		#endregion

		public static HintRoot Create(FrameworkElement fe)
		{
			return new HintRootWpf(fe);
		}

		public static HintRoot Create(Rect rect, HintSource source)
		{
			return new HintRootWin32(rect, source);
		}

		private class HintRootWpf : HintRoot
		{
			private readonly FrameworkElement _fe;

			public HintRootWpf(FrameworkElement fe)
			{
				_fe = fe;
				var pt = fe.PointToScreen(new Point());
				var pt2 = fe.PointToScreen(new Point(fe.ActualWidth, fe.ActualHeight));
				ActiveRect = new Rect(pt.X, pt.Y, pt2.X - pt.X, pt2.Y - pt.Y);

				fe.MouseLeave += OnMouse;
			}

			void OnMouse(object sender, MouseEventArgs e)
			{
				RaiseEvent();
			}

			public override bool IsMouseOver
			{
				get { return _fe.IsMouseOver; }
			}

			public override void Dispose()
			{
				_fe.MouseLeave -= OnMouse;
			}

			#region Equality

			public bool Equals(HintRootWpf other)
			{
				if (ReferenceEquals(null, other))
					return false;
				
				if (ReferenceEquals(this, other))
					return true;

				return Equals(other._fe, _fe);
			}

			public override bool Equals(object obj)
			{
				if (ReferenceEquals(null, obj))
					return false;

				if (ReferenceEquals(this, obj))
					return true;

				if (obj.GetType() != typeof(HintRootWpf))
					return false;

				return Equals((HintRootWpf)obj);
			}

			public override int GetHashCode()
			{
				return (_fe != null ? _fe.GetHashCode() : 0);
			}

			public static bool operator ==(HintRootWpf left, HintRootWpf right)
			{
				return Equals(left, right);
			}

			public static bool operator !=(HintRootWpf left, HintRootWpf right)
			{
				return !Equals(left, right);
			}

			#endregion
		}

		private class HintRootWin32 : HintRoot
		{
			private readonly HintSource source;

			public HintRootWin32(Rect rect, HintSource source)
			{
				this.source = source;
				ActiveRect = rect;
				source.MouseMove += OnMouse;
				source.MouseLeave += OnMouse;
			}

			private void OnMouse()
			{
				if (!IsMouseOver) RaiseEvent();
			}

			public override bool IsMouseOver
			{
				get { return ActiveRect.Contains(Win32.GetCursorPos()); }
			}

			public override void Dispose()
			{
				source.MouseMove -= OnMouse;
				source.MouseLeave -= OnMouse;
			}
		}
	}
}