using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using System.Windows.Threading;
using WpfHint.Parsing;
using WpfHint.UIBuilding;
using System.Diagnostics;

namespace WpfHint
{
  /// <summary>
  /// Interaction logic for HintWindow.xaml
  /// </summary>
  internal partial class HintWindow
  {
    private readonly Hint hint;
    private readonly HintRoot hintRoot;
    private readonly DispatcherTimer timer =
        new DispatcherTimer { Interval = TimeSpan.FromMilliseconds(100) };

    private string text;
    public string Text
    {
      get { return text; }
      set
      {
        text = value;

        try
        {
          var root = HintParser.Parse(value);
          var fe = HintBuilder.Build(root, hint);
          border.Child = fe;
        }
        catch (Exception ex)
        {
          Trace.WriteLine(ex);
        }
      }
    }

    public double WrapWidth
    {
      get { return ((HintDecorator)border.Child).WrapWidth; }
      set { ((HintDecorator)border.Child).WrapWidth = value; }
    }

    public HintWindow(Hint hint, HintRoot root)
    {
      InitializeComponent();

      this.hint = hint;
      hintRoot = root;

      // window props
      Left = 0;
      Top = 0;
      SizeToContent = SizeToContent.WidthAndHeight;
      Opacity = 1;

      timer.Tick += OnTimerTick;
      LayoutUpdated += OnLayoutUpdated;
      MouseLeave += OnMouseLeave;
      hintRoot.MouseLeave += TestMouseOver;
      MouseRightButtonUp += MouseButtonEventHandler;

      HintControl.AddClickHandler(this, OnClick);
      HintControl.AddMouseHoverHandler(this, OnMouseHover);
    }

    protected override void OnClosed(EventArgs e)
    {
      hintRoot.MouseLeave -= TestMouseOver;
      MouseLeave -= OnMouseLeave;

      timer.Stop();
      hintRoot.Dispose();
      base.OnClosed(e);
    }

    #region HintControl event handling

    private void MouseButtonEventHandler(object sender, MouseButtonEventArgs e)
    {
      if (e.ChangedButton == MouseButton.Right && e.ButtonState == MouseButtonState.Released)
        Close();
    }

    private void OnClick(object sender, RoutedEventArgs e)
    {
      var hc = e.Source as HintControl;
      
			if (hc == null)
				return;

      if (hc.Handler != null)
				hint.RaiseClick(hc.Handler);
    }

    private void OnMouseHover(object sender, RoutedEventArgs e)
    {
      var hc = e.Source as HintControl;
      if (hc == null)
				return;

      if (hc.Hint != null)
				ShowSubHint(hc, hc.Hint);
    }

    private void ShowSubHint(FrameworkElement el, string hintText)
    {
      var ht = HintRoot.Create(el);

      foreach (HintWindow window in OwnedWindows)
      {
        if (!window.hintRoot.Equals(ht))
					continue;

        ht.Dispose();
        return;
      }

      var wnd = new HintWindow(hint, ht) { Text = hintText, Owner = this };
      wnd.Show();
    }

    #endregion

    #region Mouse leave checking

    void OnMouseLeave(object sender, MouseEventArgs e)
    {
      TestMouseOver();
    }

    void TestMouseOver()
    {
      // restart timer
      timer.Stop();
      timer.Start();
    }

    void OnTimerTick(object sender, EventArgs e)
    {
      timer.Stop();

      if (OwnedWindows.Count > 0) return;
      if (IsMouseOver) return;
      if (hintRoot.IsMouseOver) return;

      this.Close();
      var owner = Owner as HintWindow;
      if (owner != null) owner.TestMouseOver();
    }

    #endregion

    #region Window layout relative to screen

    void OnLayoutUpdated(object sender, EventArgs e)
    {
      var dx = 1.0;
      var dy = 1.0;

      var src = PresentationSource.FromVisual(this);
      if (src != null)
      {
        var m = src.CompositionTarget.TransformToDevice;
        dx = m.M11;
        dy = m.M22;
      }

      var rect = hintRoot.ActiveRect;

      var size = new Size(ActualWidth * dx, ActualHeight * dy);
      var scrSize = new Size(SystemParameters.PrimaryScreenWidth * dx,
                             SystemParameters.PrimaryScreenHeight * dy);

      var pos = rect.BottomLeft;
      if (rect.Bottom + size.Height > scrSize.Height)
      {
        pos.Y = rect.Top - size.Height;
      }

      if (rect.Left + size.Width > scrSize.Width)
      {
        pos.X = scrSize.Width - size.Width;
      }

      // update location
      this.Left = pos.X / dx;
      this.Top = pos.Y / dy;
    }

    #endregion
  }
}
