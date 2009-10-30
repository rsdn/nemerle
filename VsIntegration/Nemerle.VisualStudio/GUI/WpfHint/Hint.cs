using System;
using System.Windows;
using System.Windows.Interop;

namespace WpfHint
{
  public class Hint
  {
    private HintWindow _hintWindow;
    private HintSource _hintSource;
    private string _text;
    private double _wrapWidth = 400.0;

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
        if (_hintWindow == null) return;
        foreach (Window window in _hintWindow.OwnedWindows) window.Close();
        _hintWindow.Text = _text;
      }
    }

    public bool IsOpen { get { return _hintWindow != null; } }

    public void Close()
    {
      if (_hintWindow != null)
        _hintWindow.Close();
    }

    public void Show(IntPtr owner, Rect placementRect, string text)
    {
      PlacementRect = placementRect;
      Text          = text;
      Show(owner);
    }

    public void Show(IntPtr owner)
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

    internal void RaiseClick(string handler)
    {
      if (Click != null) Click(this, handler);
    }
  }
}
