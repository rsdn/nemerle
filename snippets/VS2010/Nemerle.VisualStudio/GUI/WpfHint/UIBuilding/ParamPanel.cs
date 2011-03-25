using System;
using System.Windows;
using System.Windows.Controls;

namespace WpfHint.UIBuilding
{
  internal class ParamPanel : Panel
  {
    public static bool GetNameColumn(DependencyObject obj)
    {
      return (bool) obj.GetValue(NameColumnProperty);
    }

    public static void SetNameColumn(DependencyObject obj, bool value)
    {
      obj.SetValue(NameColumnProperty, value);
    }

    public static readonly DependencyProperty NameColumnProperty =
      DependencyProperty.RegisterAttached(
        "NameColumn", typeof (bool), typeof (ParamPanel),
        new FrameworkPropertyMetadata(false));


    public static readonly DependencyProperty WrapProperty =
      HintDecorator.WrapProperty.AddOwner(
        typeof (ParamPanel),
        new FrameworkPropertyMetadata((d, e) => ((UIElement) d).InvalidateMeasure()));

    #region Layout

    private const double tabSpace = 20;
    private double nameColumnWidth;

    protected override Size MeasureOverride(Size availableSize)
    {
      var panelSize = new Size();

      if (!HintDecorator.GetWrap(this))
      {
        foreach (UIElement child in base.InternalChildren)
        {
          child.Measure(availableSize);
          var desiredSize = child.DesiredSize;

          panelSize.Width += desiredSize.Width;
          panelSize.Height = Math.Max(panelSize.Height, desiredSize.Height);
        }
      }
      else
      {
        nameColumnWidth = 0.0;
        var typeColumnWidth = 0.0;
        var rowHeight = 0.0;

        foreach (UIElement child in base.InternalChildren)
        {
          child.Measure(availableSize);
          var ds = child.DesiredSize;

          if (GetNameColumn(child))
          {
            panelSize.Height += rowHeight;

            // tab sapce
            ds.Width += tabSpace;

            if (ds.Width > nameColumnWidth) nameColumnWidth = ds.Width;
            rowHeight = ds.Height;
          }
          else
          {
            if (ds.Width > typeColumnWidth) typeColumnWidth = ds.Width;
            rowHeight = Math.Max(rowHeight, ds.Height);
          }
        }

        panelSize.Height += rowHeight;
        panelSize.Width = nameColumnWidth + typeColumnWidth;
      }
      return panelSize;
    }

    protected override Size ArrangeOverride(Size finalSize)
    {
      if (!HintDecorator.GetWrap(this))
      {
        var pt = new Point();
        foreach (UIElement child in base.InternalChildren)
        {
          var size = child.DesiredSize;
          child.Arrange(new Rect(pt, size));
          pt.X += size.Width;
        }
      }
      else
      {
        var ht = 0.0;
        var y = 0.0;

        foreach (UIElement child in base.InternalChildren)
        {
          var size = child.DesiredSize;

          if (GetNameColumn(child))
          {
            child.Arrange(new Rect(tabSpace, y, nameColumnWidth - tabSpace, size.Height));
            ht = size.Height;
          }
          else
          {
            child.Arrange(new Rect(nameColumnWidth, y, size.Width, size.Height));
            y += Math.Max(ht, size.Height);
          }
        }
      }
      return finalSize;
    }

    #endregion
  }
}