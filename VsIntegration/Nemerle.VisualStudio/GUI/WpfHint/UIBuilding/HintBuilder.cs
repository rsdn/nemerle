using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;
using WpfHint.Parsing;
using System.Linq;

namespace WpfHint.UIBuilding
{
  internal static class HintBuilder
  {
    public static FrameworkElement Build(RootToken rootToken)
    {
      return new HintDecorator { Child = new TextBlock().Fill(rootToken) };
    }

    private static Span Fill(this Span span, ElementToken token)
    {
      Fill(span.Inlines, token);
      return span;
    }

    private static TextBlock Fill(this TextBlock tb, ElementToken token)
    {
      Fill(tb.Inlines, token);
      return tb;
    }

    private static void Fill(InlineCollection inlines, ElementToken token)
    {
      foreach (var t in token.Tokens)
      {
        inlines.Add(GetInline(t));
      }
    }

    private static Inline GetInline(ParseToken token)
    {
      if (token is TextToken)
      {
        var tt = (TextToken)token;
        return new Run(tt.Text);
      }
      if (token is ElementToken)
      {
        var et = (ElementToken)token;
        switch (et.Name)
        {
          case "b":
            return new Bold().Fill(et);
          case "u":
            return new Underline().Fill(et);
          case "i":
            return new Italic().Fill(et);
          case "lb":
            return new LineBreak();
          case "code":
            return new Span { FontFamily = new FontFamily("Consolas"), FontSize = 12.0d * (96d / 72d) }.Fill(et);
          case "font":
            return BuildFont(et);
          case "keyword":
            return new Span { Foreground = Brushes.Blue }.Fill(et);
          case "span":
            return BuildHint(et);
          case "ref":
            return BuildRef(et);
          case "params":
            return BuildParam(et);
          default:
            throw new NotSupportedException(et.Name);
        }
      }
      throw new NotSupportedException();
    }

    private static Inline BuildFont(ElementToken token)
    {
      var span = new Span();

      string size;
      if (token.Attributes.TryGetValue("size", out size))
      {
        var fc = new FontSizeConverter();
        var sz = (double)fc.ConvertFromString(size);
        span.FontSize = sz;
      }

      string face;
      if (token.Attributes.TryGetValue("face", out face))
      {
        span.FontFamily = new FontFamily(face);
      }

      string color;
      if (token.Attributes.TryGetValue("color", out color))
      {
        var bc = new BrushConverter();
        var br = (Brush)bc.ConvertFromString(color);
        span.Foreground = br;
      }
      return span.Fill(token);
    }

    private static Inline BuildHint(ElementToken token)
    {
      string hint;
      token.Attributes.TryGetValue("hint", out hint);
      return hint == null ? new Span().Fill(token) : BuildRef(token);
    }

    private static Inline BuildRef(ElementToken token)
    {
      string handler;
      token.Attributes.TryGetValue("handler", out handler);

      string hint;
      token.Attributes.TryGetValue("hint", out hint);
      if (hint != null)
      {
        hint = "<code>" + hint + "</code>";
      }
      var hc = new HintControl(hint, handler);
      Fill(hc.Inlines, token);
      return new InlineUIContainer { Child = hc };
    }

    private static Inline BuildParam(ElementToken token)
    {
      var pp = new ParamPanel();
      foreach (ParseToken pt in token.Tokens)
      {
        var et = pt as ElementToken;
        if (et != null)
        {
          var tb = new TextBlock().Fill(et);
          pp.Children.Add(tb);
          if (et.Name == "pname")
            ParamPanel.SetNameColumn(tb, true);

          continue;
        }

        var tt = pt as TextToken;
        if (tt != null && pp.Children.Count > 0)
        {
          var elem = pp.Children[pp.Children.Count - 1] as TextBlock;
          if (elem != null)
            elem.Inlines.Add(GetInline(tt));

          continue;
        }
      }

      var span = new Span();
      span.Inlines.Add(new SoftBreak());
      span.Inlines.Add(new InlineUIContainer
                       {
                         Child = pp,
                         BaselineAlignment = BaselineAlignment.Bottom
                       });
      span.Inlines.Add(new SoftBreak());
      return span;
    }
  }
}