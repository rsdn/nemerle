using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;
using WpfHint.Parsing;
using System.Linq;
using System.Diagnostics;

namespace WpfHint.UIBuilding
{
  internal static class HintBuilder
  {
    public static FrameworkElement Build(RootToken rootToken, Hint hint)
    {
      var textBlock = new TextBlock().Fill(rootToken, hint);
      textBlock.TextWrapping = TextWrapping.WrapWithOverflow;
      return new HintDecorator { Child = textBlock };
    }

		private static Span Fill(this Span span, ElementToken token, Hint hint)
    {
			Fill(span.Inlines, token, hint);
      return span;
    }

		private static TextBlock Fill(this TextBlock tb, ElementToken token, Hint hint)
    {
			Fill(tb.Inlines, token, hint);
      return tb;
    }

		private static void Fill(InlineCollection inlines, ElementToken token, Hint hint)
    {
      foreach (var t in token.Tokens)
      {
				inlines.Add(GetInline(t, hint));
      }
    }

		private static Inline GetInline(ParseToken token, Hint hint)
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
						return new Bold().Fill(et, hint);
          case "u":
						return new Underline().Fill(et, hint);
          case "i":
						return new Italic().Fill(et, hint);
          case "lb":
            return new LineBreak();
          case "code":
						return new Span { FontFamily = new FontFamily("Consolas"), FontSize = 12.0d * (96d / 72d) }.Fill(et, hint);
          case "font":
						return BuildFont(et, hint);
          case "keyword":
						return new Span { Foreground = Brushes.Blue }.Fill(et, hint);
					//case "span":
					case "hint":
						return BuildHint(et, hint);
          case "ref":
						return BuildRef(et, hint);
          case "params":
						return BuildParam(et, hint);
          default:
            throw new NotSupportedException(et.Name);
        }
      }
      throw new NotSupportedException();
    }

		private static Inline BuildFont(ElementToken token, Hint hint)
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
			return span.Fill(token, hint);
    }

		private static Inline BuildHint(ElementToken token, Hint hint)
    {
			Trace.Assert(token.Name == "hint");

			string handler;
			token.Attributes.TryGetValue("handler", out handler);

			string key;
			token.Attributes.TryGetValue("key", out key);

			string value = token.Attributes["value"];

			var hc = key == null ? new HintControl(token.ToString(), handler)
													 : new HintControl(key, hint.RaiseGetHintContent, handler);
			//Fill(hc.Inlines, token);
			hc.Inlines.Add(new Run(value));
			return new InlineUIContainer { Child = hc };
		}

		private static Inline BuildRef(ElementToken token, Hint hint)
    {
      string handler;
      token.Attributes.TryGetValue("handler", out handler);

      string hintStr;
      token.Attributes.TryGetValue("hint", out hintStr);
      if (hintStr != null)
      {
        hintStr = "<code>" + hintStr + "</code>";
      }
      var hc = new HintControl(hintStr, handler);
			Fill(hc.Inlines, token, hint);
      return new InlineUIContainer { Child = hc };
    }

		private static Inline BuildParam(ElementToken token, Hint hint)
    {
      var pp = new ParamPanel();
      foreach (ParseToken pt in token.Tokens)
      {
        var et = pt as ElementToken;
        if (et != null)
        {
					var tb = new TextBlock().Fill(et, hint);
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
						elem.Inlines.Add(GetInline(tt, hint));

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