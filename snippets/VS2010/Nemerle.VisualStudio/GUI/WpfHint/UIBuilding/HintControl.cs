using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System;

namespace WpfHint.UIBuilding
{
	internal class HintControl : TextBlock
	{
		#region ClickEvent

		public static readonly RoutedEvent ClickEvent =
				EventManager.RegisterRoutedEvent(
						"Click",
						RoutingStrategy.Bubble,
						typeof(RoutedEventHandler),
						typeof(HintControl));

		public static void AddClickHandler(DependencyObject d, RoutedEventHandler handler)
		{
			var element = d as UIElement;
			if (element != null)
				element.AddHandler(ClickEvent, handler);
		}


		public static void RemoveClickHandler(DependencyObject d, RoutedEventHandler handler)
		{
			var element = d as UIElement;

			if (element != null)
				element.RemoveHandler(ClickEvent, handler);
		}

		#endregion

		#region MouseHover

		public static readonly RoutedEvent MouseHoverEvent =
				EventManager.RegisterRoutedEvent(
						"MouseHover",
						RoutingStrategy.Bubble,
						typeof(RoutedEventHandler),
						typeof(HintControl));

		public static void AddMouseHoverHandler(DependencyObject d, RoutedEventHandler handler)
		{
			var element = d as UIElement;
			if (element != null) element.AddHandler(MouseHoverEvent, handler);
		}

		public static void RemoveMouseHoverHandler(DependencyObject d, RoutedEventHandler handler)
		{
			var element = d as UIElement;
			if (element != null) element.RemoveHandler(MouseHoverEvent, handler);
		}

		#endregion

		private readonly Span span;
		public string Handler { get; private set; }
		private string _hintKey;
		public string Hint
		{
			get { return HintTextLookup(_hintKey); }
		}
		public new InlineCollection Inlines { get { return span.Inlines; } }
		private Func<string, string> HintTextLookup;

		public HintControl(string hintKey, Func<string, string> hintTextLookup, string handler)
		{
			HintTextLookup = hintTextLookup;
			_hintKey       = hintKey;
			Handler        = handler;

			MouseEnter += delegate { RaiseEvent(new RoutedEventArgs(MouseHoverEvent)); };

			if (handler != null)
			{
				var hLink = new Hyperlink { Focusable = false };
				hLink.Click += 
					delegate
					{
						RaiseEvent(new RoutedEventArgs(ClickEvent));
					};
				span = hLink;
			}
			else
			{
				span = new Span();
			}
			base.Inlines.Add(span);
		}

		public HintControl(string hintText, string handler) : this(hintText, x => x, handler) { }
	}
}