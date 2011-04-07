using System;
using System.Linq;
using System.Windows;
using System.Windows.Controls;

namespace WpfHint.UIBuilding
{
	internal class HintDecorator : Decorator
	{
		public double WrapWidth
		{
			get { return (double)GetValue(WrapWidthProperty); }
			set { SetValue(WrapWidthProperty, value); }
		}

		public static readonly DependencyProperty WrapWidthProperty =
				DependencyProperty.Register(
						"WrapWidth",
						typeof(double),
						typeof(HintDecorator),
						new FrameworkPropertyMetadata
						{
							DefaultValue = 400.0,
							PropertyChangedCallback = (d, e) => ((UIElement)d).InvalidateMeasure(),
							AffectsArrange = true,
							AffectsMeasure = true
						});


		public static bool GetWrap(DependencyObject obj)
		{
			return (bool)obj.GetValue(WrapProperty);
		}

		public static void SetWrap(DependencyObject obj, bool value)
		{
			obj.SetValue(WrapProperty, value);
		}

		public static readonly DependencyProperty WrapProperty =
				DependencyProperty.RegisterAttached(
						"Wrap",
						typeof(bool),
						typeof(HintDecorator),
						new FrameworkPropertyMetadata(false));


		private bool _wrap;

		protected override Size MeasureOverride(Size constraint)
		{
			var child = base.Child;
			if (child == null)
				return new Size();

			child.Measure(constraint);
			var sz = child.DesiredSize;

			if (!_wrap && sz.Width > WrapWidth)
			{
				_wrap = true;
				OnWrap(this);

				child.Measure(constraint);
				sz = child.DesiredSize;
			}

			return sz;
		}

		private static void OnWrap(DependencyObject d)
		{
			foreach (var dobj in LogicalTreeHelper.GetChildren(d).OfType<DependencyObject>().ToList())
			{
				SetWrap(dobj, true);
				OnWrap(dobj);
			}
		}
	}
}