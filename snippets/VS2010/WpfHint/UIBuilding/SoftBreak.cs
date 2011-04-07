using System.Windows;
using System.Windows.Documents;

namespace WpfHint.UIBuilding
{
	internal class SoftBreak : Run
	{
		public static readonly DependencyProperty WrapProperty =
			HintDecorator.WrapProperty.AddOwner(
				typeof(SoftBreak),
				new FrameworkPropertyMetadata((d, e) => ((SoftBreak)d).Update()));

		private void Update()
		{
			Text = HintDecorator.GetWrap(this) ? "\r\n" : "";
		}
	}
}