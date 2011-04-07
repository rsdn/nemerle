using System.Collections.Generic;
using System.Text;
using System.Xml.Linq;

namespace WpfHint.Parsing
{
	public class TextToken : ParseToken
	{
		private TextToken(RangeList rangeList)
			: base(rangeList)
		{
		}

		public TextToken(int index, int len, RangeList rangeList)
			: base(rangeList)
		{
			Range = _rangeList.AddRange(index, len);
		}

		public Range Range { get; private set; }

		public override int StartIndex { get { return Range.StartIndex; } }

		public override int Length { get { return Range.Length; } }

		public TextToken Split(int index)
		{
			return new TextToken(_rangeList) { Range = _rangeList.SplitRange(Range, index) };
		}

		private static string HtmlMangling(string str)
		{
			return str.Replace("&", "&amp;").Replace(">", "&gt;").Replace("<", "&lt;");
		}

		public override string ToString()
		{
			return HtmlMangling(Text);
		}
	}
}