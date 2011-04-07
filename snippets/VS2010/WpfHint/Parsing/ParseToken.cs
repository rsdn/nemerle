using System.Collections.Generic;
using System.Text;
using System.Xml.Linq;

namespace WpfHint.Parsing
{
	public abstract class ParseToken
	{
		protected RangeList _rangeList;

		protected ParseToken(RangeList rangeList)
		{
			_rangeList = rangeList;
			Tokens = new LinkedList<ParseToken>();
		}

		public virtual LinkedList<ParseToken> Tokens { get; protected set; }

		public abstract int StartIndex { get; }
		public abstract int Length { get; }
		public int EndIndex { get { return StartIndex + Length; } }

		public bool Contains(int index)
		{
			return index >= StartIndex && index < EndIndex;
		}

		public string Text { get { return _rangeList.GetText(StartIndex, Length); } }

		public override string ToString()
		{
			return Text;
		}
	}
}