using System.Collections.Generic;
using System.Text;

namespace WpfHint.Parsing
{
    public class RangeList
    {
        private readonly LinkedList<Range> list;

        public RangeList(string root)
        {
            list = new LinkedList<Range>();
            Chars = root.ToCharArray();
        }

        public char[] Chars { get; private set; }

        public string GetText(int index, int count)
        {
            var sb = new StringBuilder();
            for (var i = index; i < index + count; ++i)
            {
                var c = Chars[i];
                if (c != '\0') sb.Append(c);
            }
            return sb.ToString();
        }

        public Range AddRange(int index, int len)
        {
            var r = new Range { StartIndex = index, Length = len };
            list.AddLast(r);
            return r;
        }

        public bool RemoveRange(Range r)
        {
            return list.Remove(r);
        }

        public Range SplitRange(Range r, int index)
        {
            var range = new Range { StartIndex = index, Length = r.EndIndex - index };
            r.Length = index - r.StartIndex;

            var node = list.Find(r);
            list.AddAfter(node, range);
            return range;
        }
    }
}