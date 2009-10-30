using System.Text;

namespace WpfHint.Parsing
{
    public class Range
    {
        public int StartIndex { get; set; }
        public int Length { get; set; }
        public int EndIndex { get { return StartIndex + Length; } }


        public override string ToString()
        {
            return "[" + StartIndex + "-" + EndIndex + "]";
        }
    }
}