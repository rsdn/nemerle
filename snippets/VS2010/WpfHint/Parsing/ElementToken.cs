using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace WpfHint.Parsing
{
  public class ElementToken : ParseToken
  {
    private readonly int _startIndex;

    public ElementToken(int index, RangeList rangeList)
      : base(rangeList)
    {
      _startIndex = index;
      _rangeList = rangeList;

      Tokens = new LinkedList<ParseToken>();
      Attributes = new Dictionary<string, string>();
    }

    public override int StartIndex
    {
      get
      {
        return Tokens.Count == 0 ? _startIndex : Tokens.First.Value.StartIndex;
      }
    }

    public override int Length
    {
      get { return Tokens.Count == 0 ? 0 : Tokens.Last.Value.EndIndex - StartIndex; }
    }

    public string Name { get; set; }
    public Dictionary<string, string> Attributes { get; private set; }
    public override LinkedList<ParseToken> Tokens
    {
      get { return base.Tokens; }
      protected set { base.Tokens = value; }
    }


    public override string ToString()
    {
			var attrs = Attributes.Count <= 0 ? "" : Attributes.Aggregate(" ", 
				(acc, e) => acc + " " + e.Key + "='" + e.Value + "'");
      return
					"<" + Name + attrs + ">" 
						+ Tokens.Aggregate("", (a, o) => a + o) 
				+ "</" + Name + ">";
    }


    public ElementToken Split(LinkedListNode<ParseToken> splitNode, bool after)
    {
      var token = splitNode.Value;
      var index = after ? token.EndIndex : token.StartIndex;

      var newToken = new ElementToken(index, _rangeList) { Name = Name };
      newToken.Attributes = this.Attributes;

      // split nodes
      var cur = after ? splitNode.Next : splitNode;
      while (cur != null)
      {
        var tmp = cur.Next;
        Tokens.Remove(cur);
        newToken.Tokens.AddLast(cur);
        cur = tmp;
      }
      return newToken;
    }
  }
}