using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Xml.Linq;

namespace WpfHint.Parsing
{
	public class RootToken : ElementToken
	{
		public RootToken(XElement root, RangeList rangeList)
			: base(0, rangeList)
		{
			ParseElement(this, root, 0, _rangeList);
		}

		public override LinkedList<ParseToken> Tokens
		{
			get { return base.Tokens; }
			protected set { base.Tokens = value; }
		}

		static void ParseElement(ElementToken en, XElement el, int index, RangeList list)
		{
			// parse name & attributes
			en.Name = el.Name.ToString();
			foreach (var attr in el.Attributes())
			{
				en.Attributes[attr.Name.ToString()] = attr.Value;
			}

			// parse nodes
			foreach (var node in el.Nodes())
			{
				var pn = Create(node, index, list);
				en.Tokens.AddLast(pn);
				index += pn.Length;
			}
		}

		static ParseToken Create(XNode node, int index, RangeList list)
		{
			if (node is XText)
			{
				var len = ((XText)node).Value.Length;
				return new TextToken(index, len, list);
			}
			if (node is XElement)
			{
				var el = (XElement)node;
				var en = new ElementToken(index, list);
				ParseElement(en, el, index, list);
				return en;
			}
			throw new NotSupportedException(node.NodeType.ToString());
		}
	}
}