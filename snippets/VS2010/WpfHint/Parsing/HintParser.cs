using System.Text.RegularExpressions;
using System.Xml.Linq;

namespace WpfHint.Parsing
{
	public class HintParser
	{
		public static RootToken Parse(string text)
		{
			var root = XElement.Parse(text, LoadOptions.PreserveWhitespace);

			var rlist = new RangeList(root.Value);
			var rn = new RootToken(root, rlist);

			//ParamParser.Parse(rn);
			RemoveWhitespaces(rn, rlist);
			RemoveFormating(rn);
			return rn;
		}

		static void RemoveWhitespaces(RootToken rt, RangeList list)
		{
			//var i1 = Clear(rt.StartIndex, rt, list);
			//ClearRange(i1, rt.EndIndex, list);
		}

		static int Clear(int i1, ElementToken root, RangeList list)
		{
			foreach (var node in root.Tokens)
			{
				var i2 = node.StartIndex;
				if (!(node is ElementToken)) continue;

				var el = (ElementToken)node;
				if (el.Name != "pre")
				{
					i1 = Clear(i1, el, list);
				}
				else
				{
					ClearParamsInPre(el, list);
					// clear range
					ClearRange(i1, i2, list);
					i1 = el.EndIndex;
				}
			}
			return i1;
		}

		static void ClearRange(int i1, int i2, RangeList list)
		{
			if (i1 == i2) return;

			var str = list.GetText(i1, i2 - i1);

			// find seq-s of whitespace
			var regex = new Regex("\\s+");
			var ms = regex.Matches(str);

			foreach (Match m in ms)
			{
				var j1 = i1 + m.Index;
				var j2 = j1 + m.Length;
				list.Chars[j1] = ' ';
				for (var i = j1 + 1; i < j2; i++)
				{
					list.Chars[i] = '\0';
				}
			}
		}


		static void ClearParamsInPre(ElementToken token, RangeList list)
		{
			foreach (var node in token.Tokens)
			{
				if (!(node is ElementToken)) continue;

				var el = (ElementToken)node;
				if (el.Name != "params")
				{
					ClearParamsInPre(el, list);
				}
				else
				{
					var i1 = el.StartIndex;
					var i2 = el.EndIndex;
					// clear range
					ClearRange(i1, i2, list);
				}
			}
		}


		/// <summary>
		/// remove pre
		/// </summary>
		static void RemoveFormating(ElementToken root)
		{
			var node = root.Tokens.First;
			while (node != null)
			{
				var item = node.Value;
				if (!(item is ElementToken))
				{
					node = node.Next;
					continue;
				}

				var el = (ElementToken)item;
				RemoveFormating(el);

				if (el.Name == "pre")
				{
					foreach (var subNode in el.Tokens)
					{
						root.Tokens.AddBefore(node, subNode);
					}

					var next = node.Next;
					root.Tokens.Remove(node);
					node = next;
				}
				else
				{
					node = node.Next;
					continue;
				}
			}
		}
	}
}