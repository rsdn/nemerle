using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Xml.Linq;

namespace WpfHint.Parsing
{
  public class ParamParser
  {
    const string Pat = @"\((?:\s*(?<name>\w(?:\w|\d|_)*\s*)(?<type>:\s*\w+\s*,))*(?<name>\s*\w+\s*)(?<type>:\s*\w+\s*)\)";
    //const string pat = @"\((?:(?<name>\s*\s*\w+\s*)(?<type>:\s*\w+\s*,))*(?<name>\s*\w+\s*)(?<type>:\s*\w+\s*)\)";
    private static readonly Regex r = new Regex(Pat);

    public static void Parse(RootToken rootToken)
    {
      var text = rootToken.Text;
      var matches = r.Matches(text);
      if (matches.Count == 0) return;

      foreach (Match m in matches)
      {
        var i1 = m.Index + 1;
        var i2 = m.Index + m.Length - 1;

        var names = m.Groups["name"];
        var types = m.Groups["type"];
        var n = names.Captures.Count;

        var param = FindParamToken(rootToken, i1, i2);
        var el = InsertElement(param, i1, i2, "params");
        for (int i = 0; i < n; i++)
        {
          var name = names.Captures[i];
          var j1 = name.Index;
          var j2 = name.Index + name.Length;
          InsertElement(el, j1, j2, "pname");

          var type = types.Captures[i];
          j1 = type.Index;
          j2 = type.Index + type.Length;
          InsertElement(el, j1, j2, "ptype");
        }
      }
    }

    static ElementToken FindParamToken(ElementToken rootToken, int i1, int i2)
    {
      foreach (var child in rootToken.Tokens)
      {
        var r = child;
        var a1 = r.Contains(i1);
        var a2 = r.Contains(i2);

        if (a1 && a2) // both
        {
          if (child is TextToken) return rootToken;
          return FindParamToken((ElementToken)child, i1, i2);
        }

        if (a1 || a2) return rootToken;
      }
      return null;
    }

    static ElementToken InsertElement(ElementToken token, int i1, int i2, string name)
    {
      var newToken = Split(token, i1);
      newToken.Name = name;
      token.Tokens.AddLast(newToken);

      var tmp = SplitBack(newToken, i2);
      foreach (var t in tmp.Tokens)
      {
        token.Tokens.AddLast(t);
      }

      return newToken;
    }

    static ElementToken Split(ElementToken token, int index)
    {
      for (var node = token.Tokens.First; ; node = node.Next)
      {
        var r = node.Value;

        if (index == r.StartIndex) return token.Split(node, false);
        if (index < r.EndIndex) return SplitNode(token, node, index, false);
      }
    }

    static ElementToken SplitBack(ElementToken token, int index)
    {
      for (var node = token.Tokens.Last; ; node = node.Previous)
      {
        var r = node.Value;

        if (index == r.EndIndex) return token.Split(node, true);
        if (index > r.StartIndex) return SplitNode(token, node, index, true);
      }
    }

    static ElementToken SplitNode(ElementToken token, LinkedListNode<ParseToken> node, int index, bool back)
    {
      var curToken = node.Value;

      ParseToken newToken = null;
      if (curToken is TextToken)
      {
        // split text
        newToken = ((TextToken)curToken).Split(index);
      }
      else if (curToken is ElementToken)
      {
        newToken = back
                       ? SplitBack((ElementToken)curToken, index)
                       : Split((ElementToken)curToken, index);
      }

      // add new node after curNode
      token.Tokens.AddAfter(node, newToken);

      // split by node
      return token.Split(node, true);
    }
  }
}