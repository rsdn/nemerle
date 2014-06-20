using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Text;
using Nemerle.Compiler;
using System.Diagnostics;
using System.Text.RegularExpressions;

namespace Nemerle.VisualStudio.LanguageService
{
  public static class ParseUtil
  {
    private static readonly Regex _singleLineCommentParser = new Regex(@"^//\s*(TODO\s*:)|(BUG\s*:)|(HACK\s*:)", RegexOptions.Compiled | RegexOptions.CultureInvariant | RegexOptions.IgnoreCase);
    private static readonly Regex _multiLineCommentParser  = new Regex(@"^/\*\s*(TODO\s*:)|(BUG\s*:)|(HACK\s*:)", RegexOptions.Compiled | RegexOptions.CultureInvariant | RegexOptions.IgnoreCase);

    public static bool TryParse(ITextBuffer textBuffer, out ClassificationParseResult parseResult)
    {
      var snapshot = textBuffer.CurrentSnapshot;

      ClassificationParseResult lastParseResult;
      if (textBuffer.Properties.TryGetProperty(typeof(ClassificationParseResult), out lastParseResult) && lastParseResult.Snapshot.Version == snapshot.Version)
      {
        parseResult = lastParseResult;
        return true;
      }

      NemerleSource source;
      if (!textBuffer.Properties.TryGetProperty(typeof(NemerleSource), out source))
      {
        parseResult = null;
        return false;
      }

      var engine = source.GetEngine();
      if (!engine.RequestOnInitEngine())
      {
        parseResult = null;
        return false;
      }

      var timer = Stopwatch.StartNew();

      var code = snapshot.GetText();
      var lexer = new LexerFile((ManagerClass)engine, 0, code, true);
      var preParser = new PreParser(lexer);
      var tokens = preParser.PreParse();
      var _comments = lexer.GetComments();
      var directives = lexer.GetDirectives();

      var comments = new Comment[_comments.Length];
      for (var i = 0; i < _comments.Length; ++i)
      {
        var c = _comments[i];
        var type = CommentType.Normal;
        var pos = 0;
        var commentParser = c.IsMultiline ? _multiLineCommentParser : _singleLineCommentParser;
        var match = commentParser.Match(code, c.Position, c.Length);
        if (match.Success)
        {
          if (match.Groups[1].Success)
          {
            pos = match.Groups[1].Index;
            type = CommentType.ToDo;
          }
          else if (match.Groups[2].Success)
          {
            pos = match.Groups[2].Index;
            type = CommentType.Bug;
          }
          else if (match.Groups[3].Success)
          {
            pos = match.Groups[3].Index;
            type = CommentType.Hack;
          }
        }
        comments[i] = new Comment(c, type, pos);
      }

      timer.Stop();
      Debug.Print("SyntaxClassifier.TryParse: {0}", timer.Elapsed);

      parseResult = new ClassificationParseResult(snapshot, tokens, comments, directives);
      textBuffer.Properties[typeof(ClassificationParseResult)] = parseResult;
      return true;
    }

    public static bool TryGetTokenInfo(ITextBuffer textBuffer, int line, int column, out TokenInfo tokenInfo)
    {
      tokenInfo = null;

      ClassificationParseResult parseResult;
      if (!textBuffer.Properties.TryGetProperty(typeof(ClassificationParseResult), out parseResult) || parseResult.Snapshot.Version != textBuffer.CurrentSnapshot.Version)
        return false;

      return SearchToken(parseResult.Tokens, parseResult.Snapshot, new TextPoint(line + 1, column + 1), ref tokenInfo);
    }

    private static bool SearchToken(Token token, ITextSnapshot snapshot, TextPoint point, ref TokenInfo tokenInfo)
    {
      for (; token != null; token = token.Next)
      {
        if (point < token.Location.Begin)
          break;

        if (!token.Location.Contains(point))
          continue;

        if (token is Token.LooseGroup)
        {
          var group = (Token.LooseGroup)token;
          if (SearchToken(group.Child, snapshot, point, ref tokenInfo))
            return true;
        }
        else if (token is Token.BracesGroup)
        {
          var group = (Token.BracesGroup)token;
          if (SearchToken(group.Child, snapshot, point, ref tokenInfo))
            return true;
        }
        else if (token is Token.RoundGroup)
        {
          var group = (Token.RoundGroup)token;
          if (SearchToken(group.Child, snapshot, point, ref tokenInfo))
            return true;
        }
        else if (token is Token.SquareGroup)
        {
          var group = (Token.SquareGroup)token;
          if (SearchToken(group.Child, snapshot, point, ref tokenInfo))
            return true;
        }
        else if (token is Token.QuoteGroup)
        {
          var group = (Token.QuoteGroup)token;
          if (SearchToken(group.Child, snapshot, point, ref tokenInfo))
            return true;
        }
        else if (token is Token.Namespace)
        {
          var ns = (Token.Namespace)token;
          if (SearchToken(ns.KeywordToken, snapshot, point, ref tokenInfo))
            return true;
          if (SearchToken(ns.Body, snapshot, point, ref tokenInfo))
            return true;
        }
        else if (token is Token.Using)
        {
          var ns = (Token.Using)token;
          if (SearchToken(ns.KeywordToken, snapshot, point, ref tokenInfo))
            return true;
          if (SearchToken(ns.Body, snapshot, point, ref tokenInfo))
            return true;
        }
        else if (token is Token.Keyword)
        {
          var span = Utils.NLocationToSpan(snapshot, token.Location);
          tokenInfo = new TokenInfo(span.Start, span.End, TokenType.Keyword);
          return true;
        }
        else if (token is Token.Operator)
        {
          var op = (Token.Operator)token;
          var span = Utils.NLocationToSpan(snapshot, token.Location);
          tokenInfo = new TokenInfo(span.Start, span.End, TokenType.Operator);
          if (op.name.EndsWith("."))
            tokenInfo.Trigger = TokenTriggers.MemberSelect;
          return true;
        }
        else if (token is Token.Identifier
          || token is Token.IdentifierToComplete
          || token is Token.QuotedIdentifier)
        {
          var span = Utils.NLocationToSpan(snapshot, token.Location);
          tokenInfo = new TokenInfo(span.Start, span.End, TokenType.Identifier);
          return true;
        }
        else if (token is Token.IntegerLiteral
          || token is Token.FloatLiteral
          || token is Token.DoubleLiteral
          || token is Token.DecimalLiteral
          || token is Token.CharLiteral)
        {
          var span = Utils.NLocationToSpan(snapshot, token.Location);
          tokenInfo = new TokenInfo(span.Start, span.End, TokenType.Literal);
          return true;
        }
        else if (token is Token.StringLiteral)
        {
          var span = Utils.NLocationToSpan(snapshot, token.Location);
          tokenInfo = new TokenInfo(span.Start, span.End, TokenType.String);
          return true;
        }
        else if (token is Token.WhiteSpace)
        {
          var span = Utils.NLocationToSpan(snapshot, token.Location);
          tokenInfo = new TokenInfo(span.Start, span.End, TokenType.WhiteSpace);
          return true;
        }
      }

      return false;
    }
  }
}
