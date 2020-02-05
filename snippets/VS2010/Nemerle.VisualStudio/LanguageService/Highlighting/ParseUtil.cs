using System;
using System.Collections.Generic;
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
    private static readonly HashSet<string> _quotationTypes = new HashSet<string>(StringComparer.InvariantCulture) { "decl", "parameter", "ttype", "case" };

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

      var code       = snapshot.GetText();
      var lexer      = new HighlightingLexer(engine, VsSourceSnapshot.GetSourceSnapshot(snapshot));
      var preParser  = new PreParser(lexer);
      var tokens     = preParser.ParseTopLevel();
      var _comments  = lexer.GetComments();
      var directives = lexer.GetDirectives();

      var comments = new Comment[_comments.Length];
      for (var i = 0; i < _comments.Length; ++i)
      {
        var c = _comments[i];
        var type = CommentType.Normal;
        var pos = 0;
        var commentParser = c.IsMultiline ? _multiLineCommentParser : _singleLineCommentParser;
        var match = commentParser.Match(code, c.Location.StartPos, c.Location.Length);
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

      parseResult = new ClassificationParseResult(snapshot, tokens.Child, comments, directives);
      textBuffer.Properties[typeof(ClassificationParseResult)] = parseResult;
      return true;
    }

    public static bool TryGetTokenInfo(ITextBuffer textBuffer, int line, int column, out TokenInfo tokenInfo)
    {
      tokenInfo = null;

      ClassificationParseResult parseResult;
      if (!textBuffer.Properties.TryGetProperty(typeof(ClassificationParseResult), out parseResult) || parseResult.Snapshot.Version != textBuffer.CurrentSnapshot.Version)
        return false;

      return SearchToken(parseResult.Tokens, parseResult.Snapshot, new TextPoint(line + 1, column + 1), false, ref tokenInfo);
    }

    private static bool SearchToken(Token token, ITextSnapshot snapshot, TextPoint point, bool isQuotation, ref TokenInfo tokenInfo)
    {
      while (token != null)
      {
        if (point.ToPosition(token.Location.Source) < token.Location.StartPos)
          break;

        Token.Operator spliceOp2;
        Token spliceBody;
        if (isQuotation && IsSpliceSequence(token, out spliceBody))
        {
          if (SearchTokenTest(token, snapshot, point, true, ref tokenInfo))
            return true;

          if (SearchTokenTest(spliceBody, snapshot, point, false, ref tokenInfo))
            return true;

          token = spliceBody.Next;
        }
        else if (isQuotation && IsSpliceListSequence(token, out spliceOp2, out spliceBody))
        {
          if (SearchTokenTest(token, snapshot, point, true, ref tokenInfo))
            return true;

          if (SearchTokenTest(spliceOp2, snapshot, point, true, ref tokenInfo))
            return true;

          if (SearchTokenTest(spliceBody, snapshot, point, false, ref tokenInfo))
            return true;

          token = spliceBody.Next;
        }
        else
        {
          if (SearchTokenTest(token, snapshot, point, isQuotation, ref tokenInfo))
            return true;

          token = token.Next;
        }
      }

      return false;
    }

    private static bool SearchTokenTest(Token token, ITextSnapshot snapshot, TextPoint point, bool isQuotation, ref TokenInfo tokenInfo)
    {
      if (!token.Location.Contains(point.ToPosition(token.Location.Source)))
        return false;

      if (token is Token.LooseGroup)
      {
        var group = (Token.LooseGroup)token;
        if (SearchToken(group.Child, snapshot, point, isQuotation, ref tokenInfo))
          return true;
      }
      else if (token is Token.BracesGroup)
      {
        var group = (Token.BracesGroup)token;
        if (SearchToken(group.Child, snapshot, point, isQuotation, ref tokenInfo))
          return true;
      }
      else if (token is Token.RoundGroup)
      {
        var group = (Token.RoundGroup)token;
        if (SearchToken(group.Child, snapshot, point, isQuotation, ref tokenInfo))
          return true;
      }
      else if (token is Token.SquareGroup)
      {
        var group = (Token.SquareGroup)token;
        if (SearchToken(group.Child, snapshot, point, isQuotation, ref tokenInfo))
          return true;
      }
      else if (token is Token.QuoteGroup)
      {
        var group = (Token.QuoteGroup)token;
        if (SearchToken(group.Child, snapshot, point, true, ref tokenInfo))
          return true;
      }
      else if (token is Token.Namespace)
      {
        var ns = (Token.Namespace)token;
        if (SearchToken(ns.KeywordToken, snapshot, point, isQuotation, ref tokenInfo))
          return true;
        if (SearchToken(ns.Body, snapshot, point, isQuotation, ref tokenInfo))
          return true;
      }
      else if (token is Token.Using)
      {
        var ns = (Token.Using)token;
        if (SearchToken(ns.KeywordToken, snapshot, point, isQuotation, ref tokenInfo))
          return true;
        if (SearchToken(ns.Body, snapshot, point, isQuotation, ref tokenInfo))
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
        if (op.name.EndsWith(".") && !isQuotation)
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

      return false;
    }

    public static List<SyntaxClassifier.SpanInfo> GetTokenSpans(ClassificationParseResult parseResult, Span span)
    {
      var tokenSpans = new List<SyntaxClassifier.SpanInfo>(16);

      foreach (var c in parseResult.Comments)
      {
        if (c.Position > span.End)
          break;

        var commentSpan = new Span(c.Position, c.Length);
        if (span.IntersectsWith(commentSpan))
        {
          InsertClassification(tokenSpans, new SyntaxClassifier.SpanInfo(commentSpan, c.IsMultiline ? SyntaxClassifier.SpanType.MultiLineComment : SyntaxClassifier.SpanType.SingleLineComment));
          if (c.Type != CommentType.Normal)
          {
            var textSpan = new Span(c.TextPosition, c.TextLength);
            SyntaxClassifier.SpanType spanType;
            switch (c.Type)
            {
              case CommentType.ToDo: spanType = SyntaxClassifier.SpanType.ToDoCommentText; break;
              case CommentType.Bug: spanType = SyntaxClassifier.SpanType.BugCommentText; break;
              case CommentType.Hack: spanType = SyntaxClassifier.SpanType.HackCommentText; break;
              default: spanType = default(SyntaxClassifier.SpanType); Trace.Assert(false); break;
            }
            InsertClassification(tokenSpans, new SyntaxClassifier.SpanInfo(textSpan, spanType));
          }
        }
      }

      foreach (var d in parseResult.Directives)
      {
        if (d.Position > span.End)
          break;

        var directiveSpan = new Span(d.Position, d.Length);
        if (span.IntersectsWith(directiveSpan))
          InsertClassification(tokenSpans, new SyntaxClassifier.SpanInfo(directiveSpan, SyntaxClassifier.SpanType.PreprocessorKeyword));
      }

      List<Span> splices = null; // not used
      WalkTokens(parseResult.Tokens, parseResult.Snapshot, span, tokenSpans, false, ref splices);

      return tokenSpans;
    }

    public static void WalkTokens(Token token, ITextSnapshot textSnapshot, Span span, List<SyntaxClassifier.SpanInfo> classifications, bool isQuotation, ref List<Span> splices)
    {
      while (token != null)
      {
        var chunkSpan = GetNextChunkSpan(token, textSnapshot, isQuotation);
        if (chunkSpan.Start > span.End)
          break;

        if (span.IntersectsWith(chunkSpan))
          token = WalkToken(chunkSpan, token, textSnapshot, span, classifications, isQuotation, ref splices);
        else
          token = token.Next;
      }
    }

    public static Span GetNextChunkSpan(Token token, ITextSnapshot textSnapshot, bool isQuotation)
    {
      Token.Operator op2;
      Token body;
      if (isQuotation && (IsSpliceSequence(token, out body) || IsSpliceListSequence(token, out op2, out body)))
      {
        var loc = new Location(token.Location.Source, token.Location.StartPos, body.Location.EndPos);
        return Utils.NLocationToSpan(textSnapshot, loc);
      }
      return Utils.NLocationToSpan(textSnapshot, token.Location);
    }

    public static Token WalkToken(Span chunkSpan, Token token, ITextSnapshot textSnapshot, Span span, List<SyntaxClassifier.SpanInfo> classifications, bool isQuotation, ref List<Span> splices)
    {
      Token.Operator spliceOp2;
      Token spliceToken;

      if (isQuotation && IsSpliceSequence(token, out spliceToken))
      {
        var spliceOp1Span = Utils.NLocationToSpan(textSnapshot, token.Location);
        if (span.IntersectsWith(spliceOp1Span))
          classifications.Add(new SyntaxClassifier.SpanInfo(spliceOp1Span, SyntaxClassifier.SpanType.Operator));

        if (splices == null)
          splices = new List<Span>();
        splices.Add(chunkSpan);

        var spliceSpan = Utils.NLocationToSpan(textSnapshot, spliceToken.Location);
        if (span.IntersectsWith(spliceSpan))
        {
          List<Span> innerSplices = null; // not used
          return WalkToken(spliceSpan, spliceToken, textSnapshot, span, classifications, false, ref innerSplices);
        }
        else
          return spliceToken.Next;
      }
      else if (isQuotation && IsSpliceListSequence(token, out spliceOp2, out spliceToken))
      {
        var spliceOp1Span = Utils.NLocationToSpan(textSnapshot, token.Location);
        if (span.IntersectsWith(spliceOp1Span))
          classifications.Add(new SyntaxClassifier.SpanInfo(spliceOp1Span, SyntaxClassifier.SpanType.Operator));

        var spliceOp2Span = Utils.NLocationToSpan(textSnapshot, spliceOp2.Location);
        if (span.IntersectsWith(spliceOp2Span))
          classifications.Add(new SyntaxClassifier.SpanInfo(spliceOp2Span, SyntaxClassifier.SpanType.Operator));

        if (splices == null)
          splices = new List<Span>();
        splices.Add(chunkSpan);

        var spliceSpan = Utils.NLocationToSpan(textSnapshot, spliceToken.Location);
        if (span.IntersectsWith(spliceSpan))
        {
          List<Span> innerSplices = null; // not used
          return WalkToken(spliceSpan, spliceToken, textSnapshot, span, classifications, false, ref innerSplices);
        }
        else
          return spliceToken.Next;
      }
      else if (token is Token.Identifier
        || token is Token.IdentifierToComplete
        || token is Token.QuotedIdentifier)
      {
        classifications.Add(new SyntaxClassifier.SpanInfo(chunkSpan, SyntaxClassifier.SpanType.Identifier));
      }
      else if (token is Token.Operator
        || token is Token.Semicolon
        || token is Token.Comma)
      {
        classifications.Add(new SyntaxClassifier.SpanInfo(chunkSpan, SyntaxClassifier.SpanType.Operator));
      }
      else if (token is Token.LooseGroup)
      {
        var group = (Token.LooseGroup)token;
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation, ref splices);
      }
      else if (token is Token.RoundGroup)
      {
        var group = (Token.RoundGroup)token;
        if (group.OpenBrace != null)
        {
          var braceSpan = Utils.NLocationToSpan(textSnapshot, group.OpenBrace.Location);
          if (span.IntersectsWith(braceSpan))
            classifications.Add(new SyntaxClassifier.SpanInfo(braceSpan, SyntaxClassifier.SpanType.Brace));
        }
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation, ref splices);
        if (group.CloseBrace != null)
        {
          var braceSpan = Utils.NLocationToSpan(textSnapshot, group.CloseBrace.Location);
          if (span.IntersectsWith(braceSpan))
            classifications.Add(new SyntaxClassifier.SpanInfo(braceSpan, SyntaxClassifier.SpanType.Brace));
        }
      }
      else if (token is Token.SquareGroup)
      {
        var group = (Token.SquareGroup)token;
        if (group.OpenBrace != null)
        {
          var braceSpan = Utils.NLocationToSpan(textSnapshot, group.OpenBrace.Location);
          if (span.IntersectsWith(braceSpan))
            classifications.Add(new SyntaxClassifier.SpanInfo(braceSpan, SyntaxClassifier.SpanType.Brace));
        }
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation, ref splices);
        if (group.CloseBrace != null)
        {
          var braceSpan = Utils.NLocationToSpan(textSnapshot, group.CloseBrace.Location);
          if (span.IntersectsWith(braceSpan))
            classifications.Add(new SyntaxClassifier.SpanInfo(braceSpan, SyntaxClassifier.SpanType.Brace));
        }
      }
      else if (token is Token.BracesGroup)
      {
        var group = (Token.BracesGroup)token;
        if (group.OpenBrace != null)
        {
          var braceSpan = Utils.NLocationToSpan(textSnapshot, group.OpenBrace.Location);
          if (span.IntersectsWith(braceSpan))
            classifications.Add(new SyntaxClassifier.SpanInfo(braceSpan, SyntaxClassifier.SpanType.Brace));
        }
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation, ref splices);
        if (group.CloseBrace != null)
        {
          var braceSpan = Utils.NLocationToSpan(textSnapshot, group.CloseBrace.Location);
          if (span.IntersectsWith(braceSpan))
            classifications.Add(new SyntaxClassifier.SpanInfo(braceSpan, SyntaxClassifier.SpanType.Brace));
        }
      }
      else if (token is Token.QuoteGroup)
      {
        var group = (Token.QuoteGroup)token;
        if (group.OpenBrace != null)
        {
          var braceSpan = Utils.NLocationToSpan(textSnapshot, group.OpenBrace.Location);
          if (span.IntersectsWith(braceSpan))
            classifications.Add(new SyntaxClassifier.SpanInfo(braceSpan, SyntaxClassifier.SpanType.QuotationBrace));
        }

        Token.Identifier type;
        Token.Operator colon;
        Token body;
        if (IsQuotationWithHeader(group, out type, out colon, out body))
        {
          var typeSpan = Utils.NLocationToSpan(textSnapshot, type.Location);
          if (span.IntersectsWith(typeSpan))
            classifications.Add(new SyntaxClassifier.SpanInfo(typeSpan, SyntaxClassifier.SpanType.Keyword));

          var colonSpan = Utils.NLocationToSpan(textSnapshot, colon.Location);
          if (span.IntersectsWith(colonSpan))
            classifications.Add(new SyntaxClassifier.SpanInfo(colonSpan, SyntaxClassifier.SpanType.Quotation));
        }
        else
        {
          body = group.Child;
        }

        List<Span> innerSplices = null;
        WalkTokens(body, textSnapshot, span, classifications, true, ref innerSplices);

        if (innerSplices == null)
          InsertClassification(classifications, new SyntaxClassifier.SpanInfo(chunkSpan, SyntaxClassifier.SpanType.Quotation));
        else
        {
          var pos = chunkSpan.Start;
          foreach (var s in innerSplices)
          {
            InsertClassification(classifications, new SyntaxClassifier.SpanInfo(new Span(pos, s.Start - pos), SyntaxClassifier.SpanType.Quotation));
            pos = s.End;
          }
          InsertClassification(classifications, new SyntaxClassifier.SpanInfo(new Span(pos, chunkSpan.End - pos), SyntaxClassifier.SpanType.Quotation));
        }

        if (group.CloseBrace != null)
        {
          var braceSpan = Utils.NLocationToSpan(textSnapshot, group.CloseBrace.Location);
          if (span.IntersectsWith(braceSpan))
            classifications.Add(new SyntaxClassifier.SpanInfo(braceSpan, SyntaxClassifier.SpanType.QuotationBrace));
        }
      }
      else if (token is Token.DecimalLiteral
        || token is Token.DoubleLiteral
        || token is Token.FloatLiteral
        || token is Token.IntegerLiteral)
      {
        classifications.Add(new SyntaxClassifier.SpanInfo(chunkSpan, SyntaxClassifier.SpanType.Number));
      }
      else if (token is Token.Keyword)
      {
        classifications.Add(new SyntaxClassifier.SpanInfo(chunkSpan, SyntaxClassifier.SpanType.Keyword));
      }
      else if (token is Token.CharLiteral)
      {
        classifications.Add(new SyntaxClassifier.SpanInfo(chunkSpan, SyntaxClassifier.SpanType.Character));
      }
      else if (token is Token.StringLiteral)
      {
        classifications.Add(new SyntaxClassifier.SpanInfo(chunkSpan, GetStringType((Token.StringLiteral)token)));
      }
      else if (token is Token.WhiteSpace)
      {
        classifications.Add(new SyntaxClassifier.SpanInfo(chunkSpan, SyntaxClassifier.SpanType.Whitespace));
      }
      else if (token is Token.Comment)
      {
        classifications.Add(new SyntaxClassifier.SpanInfo(chunkSpan, SyntaxClassifier.SpanType.MultiLineComment));
      }
      else if (token is Token.Namespace)
      {
        var nsToken = (Token.Namespace)token;
        WalkTokens(nsToken.KeywordToken, textSnapshot, span, classifications, isQuotation, ref splices);
        WalkTokens(nsToken.Body, textSnapshot, span, classifications, isQuotation, ref splices);
      }
      else if (token is Token.Using)
      {
        var nsToken = (Token.Using)token;
        WalkTokens(nsToken.KeywordToken, textSnapshot, span, classifications, isQuotation, ref splices);
        WalkTokens(nsToken.Body, textSnapshot, span, classifications, isQuotation, ref splices);
      }

      return token.Next;
    }

    public static void InsertClassification(List<SyntaxClassifier.SpanInfo> items, SyntaxClassifier.SpanInfo span)
    {
      var index = items.BinarySearch(span);
      items.Insert(index >= 0 ? index : ~index, span);
    }

    public static SyntaxClassifier.SpanType GetStringType(Token.StringLiteral token)
    {
      var c1 = token.rawString[0];
      if (c1 == '"')
        return SyntaxClassifier.SpanType.String;

      var c2 = token.rawString[1];
      if (c1 == '@' && c2 == '"')
        return SyntaxClassifier.SpanType.VerbatimString;

      if (c1 == '<' && c2 == '#')
        return SyntaxClassifier.SpanType.RecursiveString;

      Trace.Assert(false);
      return SyntaxClassifier.SpanType.Whitespace;
    }

    public static bool IsSpliceSequence(Token token, out Token body)
    {
      var _op1 = token as Token.Operator;
      if (_op1 != null && string.Equals(_op1.name, "$", StringComparison.InvariantCulture) && _op1.Next != null)
      {
        body = _op1.Next;
        return true;
      }
      body = null;
      return false;
    }

    public static bool IsSpliceListSequence(Token token, out Token.Operator op2, out Token body)
    {
      var _op1 = token as Token.Operator;
      if (_op1 != null && string.Equals(_op1.name, "..", StringComparison.InvariantCulture))
      {
        var _op2 = _op1.Next as Token.Operator;
        if (_op2 != null && string.Equals(_op2.name, "$", StringComparison.InvariantCulture) && _op2.Next != null)
        {
          op2 = _op2;
          body = _op2.Next;
          return true;
        }
      }
      op2 = null;
      body = null;
      return false;
    }

    public static bool IsQuotationWithHeader(Token.QuoteGroup group, out Token.Identifier type, out Token.Operator colon, out Token body)
    {
      var content = group.Child as Token.LooseGroup;
      if (content != null)
      {
        var _type = content.Child as Token.Identifier;
        if (_type != null && _quotationTypes.Contains(_type.name))
        {
          var _colon = _type.Next as Token.Operator;
          if (_colon != null && string.Equals(_colon.name, ":", StringComparison.InvariantCulture))
          {
            type = _type;
            colon = _colon;
            body = _colon.Next;
            return true;
          }
        }
      }
      type = null;
      colon = null;
      body = null;
      return false;
    }
  }
}
