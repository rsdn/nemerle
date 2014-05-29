using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.TextManager.Interop;
using Nemerle.Compiler;
using System;
using System.Collections.Generic;

namespace Nemerle.VisualStudio.LanguageService
{
  public sealed class NemerleClassifier : IClassifier
  {
    private readonly ITextBuffer _textBuffer;
    private ParseResult _parseResult;
    private readonly IClassificationType[] _classificationTypes;

    public NemerleClassifier(
      IStandardClassificationService standardClassification,
      IClassificationTypeRegistryService classificationRegistry,
      ITextBuffer textBuffer)
    {
      _textBuffer = textBuffer;
      _textBuffer.Changed += TextBuffer_Changed;
      _classificationTypes = new IClassificationType[]
      {
        standardClassification.Identifier,
        standardClassification.Keyword,
        standardClassification.PreprocessorKeyword,
        standardClassification.Operator,
        standardClassification.NumberLiteral,
        standardClassification.CharacterLiteral,
        standardClassification.WhiteSpace,
        standardClassification.Comment,
        standardClassification.Comment,
        standardClassification.StringLiteral,
        standardClassification.StringLiteral,
        classificationRegistry.GetClassificationType(ClassificationTypes.QuotationName),
      };
    }

    public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;

    private void TextBuffer_Changed(object sender, TextContentChangedEventArgs e)
    {
      _parseResult = null;
    }

    private void OnClassificationChanged(SnapshotSpan span)
    {
      var handler = ClassificationChanged;
      if (null != handler)
        handler(this, new ClassificationChangedEventArgs(span));
    }

    public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
    {
      if (_parseResult == null)
        _parseResult = TryParse();

      if (_parseResult == null)
        return EmptyClassificationSpans;

      var snapshot = _parseResult.Snapshot;
      var spans = new List<SpanInfo>(16);

      foreach (var c in _parseResult.Comments)
      {
        if (c.Position > span.End)
          break;

        var commentSpan = new Span(c.Position, c.Length);
        if (span.IntersectsWith(commentSpan))
          InsertClassification(spans, new SpanInfo(commentSpan, c.IsMultiline ? SpanType.MultiLineComment : SpanType.SingleLineComment));
      }

      foreach (var d in _parseResult.Directives)
      {
        if (d.Position > span.End)
          break;

        var directiveSpan = new Span(d.Position, d.Length);
        if (span.IntersectsWith(directiveSpan))
          InsertClassification(spans, new SpanInfo(directiveSpan, SpanType.PreprocessorKeyword));
      }

      List<Span> splices = null; // not used
      WalkTokens(_parseResult.Tokens, snapshot, span.Span, spans, false, ref splices);

      var result = new ClassificationSpan[spans.Count];
      for (var i = 0; i < spans.Count; ++i)
      {
        var spanInfo = spans[i];
        result[i] = new ClassificationSpan(new SnapshotSpan(snapshot, spanInfo.Span), _classificationTypes[(int) spanInfo.Type]);
      }
      return result;
    }

    private static Span NLocationToSpan(ITextSnapshot textSnapshot, Location location)
    {
      var startLine = textSnapshot.GetLineFromLineNumber(location.Begin.Line - 1);
      var endLine = location.Begin.Line == location.End.Line
        ? startLine
        : textSnapshot.GetLineFromLineNumber(location.End.Line - 1);

      var startPos = startLine.Start.Position + location.Begin.Column - 1;
      var endPos   = endLine.Start.Position + location.End.Column - 1;

      return new Span(startPos, endPos - startPos);
    }

    private ParseResult TryParse()
    {
      NemerleSource source;
      if (!_textBuffer.Properties.TryGetProperty<NemerleSource>(typeof(NemerleSource), out source))
        return null;

      var engine = source.GetEngine();
      if (!engine.RequestOnInitEngine())
        return null;

      var snapshot   = _textBuffer.CurrentSnapshot;
      var code       = snapshot.GetText();
      var lexer      = new LexerFile((ManagerClass)engine, 0, code, true);
      var preParser  = new PreParser(lexer);
      var tokens     = preParser.PreParse();
      var comments   = lexer.GetComments();
      var directives = lexer.GetDirectives();
      var result     = new ParseResult(snapshot, tokens, comments, directives);

      return result;
    }

    private static void WalkTokens(Token token, ITextSnapshot textSnapshot, Span span, List<SpanInfo> classifications, bool isQuotation, ref List<Span> splices)
    {
      while (token != null)
      {
        var tokenSpan = NLocationToSpan(textSnapshot, token.Location);
        if (tokenSpan.Start > span.End)
          break;

        if (!span.IntersectsWith(tokenSpan))
        {
          token = token.Next;
          continue;
        }

        token = WalkToken(tokenSpan, token, textSnapshot, span, classifications, isQuotation, ref splices);
      }
    }

    private static Token WalkToken(Span tokenSpan, Token token, ITextSnapshot textSnapshot, Span span, List<SpanInfo> classifications, bool isQuotation, ref List<Span> splices)
    {
      if (token is Token.Comma
        || token is Token.Semicolon)
      {
        classifications.Add(new SpanInfo(tokenSpan, SpanType.Operator));
      }
      else if (token is Token.Operator)
      {
        var op = (Token.Operator)token;
        if (isQuotation && op.name == "$" && op.Next != null)
        {
          classifications.Add(new SpanInfo(tokenSpan, SpanType.Operator));

          var spliceToken     = op.Next;
          var spliceTokenSpan = NLocationToSpan(textSnapshot, spliceToken.Location);
          if (splices == null)
            splices = new List<Span>();
          splices.Add(new Span(tokenSpan.Start, spliceTokenSpan.End - tokenSpan.Start));

          List<Span> innerSplices = null; // not used
          return WalkToken(spliceTokenSpan, spliceToken, textSnapshot, span, classifications, false, ref innerSplices);
        }
        else if (isQuotation && op.name == ".." && op.Next is Token.Operator && ((Token.Operator)op.Next).name == "$" && op.Next.Next != null)
        {
          var op2Span = NLocationToSpan(textSnapshot, op.Next.Location);
          classifications.Add(new SpanInfo(tokenSpan, SpanType.Operator));
          classifications.Add(new SpanInfo(op2Span, SpanType.Operator));

          var spliceToken     = op.Next.Next;
          var spliceTokenSpan = NLocationToSpan(textSnapshot, spliceToken.Location);
          if (splices == null)
            splices = new List<Span>();
          splices.Add(new Span(tokenSpan.Start, spliceTokenSpan.End - tokenSpan.Start));

          List<Span> innerSplices = null; // not used
          return WalkToken(spliceTokenSpan, spliceToken, textSnapshot, span, classifications, false, ref innerSplices);
        }
        else
        {
          classifications.Add(new SpanInfo(tokenSpan, SpanType.Operator));
        }
      }
      else if (token is Token.DecimalLiteral
        || token is Token.DoubleLiteral
        || token is Token.FloatLiteral
        || token is Token.IntegerLiteral)
      {
        classifications.Add(new SpanInfo(tokenSpan, SpanType.Number));
      }
      else if (token is Token.CharLiteral)
      {
        classifications.Add(new SpanInfo(tokenSpan, SpanType.Character));
      }
      else if (token is Token.StringLiteral)
      {
        classifications.Add(new SpanInfo(tokenSpan, SpanType.MultiLineString));
      }
      else if (token is Token.Keyword)
      {
        classifications.Add(new SpanInfo(tokenSpan, SpanType.Keyword));
      }
      else if (token is Token.WhiteSpace)
      {
        classifications.Add(new SpanInfo(tokenSpan, SpanType.Whitespace));
      }
      else if (token is Token.Identifier
        || token is Token.IdentifierToComplete
        || token is Token.QuotedIdentifier)
      {
        classifications.Add(new SpanInfo(tokenSpan, SpanType.Identifier));
      }
      else if (token is Token.Comment)
      {
        classifications.Add(new SpanInfo(tokenSpan, SpanType.MultiLineComment));
      }
      else if (token is Token.LooseGroup)
      {
        var group = (Token.LooseGroup)token;
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation, ref splices);
      }
      else if (token is Token.RoundGroup)
      {
        var group = (Token.RoundGroup)token;
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation, ref splices);
      }
      else if (token is Token.SquareGroup)
      {
        var group = (Token.SquareGroup)token;
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation, ref splices);
      }
      else if (token is Token.BracesGroup)
      {
        var group = (Token.BracesGroup)token;
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation, ref splices);
      }
      else if (token is Token.QuoteGroup)
      {
        var group = (Token.QuoteGroup)token;

        var quotationType = GetQuotationType(group);
        if (quotationType != null)
        {
          var quotationTypeSpan = NLocationToSpan(textSnapshot, quotationType.Location);
          if (span.IntersectsWith(quotationTypeSpan))
            classifications.Add(new SpanInfo(quotationTypeSpan, SpanType.Keyword));
        }

        List<Span> innerSplices = null;
        WalkTokens(group.Child, textSnapshot, span, classifications, true, ref innerSplices);

        if (innerSplices == null)
          InsertClassification(classifications, new SpanInfo(tokenSpan, SpanType.Quotation));
        else
        {
          var pos = tokenSpan.Start;
          foreach (var s in innerSplices)
          {
            InsertClassification(classifications, new SpanInfo(new Span(pos, s.Start - pos), SpanType.Quotation));
            pos = s.End;
          }
          InsertClassification(classifications, new SpanInfo(new Span(pos, tokenSpan.End - pos), SpanType.Quotation));
        }
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

    private static void InsertClassification(List<SpanInfo> items, SpanInfo span)
    {
      var index = 0;
      for (; index < items.Count; ++index)
        if (items[index].Span.Start >= span.Span.Start)
          break;

      items.Insert(index, span);
    }

    private static Token.Identifier GetQuotationType(Token.QuoteGroup group)
    {
      if (group.Child is Token.LooseGroup)
      {
        var content = (Token.LooseGroup)group.Child;
        if (content.Child is Token.Identifier)
        {
          var identifier = (Token.Identifier)content.Child;
          if (_quotationTypes.Contains(identifier.name) && identifier.Next is Token.Operator)
          {
            var op = (Token.Operator)identifier.Next;
            if (op.name == ":")
              return identifier;
          }
        }
      }
      return null;
    }

    private static readonly HashSet<string> _quotationTypes = new HashSet<string>(new []{ "decl", "parameter", "ttype", "case" });

    private static readonly IList<ClassificationSpan> EmptyClassificationSpans = new ClassificationSpan[0];

    private enum SpanType : byte
    {
      Identifier = 0,
      Keyword,
      PreprocessorKeyword,
      Operator,
      Number,
      Character,
      Whitespace,
      SingleLineComment,
      MultiLineComment,
      SingleLineString,
      MultiLineString,
      Quotation
    }

    private struct SpanInfo
    {
      public readonly Span Span;
      public readonly SpanType Type;

      public SpanInfo(Span span, SpanType type)
      {
        Span = span;
        Type = type;
      }
    }

    private sealed class ParseResult
    {
      public ParseResult(ITextSnapshot snapshot, Token.BracesGroup tokens, Comment[] comments, Directive[] directives)
      {
        Snapshot = snapshot;
        Tokens = tokens;
        Comments = comments;
        Directives = directives;
      }

      public readonly ITextSnapshot Snapshot;
      public readonly Token.BracesGroup Tokens;
      public readonly Comment[] Comments;
      public readonly Directive[] Directives;
    }
  }
}
