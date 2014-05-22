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
    private readonly IStandardClassificationService _standardClassification;
    private readonly IClassificationTypeRegistryService _classificationRegistry;
    private readonly ITextBuffer _textBuffer;
    private ParseResult _parseResult;



    public NemerleClassifier(
      IStandardClassificationService standardClassification,
      IClassificationTypeRegistryService classificationRegistry,
      ITextBuffer textBuffer)
    {
      _standardClassification = standardClassification;
      _classificationRegistry = classificationRegistry;
      _textBuffer = textBuffer;
      _textBuffer.Changed += TextBuffer_Changed;
      _parseResult = TryParse();
    }

    public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;

    private void TextBuffer_Changed(object sender, TextContentChangedEventArgs e)
    {
      _parseResult = TryParse();
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

      var snapshot = _textBuffer.CurrentSnapshot;
      var result = new List<ClassificationSpan>();

      foreach (var c in _parseResult.Comments)
      {
        if (c.Position > span.End)
          break;

        var commentSpan = new Span(c.Position, c.Length);
        if (span.IntersectsWith(commentSpan))
        {
          var index = 0;
          for (; index < result.Count; ++index)
            if (result[index].Span.Span.Start >= commentSpan.Start)
              break;

          result.Insert(index, new ClassificationSpan(new SnapshotSpan(snapshot, commentSpan), _standardClassification.Comment));
        }
      }

      foreach (var d in _parseResult.Directives)
      {
        if (d.Position > span.End)
          break;

        var directiveSpan = new Span(d.Position, d.Length);
        if (span.IntersectsWith(directiveSpan))
        {
          var index = 0;
          for (; index < result.Count; ++index)
            if (result[index].Span.Span.Start >= directiveSpan.Start)
              break;

          result.Insert(index, new ClassificationSpan(new SnapshotSpan(snapshot, directiveSpan), _standardClassification.PreprocessorKeyword));
        }
      }

      WalkTokens(_parseResult.Tokens, snapshot, span.Span, result, false);

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
      var result     = new ParseResult(tokens, comments, directives);

      return result;
    }

    private void WalkTokens(Token token, ITextSnapshot textSnapshot, Span span, List<ClassificationSpan> classifications, bool isQuotation)
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

        token = WalkToken(tokenSpan, token, textSnapshot, span, classifications, isQuotation);
      }
    }

    private Token WalkToken(Span tokenSpan, Token token, ITextSnapshot textSnapshot, Span span, List<ClassificationSpan> classifications, bool isQuotation)
    {
      if (token is Token.Comma
        || token is Token.Semicolon)
      {
        classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.Operator));
      }
      else if (token is Token.Operator)
      {
        var op = (Token.Operator)token;
        if (isQuotation && op.name == "$" && op.Next != null)
        {
          classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.Operator));

          var spliceToken = op.Next;
          var spliceTokenSpan = NLocationToSpan(textSnapshot, spliceToken.Location);
          classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, new Span(tokenSpan.Start, spliceTokenSpan.End - tokenSpan.Start)), _classificationRegistry.GetClassificationType(ClassificationTypes.QuotationSpliceName)));
          return WalkToken(spliceTokenSpan, spliceToken, textSnapshot, span, classifications, false);
        }
        else if (isQuotation && op.name == ".." && op.Next is Token.Operator && ((Token.Operator)op.Next).name == "$" && op.Next.Next != null)
        {
          var op2Span = NLocationToSpan(textSnapshot, op.Next.Location);
          classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.Operator));
          classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, op2Span), _standardClassification.Operator));

          var spliceToken     = op.Next.Next;
          var spliceTokenSpan = NLocationToSpan(textSnapshot, spliceToken.Location);
          classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, new Span(tokenSpan.Start, spliceTokenSpan.End - tokenSpan.Start)), _classificationRegistry.GetClassificationType(ClassificationTypes.QuotationSpliceName)));
          return WalkToken(spliceTokenSpan, spliceToken, textSnapshot, span, classifications, false);
        }
        else
        {
          classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.Operator));
        }
      }
      else if (token is Token.DecimalLiteral
        || token is Token.DoubleLiteral
        || token is Token.FloatLiteral
        || token is Token.IntegerLiteral)
      {
        classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.NumberLiteral));
      }
      else if (token is Token.CharLiteral)
      {
        classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.CharacterLiteral));
      }
      else if (token is Token.StringLiteral)
      {
        classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.StringLiteral));
      }
      else if (token is Token.Keyword)
      {
        classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.Keyword));
      }
      else if (token is Token.WhiteSpace)
      {
        classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.WhiteSpace));
      }
      else if (token is Token.Identifier
        || token is Token.IdentifierToComplete
        || token is Token.QuotedIdentifier)
      {
        classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.Identifier));
      }
      else if (token is Token.Comment)
      {
        classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.Comment));
      }
      else if (token is Token.LooseGroup)
      {
        var group = (Token.LooseGroup)token;
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation);
      }
      else if (token is Token.RoundGroup)
      {
        var group = (Token.RoundGroup)token;
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation);
      }
      else if (token is Token.SquareGroup)
      {
        var group = (Token.SquareGroup)token;
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation);
      }
      else if (token is Token.BracesGroup)
      {
        var group = (Token.BracesGroup)token;
        WalkTokens(group.Child, textSnapshot, span, classifications, isQuotation);
      }
      else if (token is Token.QuoteGroup)
      {
        var classificationType = _classificationRegistry.GetClassificationType(ClassificationTypes.QuotationName);
        classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), classificationType));

        var group = (Token.QuoteGroup)token;

        var quotationType = GetQuotationType(group);
        if (quotationType != null)
        {
          var quotationTypeSpan = NLocationToSpan(textSnapshot, quotationType.Location);
          if (span.IntersectsWith(quotationTypeSpan))
            classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, quotationTypeSpan), _standardClassification.Keyword));
        }

        WalkTokens(group.Child, textSnapshot, span, classifications, true);
      }
      else if (token is Token.Namespace)
      {
        var nsToken = (Token.Namespace)token;
        WalkTokens(nsToken.KeywordToken, textSnapshot, span, classifications, isQuotation);
        WalkTokens(nsToken.Body, textSnapshot, span, classifications, isQuotation);
      }
      else if (token is Token.Using)
      {
        var nsToken = (Token.Using)token;
        WalkTokens(nsToken.KeywordToken, textSnapshot, span, classifications, isQuotation);
        WalkTokens(nsToken.Body, textSnapshot, span, classifications, isQuotation);
      }

      return token.Next;
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

    private sealed class ParseResult
    {
      public ParseResult(Token.BracesGroup tokens, Comment[] comments, Directive[] directives)
      {
        Tokens = tokens;
        Comments = comments;
        Directives = directives;
      }

      public readonly Token.BracesGroup Tokens;
      public readonly Comment[] Comments;
      public readonly Directive[] Directives;
    }
  }
}
