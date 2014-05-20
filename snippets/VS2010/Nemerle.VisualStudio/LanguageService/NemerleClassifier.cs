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

      WalkTokens(_parseResult.Tokens, snapshot, span.Span, result);

      return result;
    }

    private static Span NLocationToSpan(ITextSnapshot textSnapshot, Location location)
    {
      var startLine = textSnapshot.GetLineFromLineNumber(location.Begin.Line - 1);
      var endLine   = textSnapshot.GetLineFromLineNumber(location.End.Line - 1);

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

      var snapshot  = _textBuffer.CurrentSnapshot;
      var code      = snapshot.GetText();
      var lexer     = new LexerFile((ManagerClass)engine, 0, code, true);
      var preParser = new PreParser(lexer);
      var tokens    = preParser.PreParse();
      var comments  = lexer.GetComments();
      var result    = new ParseResult(tokens, comments);

      return result;
    }

    private void WalkTokens(Token token, ITextSnapshot textSnapshot, Span span, List<ClassificationSpan> classifications)
    {
      for (; token != null; token = token.Next)
      {
        var tokenSpan = NLocationToSpan(textSnapshot, token.Location);
        if (tokenSpan.Start > span.End)
          break;

        if (!span.IntersectsWith(tokenSpan))
          continue;

        if (token is Token.Operator
          || token is Token.Comma
          || token is Token.Semicolon)
        {
          classifications.Add(new ClassificationSpan(new SnapshotSpan(textSnapshot, tokenSpan), _standardClassification.Operator));
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
          WalkTokens(group.Child, textSnapshot, span, classifications);
        }
        else if (token is Token.RoundGroup)
        {
          var group = (Token.RoundGroup)token;
          WalkTokens(group.Child, textSnapshot, span, classifications);
        }
        else if (token is Token.SquareGroup)
        {
          var group = (Token.SquareGroup)token;
          WalkTokens(group.Child, textSnapshot, span, classifications);
        }
        else if (token is Token.BracesGroup)
        {
          var group = (Token.BracesGroup)token;
          WalkTokens(group.Child, textSnapshot, span, classifications);
        }
        else if (token is Token.QuoteGroup)
        {
          var group = (Token.QuoteGroup)token;
          WalkTokens(group.Child, textSnapshot, span, classifications);
        }
        else if (token is Token.Namespace)
        {
          var nsToken = (Token.Namespace)token;
          WalkTokens(nsToken.KeywordToken, textSnapshot, span, classifications);
          WalkTokens(nsToken.Body, textSnapshot, span, classifications);
        }
        else if (token is Token.Using)
        {
          var nsToken = (Token.Using)token;
          WalkTokens(nsToken.KeywordToken, textSnapshot, span, classifications);
          WalkTokens(nsToken.Body, textSnapshot, span, classifications);
        }
      }
    }

    private static IList<ClassificationSpan> EmptyClassificationSpans = new ClassificationSpan[0];

    private sealed class ParseResult
    {
      public ParseResult(Token.BracesGroup tokens, Comment[] comments)
      {
        Tokens = tokens;
        Comments = comments;
      }

      public readonly Token.BracesGroup Tokens;
      public readonly Comment[] Comments;
    }
  }
}
