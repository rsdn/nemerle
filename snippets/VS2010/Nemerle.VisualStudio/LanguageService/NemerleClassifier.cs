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
        if (span.IntersectsWith(c))
          result.Add(new ClassificationSpan(new SnapshotSpan(snapshot, c), _standardClassification.Comment));
      }
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
      var lexer     = new LexerString((ManagerClass)engine, snapshot.GetText(), new Location(0, 1, 1));
      var preParser = new PreParser(lexer);
      var tokens    = preParser.PreParse();
      var comments  = new Span[lexer.CommentLocations.Count];
      int cIndex = 0;
      foreach (var c in lexer.CommentLocations)
        comments[cIndex++] = NLocationToSpan(snapshot, c.Field0);

      var result = new ParseResult(tokens, comments);

      return result;
    }

    private static IList<ClassificationSpan> EmptyClassificationSpans = new ClassificationSpan[0];

    private sealed class ParseResult
    {
      public ParseResult(Token.BracesGroup tokens, Span[] comments)
      {
        Tokens = tokens;
        Comments = comments;
      }

      public readonly Token.BracesGroup Tokens;
      public readonly Span[] Comments;
    }
  }
}
