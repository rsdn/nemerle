using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.LanguageService
{
  using MatchingBraces = Tuple<ITextSnapshot, List<Span>>;

  public sealed class MatchingBracesClassifier : IClassifier
  {
    private readonly ITextBuffer         _textBuffer;
    private readonly IClassificationType _braceClassificationType;
    private          MatchingBraces      _matchingBraces;

    public MatchingBracesClassifier(IClassificationTypeRegistryService classificationRegistry, ITextBuffer textBuffer)
    {
      _textBuffer              = textBuffer;
      _braceClassificationType = classificationRegistry.GetClassificationType(ClassificationTypes.MathingBracesName);
      _matchingBraces          = null;
    }

    public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;

    public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
    {
      var matchingBraces = _matchingBraces;
      if (matchingBraces == null || matchingBraces.Item1.Version != span.Snapshot.Version)
        return ClassifierUtils.EmptyClassifications;

      List<ClassificationSpan> spans = null;
      foreach (var x in matchingBraces.Item2)
        if (span.IntersectsWith(x))
        {
          if (spans == null)
            spans = new List<ClassificationSpan>();
          spans.Add(new ClassificationSpan(new SnapshotSpan(matchingBraces.Item1, x), _braceClassificationType));
        }
      return spans ?? ClassifierUtils.EmptyClassifications;
    }

    public void UpdateClassifications(CaretPosition caretPosition)
    {
      var bufferPosition    = caretPosition.BufferPosition;
      var snapshot          = bufferPosition.Snapshot;
      var position          = bufferPosition.Position;
      var oldMatchingBraces = _matchingBraces;
      if (HasBraceAtPosition(snapshot, position))
      {
        MatchingBraces newMatchingBraces = null;
        ClassificationParseResult parseResult;
        if (ParseUtil.TryParse(_textBuffer, out parseResult) && snapshot.Version == parseResult.Snapshot.Version)
          SearchMatchingBraces(parseResult.Tokens, parseResult.Snapshot, position, ref newMatchingBraces);

        _matchingBraces = newMatchingBraces;

        if (null != oldMatchingBraces)
          NotifyClassificationChanged(oldMatchingBraces);

        if (null != newMatchingBraces)
          NotifyClassificationChanged(newMatchingBraces);
      }
      else if (oldMatchingBraces != null)
      {
        _matchingBraces = null;
        NotifyClassificationChanged(oldMatchingBraces);
      }
    }

    private void NotifyClassificationChanged(MatchingBraces matchingBraces)
    {
      var handler = ClassificationChanged;
      if (handler != null)
      {
        foreach (var x in matchingBraces.Item2)
          handler(this, new ClassificationChangedEventArgs(new SnapshotSpan(matchingBraces.Item1, x)));
      }
    }

    private static bool HasBraceAtPosition(ITextSnapshot snapshot, int position)
    {
      var prevPosition = position - 1;
      if (0 <= prevPosition && prevPosition < snapshot.Length)
        switch (snapshot[prevPosition])
        {
          case ')':
          case ']':
          case '}':
            return true;
        }

      if (0 <= position && position < snapshot.Length)
        switch (snapshot[position])
        {
          case '(':
          case '[':
          case '{':
            return true;
        }

      return false;
    }

    private static void SearchMatchingBraces(Token token, ITextSnapshot snapshot, int caretPos, ref MatchingBraces spans)
    {
      while (token != null)
      {
        var tokenSpan = Utils.NLocationToSpan(snapshot, token.Location);
        if (caretPos < tokenSpan.Start)
          break;

        if (token is Token.BracesGroup)
        {
          var groupToken = (Token.BracesGroup) token;
          if ((caretPos == tokenSpan.Start || caretPos == tokenSpan.End) && groupToken.CloseBrace != null)
            AddMatchingBraces(snapshot, tokenSpan.Start, tokenSpan.End - 1, ref spans);
          SearchMatchingBraces(groupToken.Child, snapshot, caretPos, ref spans);
        }
        else if (token is Token.RoundGroup)
        {
          var groupToken = (Token.RoundGroup) token;
          if ((caretPos == tokenSpan.Start || caretPos == tokenSpan.End) && groupToken.CloseBrace != null)
            AddMatchingBraces(snapshot, tokenSpan.Start, tokenSpan.End - 1, ref spans);
          SearchMatchingBraces(groupToken.Child, snapshot, caretPos, ref spans);
        }
        else if (token is Token.SquareGroup)
        {
          var groupToken = (Token.SquareGroup) token;
          if ((caretPos == tokenSpan.Start || caretPos == tokenSpan.End) && groupToken.CloseBrace != null)
            AddMatchingBraces(snapshot, tokenSpan.Start, tokenSpan.End - 1, ref spans);
          SearchMatchingBraces(groupToken.Child, snapshot, caretPos, ref spans);
        }
        else if (token is Token.LooseGroup)
        {
          var groupToken = (Token.LooseGroup) token;
          SearchMatchingBraces(groupToken.Child, snapshot, caretPos, ref spans);
        }
        else if (token is Token.QuoteGroup)
        {
          var groupToken = (Token.QuoteGroup) token;
          SearchMatchingBraces(groupToken.Child, snapshot, caretPos, ref spans);
        }
        else if (token is Token.Namespace)
        {
          var nsToken = (Token.Namespace) token;
          SearchMatchingBraces(nsToken.KeywordToken, snapshot, caretPos, ref spans);
          SearchMatchingBraces(nsToken.Body, snapshot, caretPos, ref spans);
        }
        else if (token is Token.Using)
        {
          var nsToken = (Token.Using) token;
          SearchMatchingBraces(nsToken.KeywordToken, snapshot, caretPos, ref spans);
          SearchMatchingBraces(nsToken.Body, snapshot, caretPos, ref spans);
        }

        token = token.Next;
      }
    }

    private static void AddMatchingBraces(ITextSnapshot snapshot, int openBrace, int closeBrace, ref MatchingBraces spans)
    {
      if (null == spans)
        spans = new MatchingBraces(snapshot, new List<Span>(4));
      spans.Item2.Add(new Span(openBrace, 1));
      spans.Item2.Add(new Span(closeBrace, 1));
    }
  }
}
