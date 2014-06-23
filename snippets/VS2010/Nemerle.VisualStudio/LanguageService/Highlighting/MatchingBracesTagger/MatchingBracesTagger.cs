using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.LanguageService
{
  using MatchingBraces = Tuple<ITextSnapshot, List<Span>>;

  public sealed class MatchingBracesTagger : ITagger<TextMarkerTag>
  {
    private static readonly TextMarkerTag _braceTag = new TextMarkerTag("blue");
    private readonly ITextBuffer _textBuffer;
    private MatchingBraces _matchingBraces;

    public MatchingBracesTagger(ITextView textView, ITextBuffer textBuffer)
    {
      _textBuffer = textBuffer;

      textView.Caret.PositionChanged += (_, args) => UpdateClassifications(args.NewPosition);
      textBuffer.PostChanged += (_, args) => UpdateClassifications(textView.Caret.Position);
    }

    public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

    public IEnumerable<ITagSpan<TextMarkerTag>> GetTags(NormalizedSnapshotSpanCollection spans)
    {
      var matchingBraces = _matchingBraces;
      if (matchingBraces == null || spans.Count == 0)
        yield break;

      var currentSnapshot = spans[0].Snapshot;
      foreach (var _tagSpan in matchingBraces.Item2)
      {
        var tagSpan = matchingBraces.Item1.Version == currentSnapshot
          ? new SnapshotSpan(currentSnapshot, _tagSpan)
          : new SnapshotSpan(matchingBraces.Item1, _tagSpan).TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive);

        foreach (var span in spans)
          if (span.IntersectsWith(tagSpan))
            yield return new TagSpan<TextMarkerTag>(tagSpan, _braceTag);
      }
    }

    private void UpdateClassifications(CaretPosition caretPosition)
    {
      var bufferPosition = caretPosition.BufferPosition;
      var snapshot = bufferPosition.Snapshot;
      var position = bufferPosition.Position;
      var oldMatchingBraces = _matchingBraces;
      if (HasBraceAtPosition(snapshot, position))
      {
        MatchingBraces newMatchingBraces = null;
        ClassificationParseResult parseResult;
        if (ParseUtil.TryParse(_textBuffer, out parseResult) && snapshot.Version == parseResult.Snapshot.Version)
          SearchMatchingBraces(parseResult.Tokens, parseResult.Snapshot, position, ref newMatchingBraces);

        _matchingBraces = newMatchingBraces;

        if (null != oldMatchingBraces)
          NotifyTagsChanged(oldMatchingBraces);

        if (null != newMatchingBraces)
          NotifyTagsChanged(newMatchingBraces);
      }
      else if (oldMatchingBraces != null)
      {
        _matchingBraces = null;
        NotifyTagsChanged(oldMatchingBraces);
      }
    }

    private void NotifyTagsChanged(MatchingBraces matchingBraces)
    {
      var handler = TagsChanged;
      if (handler != null)
      {
        foreach (var x in matchingBraces.Item2)
          handler(this, new SnapshotSpanEventArgs(new SnapshotSpan(matchingBraces.Item1, x)));
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

      if (0 <= prevPosition && prevPosition < snapshot.Length)
        switch (snapshot[prevPosition])
        {
          case '(':
          case '[':
          case '{':
            return true;
        }

      if (0 <= position && position < snapshot.Length)
        switch (snapshot[position])
        {
          case ')':
          case ']':
          case '}':
            return true;
        }

      return false;
    }

    private static void SearchMatchingBraces(Token token, ITextSnapshot snapshot, int caretPos, ref MatchingBraces matchingBraces)
    {
      for (; token != null; token = token.Next)
      {
        var tokenSpan = Utils.NLocationToSpan(snapshot, token.Location);
        if (tokenSpan.End < caretPos)
          continue;

        if (caretPos < tokenSpan.Start)
          break;

        if (token is Token.BracesGroup)
        {
          var groupToken = (Token.BracesGroup)token;
          TryAddMatchingBraces(snapshot, caretPos, ref matchingBraces, tokenSpan, groupToken.CloseBrace);
          SearchMatchingBraces(groupToken.Child, snapshot, caretPos, ref matchingBraces);
        }
        else if (token is Token.RoundGroup)
        {
          var groupToken = (Token.RoundGroup)token;
          TryAddMatchingBraces(snapshot, caretPos, ref matchingBraces, tokenSpan, groupToken.CloseBrace);
          SearchMatchingBraces(groupToken.Child, snapshot, caretPos, ref matchingBraces);
        }
        else if (token is Token.SquareGroup)
        {
          var groupToken = (Token.SquareGroup)token;
          TryAddMatchingBraces(snapshot, caretPos, ref matchingBraces, tokenSpan, groupToken.CloseBrace);
          SearchMatchingBraces(groupToken.Child, snapshot, caretPos, ref matchingBraces);
        }
        else if (token is Token.LooseGroup)
        {
          var groupToken = (Token.LooseGroup)token;
          SearchMatchingBraces(groupToken.Child, snapshot, caretPos, ref matchingBraces);
        }
        else if (token is Token.QuoteGroup)
        {
          var groupToken = (Token.QuoteGroup)token;
          SearchMatchingBraces(groupToken.Child, snapshot, caretPos, ref matchingBraces);
        }
        else if (token is Token.Namespace)
        {
          var nsToken = (Token.Namespace)token;
          SearchMatchingBraces(nsToken.KeywordToken, snapshot, caretPos, ref matchingBraces);
          SearchMatchingBraces(nsToken.Body, snapshot, caretPos, ref matchingBraces);
        }
        else if (token is Token.Using)
        {
          var nsToken = (Token.Using)token;
          SearchMatchingBraces(nsToken.KeywordToken, snapshot, caretPos, ref matchingBraces);
          SearchMatchingBraces(nsToken.Body, snapshot, caretPos, ref matchingBraces);
        }
      }
    }

    private static void TryAddMatchingBraces(ITextSnapshot snapshot, int caretPos, ref MatchingBraces matchingBraces, Span tokenSpan, object closeBrace)
    {
      if (IsMatchingBraces(caretPos, tokenSpan, closeBrace))
        AddMatchingBraces(snapshot, tokenSpan.Start, tokenSpan.End - 1, ref matchingBraces);
    }

    private static bool IsMatchingBraces(int caretPos, Span tokenSpan, object closeBrace)
    {
      return (caretPos == tokenSpan.Start || caretPos == tokenSpan.End || caretPos - 1 == tokenSpan.Start || caretPos == tokenSpan.End - 1) && closeBrace != null;
    }

    private static void AddMatchingBraces(ITextSnapshot snapshot, int openBrace, int closeBrace, ref MatchingBraces matchingBraces)
    {
      if (null == matchingBraces)
        matchingBraces = new MatchingBraces(snapshot, new List<Span>(4));
      matchingBraces.Item2.Add(new Span(openBrace, 1));
      matchingBraces.Item2.Add(new Span(closeBrace, 1));
    }
  }
}
