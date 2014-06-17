using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.LanguageService
{
  public sealed class MatchingBracesClassifier : IClassifier
  {
    private readonly ITextBuffer _textBuffer;
    private readonly IClassificationType _braceClassificationType;
    private int _caretPos;

    public MatchingBracesClassifier(IClassificationTypeRegistryService classificationRegistry, ITextBuffer textBuffer)
    {
      _textBuffer = textBuffer;
      _braceClassificationType = classificationRegistry.GetClassificationType(ClassificationTypes.MathingBracesName);
      _caretPos = -1;
    }

    public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;

    public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
    {
      var caretPos = _caretPos;
      if (caretPos >= 0)
      {
        ClassificationParseResult parseResult;
        if (ParseUtil.TryParse(_textBuffer, out parseResult) && span.Snapshot.Version == parseResult.Snapshot.Version)
        {
          List<ClassificationSpan> spans = null;
          WalkTokens(parseResult.Tokens, parseResult.Snapshot, span, caretPos, ref spans);
          return spans ?? ClassifierUtils.EmptyClassifications;
        }
      }
      return ClassifierUtils.EmptyClassifications;
    }

    public void UpdateClassifications(CaretPosition caretPosition)
    {
      var bufferPosition = caretPosition.BufferPosition;
      var snapshot       = bufferPosition.Snapshot;
      var position       = bufferPosition.Position;
      if (HasBraceAtPosition(snapshot, position))
      {
        _caretPos = position;
        NotifyClassificationChanged(snapshot, new Span(0, snapshot.Length));
      }
      else if (_caretPos >= 0)
      {
        _caretPos = -1;
        NotifyClassificationChanged(snapshot, new Span(0, snapshot.Length));
      }
    }

    private void NotifyClassificationChanged(ITextSnapshot snapshot, Span span)
    {
      var handler = ClassificationChanged;
      if (handler != null)
        handler(this, new ClassificationChangedEventArgs(new SnapshotSpan(snapshot, span)));
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

    private void WalkTokens(Token token, ITextSnapshot snapshot, Span spanToClassify, int caretPos, ref List<ClassificationSpan> spans)
    {
      while (token != null)
      {
        var tokenSpan = Utils.NLocationToSpan(snapshot, token.Location);
        if (tokenSpan.Start >= spanToClassify.End)
          break;

        if (tokenSpan.IntersectsWith(spanToClassify))
        {
          if (token is Token.BracesGroup)
          {
            var groupToken = (Token.BracesGroup) token;
            if ((caretPos == tokenSpan.Start || caretPos == tokenSpan.End) && groupToken.CloseBrace != null)
            {
              AddBraceSpan(snapshot, spanToClassify, tokenSpan.Start, ref spans);
              AddBraceSpan(snapshot, spanToClassify, tokenSpan.End - 1, ref spans);
            }
            WalkTokens(groupToken.Child, snapshot, spanToClassify, caretPos, ref spans);
          }
          else if (token is Token.RoundGroup)
          {
            var groupToken = (Token.RoundGroup) token;
            if ((caretPos == tokenSpan.Start || caretPos == tokenSpan.End) && groupToken.CloseBrace != null)
            {
              AddBraceSpan(snapshot, spanToClassify, tokenSpan.Start, ref spans);
              AddBraceSpan(snapshot, spanToClassify, tokenSpan.End - 1, ref spans);
            }
            WalkTokens(groupToken.Child, snapshot, spanToClassify, caretPos, ref spans);
          }
          else if (token is Token.SquareGroup)
          {
            var groupToken = (Token.SquareGroup) token;
            if ((caretPos == tokenSpan.Start || caretPos == tokenSpan.End) && groupToken.CloseBrace != null)
            {
              AddBraceSpan(snapshot, spanToClassify, tokenSpan.Start, ref spans);
              AddBraceSpan(snapshot, spanToClassify, tokenSpan.End - 1, ref spans);
            }
            WalkTokens(groupToken.Child, snapshot, spanToClassify, caretPos, ref spans);
          }
          else if (token is Token.LooseGroup)
          {
            var groupToken = (Token.LooseGroup) token;
            WalkTokens(groupToken.Child, snapshot, spanToClassify, caretPos, ref spans);
          }
          else if (token is Token.QuoteGroup)
          {
            var groupToken = (Token.QuoteGroup) token;
            WalkTokens(groupToken.Child, snapshot, spanToClassify, caretPos, ref spans);
          }
          else if (token is Token.Namespace)
          {
            var nsToken = (Token.Namespace) token;
            WalkTokens(nsToken.KeywordToken, snapshot, spanToClassify, caretPos, ref spans);
            WalkTokens(nsToken.Body, snapshot, spanToClassify, caretPos, ref spans);
          }
          else if (token is Token.Using)
          {
            var nsToken = (Token.Using) token;
            WalkTokens(nsToken.KeywordToken, snapshot, spanToClassify, caretPos, ref spans);
            WalkTokens(nsToken.Body, snapshot, spanToClassify, caretPos, ref spans);
          }
        }

        token = token.Next;
      }
    }

    private void AddBraceSpan(ITextSnapshot snapshot, Span spanToClassify, int pos, ref List<ClassificationSpan> spans)
    {
      var braceSpan = new Span(pos, 1);
      if (spanToClassify.IntersectsWith(braceSpan))
      {
        if (null == spans)
          spans = new List<ClassificationSpan>(4);
        spans.Add(new ClassificationSpan(new SnapshotSpan(snapshot, braceSpan), _braceClassificationType));
      }
    }
  }
}
