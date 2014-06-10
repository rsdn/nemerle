using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Nemerle.Completion2;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Nemerle.VisualStudio.LanguageService
{
  public sealed class UsageClassifier : IClassifier
  {
    private readonly IClassificationType  _highlightOne;
    private readonly IClassificationType  _highlightTwo;
    private readonly ITextBuffer          _textBuffer;
    private          ClassificationSpan[] _classificationSpans = {};

    public UsageClassifier(IClassificationTypeRegistryService classificationRegistry, ITextBuffer textBuffer)
    {
      _highlightOne = classificationRegistry.GetClassificationType(ClassificationTypes.HighlightOneName);
      _highlightTwo = classificationRegistry.GetClassificationType(ClassificationTypes.HighlightTwoName);
      _textBuffer = textBuffer;
      _textBuffer.Changed += TextBuffer_Changed;
    }

    private void TextBuffer_Changed(object sender, TextContentChangedEventArgs e)
    {
      UpdateUsageHighlighting(Enumerable.Empty<GotoInfo>());
    }

    public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;

    public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
    {
      if (_classificationSpans.Length == 0)
        return _classificationSpans;

      var spans = new List<ClassificationSpan>();
      foreach (var s in _classificationSpans)
      {
        if (span.End < s.Span.Start)
          break;
        if (span.IntersectsWith(s.Span))
          spans.Add(s);
      }
      return spans;
    }

    public void UpdateUsageHighlighting(IEnumerable<GotoInfo> highlightings)
    {
      var snapshot               = _textBuffer.CurrentSnapshot;
      var newClassificationSpans = highlightings.Select(x => MakeClassificationSpan(snapshot, x)).OrderBy(x => x.Span.Start).ThenBy(x => x.Span.End).ToArray();
      if (newClassificationSpans.Length != 0 || _classificationSpans.Length != 0)
      {
        var oldBounds = GetBounds(_classificationSpans);
        var newBounds = GetBounds(newClassificationSpans);

        _classificationSpans = newClassificationSpans;
        var handler = ClassificationChanged;
        if (handler != null)
        {
          var updateSpan = GetUpdateBounds(oldBounds, newBounds);
          handler(this, new ClassificationChangedEventArgs(new SnapshotSpan(snapshot, updateSpan)));
        }
      }
    }

    private static Span GetBounds(ClassificationSpan[] spans)
    {
      if (spans.Length == 0)
        return new Span(0, 0);

      var startPos = spans[0].Span.Start;
      var endPos   = spans[spans.Length - 1].Span.End;
      return new Span(startPos, endPos - startPos);
    }

    private static Span GetUpdateBounds(Span oldBounds, Span newBounds)
    {
      if (oldBounds.IsEmpty)
        return newBounds;

      if (newBounds.IsEmpty)
        return oldBounds;

      var startPos = Math.Min(oldBounds.Start, newBounds.Start);
      var endPos   = Math.Max(oldBounds.End, newBounds.End);
      return new Span(startPos, endPos - startPos);
    }

    private ClassificationSpan MakeClassificationSpan(ITextSnapshot snapshot, GotoInfo gotoInfo)
    {
      var span = Utils.NLocationToSpan(snapshot, gotoInfo.Location);
      var classificationType = gotoInfo.UsageType == UsageType.Usage
        ? _highlightOne
        : _highlightTwo;
      return new ClassificationSpan(new SnapshotSpan(snapshot, span), classificationType);
    }
  }
}
