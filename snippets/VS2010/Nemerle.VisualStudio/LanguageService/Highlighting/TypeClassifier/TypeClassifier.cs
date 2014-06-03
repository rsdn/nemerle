using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Nemerle.Compiler;
using Nemerle.VisualStudio.Project;

namespace Nemerle.VisualStudio.LanguageService.Highlighting.TypeClassifier
{
  public sealed class TypeClassifier : IClassifier
  {
    private readonly IStandardClassificationService _standardClassification;
    private readonly IClassificationTypeRegistryService _classificationRegistry;
    private readonly ITextBuffer _textBuffer;


    public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;

    public TypeClassifier(
      IStandardClassificationService standardClassification,
      IClassificationTypeRegistryService classificationRegistry,
      ITextBuffer textBuffer)
    {
      _standardClassification = standardClassification;
      _classificationRegistry = classificationRegistry;
      _textBuffer = textBuffer;
      _textBuffer.Changed += TextBuffer_Changed;
    }


    private void TextBuffer_Changed(object sender, TextContentChangedEventArgs e)
    {
    }

    public void RedrawTypeHighlighting()
    {
      var currentSnapshot = _textBuffer.CurrentSnapshot;
      OnClassificationChanged(new SnapshotSpan(currentSnapshot, 0, currentSnapshot.Length));
    }

    private void OnClassificationChanged(SnapshotSpan span)
    {
      var handler = ClassificationChanged;
      if (null != handler)
        handler(this, new ClassificationChangedEventArgs(span));
    }

    public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
    {
      NemerleSource source;
      var result               = new List<ClassificationSpan>();
      if (!_textBuffer.Properties.TryGetProperty(typeof(NemerleSource), out source))
        return result;
      source.TypeClassifier = this;
      var fileIndex            = source.FileIndex;
      var snapshot             = span.Snapshot;
      var loc                  = Utils.ToNLocation(fileIndex, span);
      var locationsToHighlight = GetLocationsToHighlight(source, loc).ToArray();

      foreach (var location in locationsToHighlight)
      {
        var highlightSpan = Utils.NLocationToSpan(snapshot, location);
        result.Add(new ClassificationSpan(new SnapshotSpan(snapshot, highlightSpan), _standardClassification.SymbolReference));
      }
      
      return result;
    }

    private static IEnumerable<Location> GetLocationsToHighlight(NemerleSource source, Location loc)
    {
      return source.TypeLocations.Where(tl => tl.IsIntersect(loc));
    }


    private static ClassificationSpan MakeClassificationSpan(ITextSnapshot textSnapshot, Span span, IClassificationType classificationType)
    {
      return new ClassificationSpan(new SnapshotSpan(textSnapshot, span), classificationType);
    }

    private static readonly IList<ClassificationSpan> EmptyClassificationSpans = new ClassificationSpan[0];
  }
}
