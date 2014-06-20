using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.LanguageService
{
  public sealed partial class SyntaxClassifier : IClassifier
  {
    private readonly IClassificationType[]     _classificationTypes;
    private readonly ITextBuffer               _textBuffer;
    private          ClassificationParseResult _lastParseResult;
    private          IEnumerable<ITextChange>  _lastTextChanges;

    public SyntaxClassifier(
      IStandardClassificationService standardClassification,
      IClassificationTypeRegistryService classificationRegistry,
      ITextBuffer textBuffer)
    {
      _classificationTypes = new IClassificationType[]
      {
        standardClassification.WhiteSpace,
        standardClassification.Identifier,
        standardClassification.Keyword,
        standardClassification.PreprocessorKeyword,
        standardClassification.Operator,
        standardClassification.NumberLiteral,
        standardClassification.CharacterLiteral,
        standardClassification.Comment,
        standardClassification.Comment,
        standardClassification.StringLiteral,
        classificationRegistry.GetClassificationType(ClassificationTypes.VerbatimStringName),
        classificationRegistry.GetClassificationType(ClassificationTypes.RecursiveStringName),
        classificationRegistry.GetClassificationType(ClassificationTypes.QuotationName),
        classificationRegistry.GetClassificationType(ClassificationTypes.QuotationBracesName),
        classificationRegistry.GetClassificationType(ClassificationTypes.ToDoCommentName),
        classificationRegistry.GetClassificationType(ClassificationTypes.BugCommentName),
        classificationRegistry.GetClassificationType(ClassificationTypes.HackCommentName),
      };
      _textBuffer = textBuffer;
      _textBuffer.Changed += TextBuffer_Changed;
    }

    public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;

    public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
    {
      var parseResult = _lastParseResult;
      if (parseResult == null)
        if (ParseUtil.TryParse(_textBuffer, out parseResult))
          _lastParseResult = parseResult;
        else
          return ClassifierUtils.EmptyClassifications;

      var textChanges = _lastTextChanges;
      if (textChanges != null)
      {
        _lastTextChanges = null;
        var handler = ClassificationChanged;
        if (handler != null)
          foreach (var s in SearchClassificationChanges(parseResult, textChanges.Select(c => c.NewSpan)))
            if (!(span.Span.Start < s.Start && s.End <= span.Span.End))
              handler(this, new ClassificationChangedEventArgs(new SnapshotSpan(parseResult.Snapshot, s)));
      }

      var tokenSpans = ParseUtil.GetTokenSpans(_lastParseResult, span.Span);

      var result = new ClassificationSpan[tokenSpans.Count];
      for (var i = 0; i < tokenSpans.Count; ++i)
      {
        var spanInfo = tokenSpans[i];
        result[i] = new ClassificationSpan(new SnapshotSpan(_lastParseResult.Snapshot, spanInfo.Span), _classificationTypes[(int)spanInfo.Type]);
      }
      return result;
    }

    private void TextBuffer_Changed(object sender, TextContentChangedEventArgs e)
    {
      var parseResult = _lastParseResult;
      if (parseResult != null)
      {
        var handler = ClassificationChanged;
        if (handler != null)
          foreach (var s in SearchClassificationChanges(parseResult, e.Changes.Select(c => c.OldSpan)))
            handler(this, new ClassificationChangedEventArgs(new SnapshotSpan(parseResult.Snapshot, s)));
      }
      _lastParseResult = null;
      _lastTextChanges = e.Changes;
    }

    private static List<Span> SearchClassificationChanges(ClassificationParseResult parseResult, IEnumerable<Span> changes)
    {
      var spansToRedraw = new List<Span>();
      foreach (var c in changes)
      {
        var tokenSpans = ParseUtil.GetTokenSpans(parseResult, c);
        foreach (var si in tokenSpans)
        {
          switch (si.Type)
          {
            case SpanType.VerbatimString:
            case SpanType.RecursiveString:
            case SpanType.MultiLineComment:
              spansToRedraw.Add(si.Span);
              break;

            case SpanType.PreprocessorKeyword:
              spansToRedraw.Add(new Span(si.Span.Start, parseResult.Snapshot.Length - si.Span.Start));
              break;
          }
        }
      }
      return spansToRedraw;
    }

  }
}
