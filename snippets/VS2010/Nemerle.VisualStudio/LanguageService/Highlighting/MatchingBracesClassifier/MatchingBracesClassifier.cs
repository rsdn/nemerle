using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Nemerle.VisualStudio.LanguageService
{
  public sealed class MatchingBracesClassifier : IClassifier
  {
    public MatchingBracesClassifier(IClassificationTypeRegistryService classificationRegistry, ITextBuffer textBuffer)
    {
    }

    public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;

    public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
    {
      return new ClassificationSpan[0];
    }

    public void UpdateClassifications(CaretPosition caretPosition)
    {
      
    }
  }
}
