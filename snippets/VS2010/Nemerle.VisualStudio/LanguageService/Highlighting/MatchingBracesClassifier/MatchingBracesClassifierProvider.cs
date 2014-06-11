using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;
using System.Diagnostics;

namespace Nemerle.VisualStudio.LanguageService
{
  [Export(typeof(IClassifierProvider)), ContentType("nemerle")]
  public sealed class MatchingBracesClassifierProvider : IClassifierProvider
  {
    [Import]
    private IClassificationTypeRegistryService _classificationRegistry = null;

    public IClassifier GetClassifier(ITextBuffer textBuffer)
    {
      Debug.Assert(_classificationRegistry != null);

      var classifierKey = typeof(MatchingBracesClassifier);
      MatchingBracesClassifier classifier;
      if (!textBuffer.Properties.TryGetProperty(classifierKey, out classifier))
      {
        classifier = new MatchingBracesClassifier(_classificationRegistry, textBuffer);
        textBuffer.Properties.AddProperty(classifierKey, classifier);
      }
      return classifier;
    }
  }
}
