using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;
using System.Diagnostics;

namespace Nemerle.VisualStudio.LanguageService.Highlighting.TypeClassifier
{
  [Export(typeof(IClassifierProvider)), ContentType("nemerle")]
  public sealed class TypeClassifierProvider : IClassifierProvider
  {
    [Import]
    private IClassificationTypeRegistryService _classificationRegistry = null;

    [Import]
    private IStandardClassificationService _standardClassification = null;

    public IClassifier GetClassifier(ITextBuffer textBuffer)
    {
      Debug.Assert(_classificationRegistry != null);
      Debug.Assert(_standardClassification != null);

      var classifierKey = typeof(TypeClassifier);
      TypeClassifier classifier;
      if (!textBuffer.Properties.TryGetProperty(classifierKey, out classifier))
      {
        classifier = new TypeClassifier(_standardClassification, _classificationRegistry, textBuffer);
        textBuffer.Properties.AddProperty(classifierKey, classifier);
      }
      return classifier;
    }
  }
}
