using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using System;
using System.ComponentModel.Composition;
using System.IO;

namespace Nemerle.VisualStudio.LanguageService
{
  [Export(typeof(IClassifierProvider)), ContentType("code")]
  public sealed class SyntaxClassifierProvider : IClassifierProvider
  {
    [Import]
    private IClassificationTypeRegistryService _classificationRegistry = null;

    [Import]
    private IStandardClassificationService _standardClassification = null;

    public IClassifier GetClassifier(ITextBuffer textBuffer)
    {
      var extension = Path.GetExtension(textBuffer.GetFilePath());
      var isNemerleFile = string.Compare(extension, NemerleConstants.FileExtension, StringComparison.OrdinalIgnoreCase) == 0;
      if (isNemerleFile)
      {
        var classifierKey = typeof(SyntaxClassifier);
        SyntaxClassifier classifier;
        if (!textBuffer.Properties.TryGetProperty(classifierKey, out classifier))
        {
          classifier = new SyntaxClassifier(_standardClassification, _classificationRegistry, textBuffer);
          textBuffer.Properties.AddProperty(classifierKey, classifier);
        }
        return classifier;
      }
      else
        return null;
    }
  }
}
