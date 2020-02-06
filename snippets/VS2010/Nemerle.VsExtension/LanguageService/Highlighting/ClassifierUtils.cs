using System.Collections.Generic;
using Microsoft.VisualStudio.Text.Classification;

namespace Nemerle.VisualStudio.LanguageService
{
  public static class ClassifierUtils
  {
    public static readonly IList<ClassificationSpan> EmptyClassifications = new ClassificationSpan[0];
  }
}
