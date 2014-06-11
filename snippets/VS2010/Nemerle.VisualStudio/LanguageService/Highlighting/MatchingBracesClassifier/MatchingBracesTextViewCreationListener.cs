using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;

namespace Nemerle.VisualStudio.LanguageService
{
  [Export(typeof(IWpfTextViewCreationListener)), ContentType("nemerle"), TextViewRole(PredefinedTextViewRoles.Document)]
  public sealed class MatchingBracesTextViewCreationListener : IWpfTextViewCreationListener
  {
    public void TextViewCreated(IWpfTextView textView)
    {
      MatchingBracesClassifier classifier;
      if (textView.TextBuffer.Properties.TryGetProperty<MatchingBracesClassifier>(typeof(MatchingBracesClassifier), out classifier))
        textView.Caret.PositionChanged += (_, args) => classifier.UpdateClassifications(args.NewPosition);
    }
  }
}
