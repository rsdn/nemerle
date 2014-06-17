using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;

namespace Nemerle.VisualStudio.LanguageService
{
  [Export(typeof(IWpfTextViewCreationListener)), ContentType("nemerle"), TextViewRole(PredefinedTextViewRoles.Document)]
  public sealed class MatchingBracesTextViewCreationListener : IWpfTextViewCreationListener
  {
    public void TextViewCreated(IWpfTextView textView)
    {
      MatchingBracesClassifier classifier;
      if (textView.TextBuffer.Properties.TryGetProperty(typeof(MatchingBracesClassifier), out classifier))
      {
        textView.Caret.PositionChanged += (_, args) => classifier.UpdateClassifications(args.NewPosition);
        textView.TextBuffer.PostChanged += (_, args) => classifier.UpdateClassifications(textView.Caret.Position);
      }
    }
  }
}
