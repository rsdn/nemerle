using System.ComponentModel.Composition;
using System.Diagnostics;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;

namespace Nemerle.VisualStudio.LanguageService
{
  [Export(typeof(IViewTaggerProvider)), ContentType("nemerle"), TagType(typeof(TextMarkerTag))]
  public sealed class MatchingBracesTaggerProvider : IViewTaggerProvider
  {
    public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer textBuffer) where T : ITag
    {
      Debug.Assert(typeof(T).IsAssignableFrom(typeof(TextMarkerTag)));

      var taggerKey = typeof(MatchingBracesTagger);
      object tagger;
      if (!textBuffer.Properties.TryGetProperty(taggerKey, out tagger))
      {
        tagger = new MatchingBracesTagger(textView, textBuffer);
        textBuffer.Properties.AddProperty(taggerKey, tagger);
      }
      return (ITagger<T>)tagger;
    }
  }
}
