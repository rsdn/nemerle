using System;
using System.Collections.Generic;
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
    private ParseResult _parseResult;

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

    public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;

    private void TextBuffer_Changed(object sender, TextContentChangedEventArgs e)
    {
    }

    private void OnClassificationChanged(SnapshotSpan span)
    {
      var handler = ClassificationChanged;
      if (null != handler)
        handler(this, new ClassificationChangedEventArgs(span));
    }

    public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
    {
      var result = new List<ClassificationSpan>();

      var path = _textBuffer.GetFilePath();
      var fileIndex = Location.GetFileIndex(path);
      //var projectInfo = ProjectInfo.FindProject(path);
      //projectInfo.Engine.;
      NemerleSource source;
      if (!_textBuffer.Properties.TryGetProperty<NemerleSource>(typeof(NemerleSource), out source))
        return result;

      var engine = source.GetEngine();
      if (!engine.RequestOnInitEngine())
        return result;
      var manager = (ManagerClass) engine;

      foreach (var typeBuilder in manager.Hierarchy.TopTypeBuilders())
      {
        foreach (var ast in typeBuilder.AstParts)
        {
          if (ast.Location.FileIndex == fileIndex)
          {
            
          }
        }
      }


      return result;
    }


    private static ClassificationSpan MakeClassificationSpan(ITextSnapshot textSnapshot, Span span, IClassificationType classificationType)
    {
      return new ClassificationSpan(new SnapshotSpan(textSnapshot, span), classificationType);
    }

    private static readonly IList<ClassificationSpan> EmptyClassificationSpans = new ClassificationSpan[0];
  }
}
