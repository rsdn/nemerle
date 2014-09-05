/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Diagnostics;
using System.Windows.Forms;

using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.TextManager.Interop;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using Microsoft.VisualStudio;
using Nemerle.Completion2;

namespace Nemerle.VisualStudio.LanguageService
{
  internal sealed class NemerleCompletionSet : CompletionSet
  {
    public NemerleSource Source { get; private set; }
    internal IVsTextView view;

    internal NemerleCompletionSet(ImageList imageList, NemerleSource source)
      : base(imageList, source)
    {
      Source = source;
    }

    public override void Init(IVsTextView textView, Declarations declarations, bool completeWord)
    {
      view = textView;
      base.Init(textView, declarations, completeWord);
    }

    public override int GetInitialExtent(out int line, out int startIdx, out int endIdx)
    {
      var source =  (NemerleSource)this.Source;
      var textView = view.ToITextView();
      var textBuffer = textView.TextBuffer;
      var textViewLine = textView.Caret.ContainingTextViewLine;
      var snapshot = textBuffer.CurrentSnapshot;

      var caretPos = textView.Caret.Position.BufferPosition.Position;
      var caretSpan = new Span(caretPos, 0);
      //textView.Li
      ClassificationParseResult parseResult;
      if (ParseUtil.TryParse(textBuffer, out parseResult))
      {
        var spanInfos = ParseUtil.GetTokenSpans(parseResult, textViewLine.Extent);
        foreach (var spanInfo in spanInfos)
          if (spanInfo.Span.IntersectsWith(caretSpan))
            return GetTokenExtent(source, snapshot, spanInfo, out line, out startIdx, out endIdx);
      }

      return GetDefaultExtent(out line, out startIdx, out endIdx);
    }

    private int GetTokenExtent(NemerleSource source, ITextSnapshot snapshot, SyntaxClassifier.SpanInfo spanInfo, out int line, out int startIdx, out int endIdx)
    {
      switch (spanInfo.Type)
      {
        case SyntaxClassifier.SpanType.String:
        case SyntaxClassifier.SpanType.RecursiveString:
        case SyntaxClassifier.SpanType.VerbatimString:
          // TODO: implement logic for $" $(x.|)  "
          goto default;

        case SyntaxClassifier.SpanType.Operator:
          if (!spanInfo.Span.IsEmpty && snapshot[spanInfo.Span.End - 1] == '.')
            return GetDefaultExtent(out line, out startIdx, out endIdx);
          else
            goto default;

        default:
          var loc = Utils.ToNLocation(source.FileIndex, new SnapshotSpan(snapshot, spanInfo.Span));
          Debug.Assert(loc.Line == loc.EndLine);
          line = loc.Line - 1;
          startIdx = loc.Column - 1;
          endIdx = loc.EndColumn - 1;
          return VSConstants.S_OK;
      }
    }

    private int GetDefaultExtent(out int line, out int startIdx, out int endIdx)
    {
      int column;
      ErrorHelper.ThrowOnFailure(this.view.GetCaretPos(out line, out column));
      startIdx = endIdx = column;
      return VSConstants.S_OK;
    }

    public override int OnCommit(string textSoFar, int index, int selected, ushort commitChar, out string completeWord)
    {
      try
      {
        var decls = (NemerleDeclarations)Declarations;

        if (decls.Result.ImportCompletion)
        {
          var env = decls.Result.CompletionResult.Env;
          var elem = decls.Result.CompletionResult.CompletionList[index];

          var usingInfo = NemerleCompletionResult.CalcUsingDeclarationInfo(env, elem.Overloads[0]);

          if (!usingInfo.NeedUsing && !usingInfo.Hiden && string.IsNullOrEmpty(usingInfo.Alias))
            return base.OnCommit(textSoFar, index, selected, commitChar, out completeWord);

          if (!string.IsNullOrEmpty(usingInfo.Alias))
          {
            var result = base.OnCommit(textSoFar, index, selected, commitChar, out completeWord);
            completeWord = usingInfo.Alias + "." + completeWord;
            return result;
          }

          if (usingInfo.Hiden)
          {
            var result = base.OnCommit(textSoFar, index, selected, commitChar, out completeWord);
            completeWord = usingInfo.Namespase + "." + completeWord;
            return result;
          }

          if (usingInfo.NeedUsing)
          {
            var result = base.OnCommit(textSoFar, index, selected, commitChar, out completeWord);
            
            if (result == VSConstants.S_OK)
            {
              var cu = Source.CompileUnit;

              var line = cu != null
                ? NemerleCompletionResult.CalcUsingDeclarationInsertionLine(usingInfo.Namespase, cu) - 1
                : 0;
              //if (Source.CompletedFirstParse && cu == null)
              Source.SetText(line, 0, line, 0, "using " + usingInfo.Namespase + ";" + Environment.NewLine);
            }

            return result;
          }
        }
      }
      catch (Exception)
      {
      }
      return base.OnCommit(textSoFar, index, selected, commitChar, out completeWord);
    }
  }
}

