using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Completion2;
using Nemerle.Compiler;
using Nemerle.Completion2.CodeFormatting;
using Nemerle.VisualStudio.Project;
using Nemerle.VisualStudio.GUI;

using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Package;

namespace Nemerle.VisualStudio.LanguageService
{
	class NemerleViewFilter : ViewFilter
	{
		public NemerleViewFilter(CodeWindowManager mgr, IVsTextView view)
			: base(mgr, view)
		{
		}

		public override void OnSetFocus(IVsTextView view)
		{
			ShowAst(view, true);
			base.OnSetFocus(view);
		}

		private void ShowAst(IVsTextView view, bool showInfo)
		{
			NemerleSource source = Source as NemerleSource;
			if (source != null && source.ProjectInfo != null)
			{
				int line, col;
				ErrorHandler.ThrowOnFailure(view.GetCaretPos(out line, out col));
				//Debug.WriteLine(
				//		string.Format("OnChangeScrollInfo line={0}, col={1}", line + 1, col + 1));
				AstToolWindow tw = (AstToolWindow)source.ProjectInfo.ProjectNode.Package
					.FindToolWindow(typeof(AstToolWindow), 0, true);

				if (showInfo)
					tw.ShowInfo(source);

				tw.Activate(line + 1, col + 1);
			}
		}

		public override void OnChangeScrollInfo(IVsTextView view, int iBar,
			int iMinUnit, int iMaxUnits, int iVisibleUnits, int iFirstVisibleUnit)
		{
			base.OnChangeScrollInfo(view, iBar, iMinUnit, iMaxUnits, iVisibleUnits, iFirstVisibleUnit);
			Source.TryHighlightBraces(view);
			ShowAst(view, false);
		}

		protected override int ExecCommand(ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
		{
			// hi_octane : found a lot of mistakes comparing the switch
			// ids with PkgCmdID.h and NemerleMenus.cs
			// decided modify the code
			// leaving only two files required to be synchronized

			string txt = null;
			switch((MenuCmd.CmdId)nCmdId)
			{
				case MenuCmd.CmdId.SetAsMain: 
					txt = "cmdidSetAsMain"; 
					break;
				case MenuCmd.CmdId.ExtendSelection:
					// cmdIdExtendSelection
					ExpandSelection();
					// it's prevent repeated execution of comand in base.ExecCommand() 
					return VSConstants.S_OK;
				case MenuCmd.CmdId.ShrinkSelection:
					// cmdIdShrinkSelection
					ShrinkSelection();
					return VSConstants.S_OK;
				case MenuCmd.CmdId.FindInheritors: //cmdIdFindInheritors 
				case MenuCmd.CmdId.FindInheritorsCtxt: //cmdIdFindInheritorsCtxt
					FindInheritors();
					return VSConstants.S_OK;
				case MenuCmd.CmdId.Rename: 
					txt = "cmdIdRename";
					RunRenameRefactoring();
					return VSConstants.S_OK;
				case MenuCmd.CmdId.Inline: // cmdIdInline
					RunInlineRefactoring();
					return VSConstants.S_OK;
				case MenuCmd.CmdId.Options:
					// cmdIdOptions
					ShowOptions();
					return VSConstants.S_OK;
				case MenuCmd.CmdId.AstToolWindow: // AstToolWindow
					{
						//this.CodeWindowManager.LanguageService.
						NemerleSource source = Source as NemerleSource;
						if (source != null)
							source.ProjectInfo.ProjectNode.Package.OnAstToolWindowShow(null, null);
					}
					return VSConstants.S_OK;
				case MenuCmd.CmdId.AddHighlighting: // cmdIdAddHighlighting
					HighlightSymbol();
					return VSConstants.S_OK;
				case MenuCmd.CmdId.ESC: // ESC
				case MenuCmd.CmdId.RemoveLastHighlighting: // cmdIdRemoveLastHighlighting
					RemoveLastHighlighting();
					if(nCmdId == (int)MenuCmd.CmdId.ESC) // ESC
						break; // go trocess ESC
					return VSConstants.S_OK;
				case MenuCmd.CmdId.GoToFile:
					using (var frm = new GoToFileForm())
					{
						var dte = Source.ProjectInfo.ProjectNode.ProjectMgr.Site.GetService(typeof(EnvDTE.DTE)) as EnvDTE.DTE;
						if (dte != null)
						{
							var names = CollectFileNames(dte);

							frm.SetFiles(names);
							frm.ShowDialog(TextEditorWindow);
							if (frm.SelectedFileName != null)
							{
								var srv = Source.LanguageService as NemerleLanguageService;
								if (srv != null)
								{
									var loc = new Location(frm.SelectedFileName, 1, 0, 3, 10);
									srv.GotoLocation(loc);
								}
							}
						}
					}
					return VSConstants.S_OK;
				case MenuCmd.CmdId.SourceOutlinerWindow:
					{
						if(Source != null)
							Source.ProjectInfo.ProjectNode.Package.OnSourceOutlinerWindowShow(null, null);
					}
					return VSConstants.S_OK;
			}

			Trace.Assert(txt == null, "Implement the menu!\r\nID: " + txt);


			return base.ExecCommand(ref guidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut);
		}

		private IEnumerable<string> CollectFileNames(EnvDTE.DTE dte)
		{
			List<string> result = new List<string>();
			foreach (EnvDTE.Project prj in dte.Solution.Projects)
			{
				result.AddRange(CollectFileNamesRecursively(prj.ProjectItems));
			}
			return result;
		}

		private IEnumerable<string> CollectFileNamesRecursively(EnvDTE.ProjectItems items)
		{
			if (items != null)
			{
				foreach (EnvDTE.ProjectItem item in items)
				{
					if (!(item is OAFileItem || item is OAFolderItem))
						continue;

					for (short i = 1; i < item.FileCount; ++i)
						yield return item.get_FileNames(i);
					//yield return fileItem..Name;
					foreach (var s in CollectFileNamesRecursively(item.ProjectItems))
						yield return s;
				}
			}
		}

		private List<Location> _selectionsStack;

		private int _currentSelection;
		public  int  CurrentSelection
		{
			get { return _currentSelection; }
			set
			{
				if (_selectionsStack != null && value != _currentSelection && value >= 0 && value < _selectionsStack.Count)
				{
					_currentSelection = value;
					TextSpan span = Utils.SpanFromLocation(_selectionsStack[_currentSelection]);
					TextView.SetSelection(span.iEndLine, span.iEndIndex, span.iStartLine, span.iStartIndex);
				}
			}
		}

		public new NemerleSource Source
		{
			get { return (NemerleSource)base.Source; }
		}

		public IWin32Window TextEditorWindow
		{
			get { return NativeWindow.FromHandle(TextView.GetWindowHandle()); }
		}

		private void ExpandSelection()
		{
			TextSpan selection = GetSelection();
			if (!SelectionIsInStack(selection))
			{
				_selectionsStack = GetSelectionsStack(selection);
				AddWordSpan(selection);
				_currentSelection = _selectionsStack.Count;
			}
			CurrentSelection--;
		}

		private void ShrinkSelection()
		{
			if (SelectionIsInStack(GetSelection()))
				CurrentSelection++;
		}

		private void HighlightSymbol()
		{
			NemerleSource source = Source;

			if (source != null)
			{
				TextSpan span = GetSelection();
				if (source.ProjectInfo == null)
					throw new ApplicationException("Source.ProjectInfo equals null.");
				source.ProjectInfo.HighlightUsages(source.FileIndex,
													 span.iStartLine,
													 span.iStartIndex,
													 new SourceTextManager(source),
													 true);
			}
		}

		private void RemoveLastHighlighting()
		{
			if (Source != null && Source.ProjectInfo != null)
				Source.ProjectInfo.RemoveLastHighlighting(new SourceTextManager(Source));
		}

		private bool WarnAboutErrors(Nemerle.Completion2.Project project)
		{
			foreach (var cm in project.Errors)
				if (cm.Kind == MessageKind.Error)
					return MessageBox.Show(TextEditorWindow, "This project doesn't build. Are you sure you want to proceed?",
										"",
										MessageBoxButtons.YesNo,
										MessageBoxIcon.Exclamation) == DialogResult.Yes;
			return true;
		}

		private void RunInlineRefactoring()
		{
			if (Source == null) return;
			var proj = Source.ProjectInfo.Project;

			if(!WarnAboutErrors(proj)) return;

			int lineIndex;
			int colIndex;
			TextView.GetCaretPos(out lineIndex, out colIndex);
			GotoInfo[] definitions = proj.GetDefinition(Source.GetFilePath(),
																	 lineIndex + 1,
																	 colIndex);
			if (definitions == null || definitions.Length == 0)
				return;

			if (definitions.Length > 1)
			{
				MessageBox.Show(TextEditorWindow, "More than one definition found. Cannot inline.");
				return;
			}

			if (proj.CanInlineExpressionAt(definitions[0]))
			{
				HighlightSymbol();

				using (var undoTransaction = new LinkedUndoTransaction("Inline refactoring", Source.Service.Site))
				{
					// replacing usages with initializer
					var tuple = proj.GetReplacementStringForInline(definitions[0]);
					var defLocation = tuple.Field0;
					var initLocation = tuple.Field1;
					var replacement = Source.GetText(Utils.SpanFromLocation(initLocation));
					var shouldEmbrace = tuple.Field2;
					var usages = proj.GetUsages(Source.GetFilePath(),
														 lineIndex + 1,
														 colIndex)
											.Where(usage => usage.UsageType == UsageType.Usage)
											.ToArray();
					using (var frm = new InlineRefactoringPreview(proj))
					{
						frm.Usages = usages;
						frm.ExpressionToInline = shouldEmbrace
													? string.Format("({0})", replacement)
													: replacement;
						if (frm.ShowDialog(TextEditorWindow) != DialogResult.OK)
						{
							RemoveLastHighlighting();
							return;
						}
					}
					Source.RenameSymbols(replacement, usages, false);

					// replacing definition with empty string
					Source.RenameSymbols("", new[] { new GotoInfo(defLocation) });

					Source.DeleteEmptyStatementAt(defLocation.Line - 1);

					undoTransaction.Commit();
				}
				RemoveLastHighlighting();
			}
			else
			{
				MessageBox.Show(TextEditorWindow, "Only simple def's can be inlined at the moment.");
			}
		}

		private void RunRenameRefactoring()
		{
			if (Source == null) return;
			var proj = Source.ProjectInfo.Project;
			if(!WarnAboutErrors(proj)) return;

			int lineIndex;
			int colIndex;
			TextView.GetCaretPos(out lineIndex, out colIndex);
			var allUsages = proj.GetUsages(Source.GetFilePath(), lineIndex + 1, colIndex);
			var usages = allUsages.Distinct().ToArray();
			if (usages == null || usages.Length == 0)
				return;

			var definitionCount = usages.Count(usage => usage.UsageType == UsageType.Definition);
			if(definitionCount == 0)
			{
				MessageBox.Show(TextEditorWindow, "Cannot find definition.");
				return;
			}
			if(definitionCount > 1)
			{
				MessageBox.Show(TextEditorWindow, "More than one definition found. Must be error in Find Usages.");
				return;
			}

			using (var frm = new RenameRefactoringDlg(proj, usages))
			{
				if (frm.ShowDialog(TextEditorWindow) == DialogResult.OK)
				{
					Source.RenameSymbols(frm.NewName, usages);
				}
			}
			RemoveLastHighlighting();
		}

		private void FindInheritors()
		{
			if (Source == null)
				return;

			NemerleLanguageService ourLanguageService = Source.LanguageService as NemerleLanguageService;
			if (ourLanguageService == null)
				return;

			int lineIndex;
			int colIndex;
			TextView.GetCaretPos(out lineIndex, out colIndex);
			GotoInfo[] infos =
				Source.ProjectInfo.Project.GetInheritors(Source.FileIndex, lineIndex + 1, colIndex + 1);

			// If we have only one found usage, then jump directly to it.
			if (infos.Length == 1)
				ourLanguageService.GotoLocation(infos[0].Location);
			else if (infos.Length > 0) // otherwise show a form to let user select entry manually
			{
				using (GotoUsageForm popup = new GotoUsageForm(infos))
					if (popup.ShowDialog(TextEditorWindow) == DialogResult.OK)
						ourLanguageService.GotoLocation(popup.Result.Location);
			}
		}

		private bool SelectionIsInStack(TextSpan selection)
		{
			// perhaps, using OnSelectChange routine would be better
			// to determine, whether user already moved selection or still in the selection chain
			if (_selectionsStack == null || _selectionsStack.Count == 0)
				return false;
			Location current = Utils.LocationFromSpan(_selectionsStack[0].FileIndex, selection);

			int i = _selectionsStack.IndexOf(current);
			if (i >= 0)
			{
				_currentSelection = i;
				return true;
			}

			return false;
		}

		private void ShowOptions()
		{
			using (Options options = new Options())
			{
				if (options.ShowDialog(TextEditorWindow) == DialogResult.OK)
					options.Save();
			}
		}

		private void AddWordSpan(TextSpan selection)
		{
			TextSpan[] spans = new TextSpan[1];
			// HACK: have seen 4 as usual flags value for GetWordExtent ;)
			uint flags = 4;
			GetWordExtent(selection.iStartLine, selection.iStartIndex, flags, spans);
			TextSpan wordSpan = spans[0];
			GetWordExtent(selection.iEndLine, selection.iEndIndex, flags, spans);
			TextSpan wordSpanOther = spans[0];
			// in principle, selectionsStack contains at least the whole file location
			if (wordSpan.Equals(wordSpanOther) && _selectionsStack != null && _selectionsStack.Count > 0)
			{
				Location last = _selectionsStack[_selectionsStack.Count - 1];
				Location word = Utils.LocationFromSpan(last.FileIndex, wordSpan);
				if (last.StrictlyContains(word) && last != word)
					_selectionsStack.Add(word);
			}
			else
			{
				_selectionsStack = new List<Location>();
				_selectionsStack.Add(Utils.LocationFromSpan(0, wordSpan));
			}
		}

		private List<Location> GetSelectionsStack(TextSpan span)
		{
			NemerleSource source = Source as NemerleSource;
			if (source != null)
			{
				Location current = Utils.LocationFromSpan(source.FileIndex, span);
				List<Location> stack = new List<Location>();
				stack.AddRange(source.ProjectInfo.Project.GetEnclosingLocationsChain(current).ToArray());
				return stack;
			}
			else
				return null;
		}

		//public override int GetWordExtent(int line, int index, uint flags, TextSpan[] span)
		//{
    //	System.Diagnostics.Debug.Assert(false, "line " + line + " index " + index + " flags " + flags + " span length " + span.Length);

		//	base.GetWordExtent(line, index, flags, span);
		//	return 0;
		//}

		protected override int QueryCommandStatus(ref Guid guidCmdGroup, uint nCmdId)
		{
			if (guidCmdGroup == VSConstants.VSStd2K &&
				nCmdId == (uint)VSConstants.VSStd2KCmdID.INSERTSNIPPET ||
				nCmdId == (uint)VSConstants.VSStd2KCmdID.SURROUNDWITH)
			{
				return (int)(OLECMDF.OLECMDF_SUPPORTED | OLECMDF.OLECMDF_ENABLED);
			}

			return base.QueryCommandStatus(ref guidCmdGroup, nCmdId);
		}

		int _startLine;
		int _startPos;

		public override void CommentSelection()
		{
			TextSpan ts = GetRealSelectionSpan();
			Source.CommentLines(ts, "//");
		}
		public override void UncommentSelection()
		{
			TextSpan ts = GetRealSelectionSpan();
			Source.UncommentLines(ts, "//");
		}

		private TextSpan GetRealSelectionSpan()
		{
			// workaround for bug #1050
			// TextView.GetSelection adds caret position to the selection span,
			// so when we select whole block of text (caret is at the first column),
			// one extra line is added to the selection (where the caret is left)
			TextSpan ts = new TextSpan();
			TextView.GetSelection(out ts.iStartLine, out ts.iStartIndex, out ts.iEndLine, out ts.iEndIndex);
			if (ts.iEndIndex == 0 && ts.iEndLine - ts.iStartLine > 0)
				ts.iEndLine--;
			return ts;
		}

		public override bool HandlePreExec(
			ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
		{
			VsCommands2K cmd = (VsCommands2K)nCmdId;

			// we're goona to erase some symbol from existence. 
			// In some cases we need to know what it was (auto-deletion of paired token)
			if (cmd == VsCommands2K.BACKSPACE)
				Source.RememberCharBeforeCaret(TextView);
			else
				Source.ClearRememberedChar();

			_startLine = -1;

			if (guidCmdGroup == VSConstants.VSStd2K)
			{
				if (Source.MethodData.IsDisplayed)
					TextView.GetCaretPos(out _startLine, out _startPos);



				switch (cmd)
				{
					case VsCommands2K.COMPLETEWORD:
						{
							int line;
							int col;
							ErrorHandler.ThrowOnFailure(TextView.GetCaretPos(out line, out col));
							var tokenInfo = Source.GetTokenInfo(line, col);
							Source.Completion(TextView, tokenInfo, ParseReason.CompleteWord);
							return true;
						}
					case VsCommands2K.FORMATSELECTION:
						ReformatSelection();
						return true;

					case VsCommands2K.INSERTSNIPPET:
						{
							ExpansionProvider ep = GetExpansionProvider();

							if (TextView != null && ep != null)
								ep.DisplayExpansionBrowser(TextView, Resources.InsertSnippet, null, false, null, false);

							return true; // Handled the command.
						}
					case VsCommands2K.SURROUNDWITH:
						{
							ExpansionProvider ep = GetExpansionProvider();

							if (TextView != null && ep != null)
								ep.DisplayExpansionBrowser(TextView, Resources.SurroundWith, null, false, null, false);

							return true; // Handled the command.
						}

					case VsCommands2K.UP:
						if (Source.MethodData.IsDisplayed && Source.MethodData.GetCurMethod() == 0)
						{
							int count = Source.MethodData.GetOverloadCount();

							if (count > 1)
							{
								while (Source.MethodData.NextMethod() < count - 1)
									Source.MethodData.UpdateView();

								return true;
							}
						}

						break;

					case VsCommands2K.DOWN:
						if (Source.MethodData.IsDisplayed)
						{
							int count = Source.MethodData.GetOverloadCount();

							if (count > 1 && Source.MethodData.GetCurMethod() == count - 1)
							{
								while (Source.MethodData.PrevMethod() > 0)
									Source.MethodData.UpdateView();

								return true;
							}
						}

						break;
				}
			}

			// Base class handled the command.  Do nothing more here.
			//
			return base.HandlePreExec(ref guidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut);
		}

		// TODO: Implement smart indention
		// 1. When typing open curly brace (second is entered automatically) and 
		//		then pressing enter
		// 2. When typing enter between sentences.
		// 3. When typing enter in the middle of expression.
		public override bool HandleSmartIndent()
		{
			// TODO: Minimal functionality is to find what caret position should be,
			// and to insert needed whitespace to move all the text that is after caret
			// to the position after preferred caret position.

            //int line;
            //int idx;
            //TextView.GetCaretPos(out line, out idx);

            //string filePath = Source.GetFilePath();
            //ProjectInfo projectInfo = ProjectInfo.FindProject(filePath);
            //Engine engine = projectInfo.Engine;

            //List<FormatterResult> results = Formatter.FormatExpressionAt(engine, filePath, line + 1, idx + 1);
            //ApplyFormatterResults(results);

			//MessageBox.Show(TextEditorWindow, "Caret pos in HandleSmartIndent: " + line + ":" + col);
			return false;
		}

		public override void HandlePostExec(
			ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut, bool bufferWasChanged)
		{
			VsCommands2K cmd = (VsCommands2K)nCmdId;
			// Special handling of "Toggle all outlining" command
			if (guidCmdGroup == typeof(VsCommands2K).GUID)
			{
				if ((VsCommands2K)nCmdId == VsCommands2K.OUTLN_TOGGLE_ALL)
				{
					Source.CollapseAllRegions();
					return;
				}
			}

			base.HandlePostExec(ref guidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut, bufferWasChanged);

			// workaround: for some reason, UP and DOWN commands are not passed to Source in base.HandlePostExec
			if (cmd == VsCommands2K.UP || cmd == VsCommands2K.DOWN)
				Source.OnCommand(TextView, cmd, '\0');

			if (_startLine >= 0 && Source.MethodData.IsDisplayed)
			{
				int line;
				int pos;

				TextView.GetCaretPos(out line, out pos);

				if (line != _startLine || pos != _startPos)
				{
					bool backward =
						cmd == VsCommands2K.BACKSPACE ||
						cmd == VsCommands2K.BACKTAB ||
						cmd == VsCommands2K.LEFT ||
						cmd == VsCommands2K.LEFT_EXT;

					TokenInfo info = Source.GetTokenInfo(line, pos);
					TokenTriggers triggerClass = info.Trigger;

					if (!backward && (triggerClass & TokenTriggers.MethodTip) == TokenTriggers.ParameterNext)
					{
						Source.MethodData.AdjustCurrentParameter(1);
					}
					else
					{
						Source.MethodTip(TextView, line, pos, info);
					}
				}
			}
		}

		private void ApplyFormatterResults(List<FormatterResult> results)
		{
			foreach (FormatterResult result in results)
			{
				if (result.StartLine == result.EndLine)
				{
					int line = result.StartLine - 1;
					Debug.Assert(Source.GetLineLength(line) >= result.EndCol - 1, "Line must be GE that formatter result");
					Source.SetText(line, result.StartCol - 1, line, result.EndCol - 1,
									 result.ReplacementString);
				}
				else
					Source.SetText(result.StartLine, result.StartCol - 1, result.EndLine - 1, result.EndCol - 1,
								 result.ReplacementString);
			}
		}

		/// <summary>
		/// Here we will do our formatting...
		/// </summary>
		//public override void ReformatSelection()
		//{
		//	if (this.CanReformat())
		//	{

		//		Debug.Assert(this.Source != null);
		//		if (this.Source != null)
		//		{
		//			TextSpan ts = GetSelection();
		//			if (TextSpanHelper.IsEmpty(ts))
		//			{
		//				// format just this current line.
		//				ts.iStartIndex = 0;
		//				ts.iEndLine = ts.iStartLine;
		//				ts.iEndIndex = this.Source.GetLineLength(ts.iStartLine);
		//			}
		//			Formatter.FormatSpan(ts.iStartLine, ts.iEndLine);
		//			//using (EditArray mgr = new EditArray(this.Source, this.TextView, true, "Formatting"))
		//			//{
		//			//	this.Source.ReformatSpan(mgr, ts);
		//			//	mgr.ApplyEdits();
		//			//}
		//		}
		//	}

		//	//base.ReformatSelection();
		//}
	}
}
