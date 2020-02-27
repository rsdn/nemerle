using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Completion2;
using Nemerle.Compiler;
using Nemerle.Completion2.CodeFormatting;
using Nemerle.VisualStudio.Project;
using Nemerle.VisualStudio.GUI;
using TupleIntInt = Nemerle.Builtins.Tuple<int, int>;

using VsCommands = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Project;
using Nemerle.Compiler.Utils.Async;
using Microsoft.VisualStudio.Shell;
using System.ComponentModel.Design;
using System.Runtime.InteropServices;
using Nemerle.VisualStudio;
using Microsoft.VisualStudio.Language.Intellisense;
using Nemerle.VsExtension;
using Microsoft.VisualStudio.Text;

namespace Nemerle.VisualStudio.LanguageService
{
	partial class NemerleViewFilter : ViewFilter
	{
		public NemerleViewFilter(CodeWindowManager mgr, IVsTextView view)
			: base(mgr, view)
		{
		}

		public override void HandleGoto(VsCommands cmd)
		{
			bool gotoDefinition = false;
			switch (cmd)
			{
				case VSConstants.VSStd97CmdID.GotoDecl:
				case VSConstants.VSStd97CmdID.GotoDefn: gotoDefinition = true; break;

				case VSConstants.VSStd97CmdID.GotoRef:
					Trace.WriteLine("GoToReference() not implemenred yet");
					break;
				default: Trace.Assert(false); break;
			}

			Source.Goto(TextView, gotoDefinition, this.TextView.ToITextView().Caret.Position.BufferPosition.ToLocation());
		}

		#region GetDataTipText

		public override TextTipData CreateTextTipData()
		{
			return base.CreateTextTipData();
		}

		public override int GetDataTipText(TextSpan[] aspan, out string textValue)
		{
			//VladD2: Стратегия отображения хинта:
			// У нас есть два основных режима отображения хинтов 1. Во время разработки. 2. Во время отладки.
			// В любом случае, данные описывающие текущий элемент формируются в теневом потоке.
			// В них формируется строка которая будет оторбажаться и описание выражений на которых указывает курсор (aspan[0]).
			// Когда хинт сформирован проверяем не под отладчиком ли мы и если под отладчиктом, то пытаемся понять,
			// что конкретно нужно отображать - hint с информацией о выражении, или DataHint который отображает значение выражения
			// при отладке.

			textValue = null;
			if (Source == null || Source.LanguageService == null || !Source.LanguageService.Preferences.EnableQuickInfo)
				return NativeMethods.E_FAIL;

            return Source.GetDataTipText(TextView, aspan, out textValue);
		}

		/// <summary>This method checks to see if the IVsDebugger is running, and if so,
		/// calls it to get additional information about the current token and returns a combined result.
		/// You can return an HRESULT here like TipSuccesses2.TIP_S_NODEFAULTTIP.</summary>
		public override int GetFullDataTipText(string textValue, TextSpan ts, out string fullTipText)
		{
			IVsTextLines textLines;
			fullTipText = textValue;

			ErrorHandler.ThrowOnFailure(this.TextView.GetBuffer(out textLines));

			// Now, check if the debugger is running and has anything to offer
			try
			{
				Microsoft.VisualStudio.Shell.Interop.IVsDebugger debugger = Source.LanguageService.GetIVsDebugger();

				if (debugger != null && Source.LanguageService.IsDebugging)
				{
					var tsdeb = new TextSpan[1] { new TextSpan() };
					if (!TextSpanHelper.IsEmpty(ts))
					{
						// While debugging we always want to evaluate the expression user is hovering over
						ErrorHandler.ThrowOnFailure(TextView.GetWordExtent(ts.iStartLine, ts.iStartIndex, (uint)WORDEXTFLAGS.WORDEXT_FINDEXPRESSION, tsdeb));
						// If it failed to find something, then it means their is no expression so return S_FALSE
						if (TextSpanHelper.IsEmpty(tsdeb[0]))
						{
							return NativeMethods.S_FALSE;
						}
					}
					string debugTextTip = null;
					int hr = debugger.GetDataTipValue(textLines, tsdeb, null, out debugTextTip);
					fullTipText = debugTextTip;
					if (hr == (int)TipSuccesses2.TIP_S_NODEFAULTTIP)
					{
						return hr;
					}
					if (!string.IsNullOrEmpty(debugTextTip) && debugTextTip != textValue)
					{
						// The debugger in this case returns "=value [type]" which we can
						// append to the variable name so we get "x=value[type]" as the full tip.
						int i = debugTextTip.IndexOf('=');
						if (i >= 0)
						{
							string spacer = (i < debugTextTip.Length - 1 && debugTextTip[i + 1] == ' ') ? " " : "";
							fullTipText = textValue + spacer + debugTextTip.Substring(i);
						}
					}
				}
#if LANGTRACE
						} catch (COMException e) {
								Trace.WriteLine("COMException: GetDataTipValue, errorcode=" + e.ErrorCode);
#else
			}
			catch (System.Runtime.InteropServices.COMException)
			{
#endif
			}

			if (string.IsNullOrEmpty(fullTipText))
				fullTipText = textValue;

			return NativeMethods.S_OK;
		}

		#endregion
		public override void OnKillFocus(IVsTextView view)
		{
			var pkg = Source.Service.Package;
			pkg.RefactoringMenu.Visible = false;
			pkg.SelectionExtend.Visible = false;
			pkg.SelectionShrink.Visible = false;
			pkg.SelectionExtend.Enabled = false;
			pkg.SelectionShrink.Enabled = false;

			//Debug.WriteLine("OnKillFocus(IVsTextView view)");
			base.OnKillFocus(view);
		}

		public override void OnSetFocus(IVsTextView view)
		{
			var pkg = Source.Service.Package;
			pkg.RefactoringMenu.Visible = true;
			pkg.SelectionExtend.Visible = true;
			pkg.SelectionShrink.Visible = true;
			pkg.SelectionExtend.Enabled = true;
			pkg.SelectionShrink.Enabled = true;

			//Debug.WriteLine("OnSetFocus(IVsTextView view)");
			//ShowAst(view, true);
			base.OnSetFocus(view);
		}

		Dictionary<IntPtr, TupleIntInt> _viewsCarretInfo = new Dictionary<IntPtr, TupleIntInt>();

		public override void OnChangeScrollInfo(IVsTextView view, int iBar,
			int iMinUnit, int iMaxUnits, int iVisibleUnits, int iFirstVisibleUnit)
		{
			base.OnChangeScrollInfo(view, iBar, iMinUnit, iMaxUnits, iVisibleUnits, iFirstVisibleUnit);

			Source.Service.Hint.Close();

			var viewUnknown = Utilities.QueryInterfaceIUnknown(view);
			if (viewUnknown != IntPtr.Zero)
				Marshal.Release(viewUnknown);

			TupleIntInt pos;
			var posExists = _viewsCarretInfo.TryGetValue(viewUnknown, out pos);
			int line, idx;
			if (view.GetCaretPos(out line, out idx) == VSConstants.S_OK)
			{
				if (!posExists || pos.Field0 != line || pos.Field1 != idx)
				{
					// pos was changed
					_viewsCarretInfo[viewUnknown] = new TupleIntInt(line, idx);
					//Debug.WriteLine("pos was changed line=" + line + " col=" + idx);
				}
			}
		}

		public override void ShowContextMenu(int menuId, Guid groupGuid, IOleCommandTarget target, int x, int y)
		{
			var service = Source.Service;

			service.IsHintsEnabled = false;

			try
			{
				service.Hint.Close();

				var menuService = (OleMenuCommandService)service.GetService(typeof(IMenuCommandService));
				if (menuService == null || service.IsMacroRecordingOn())
					return;

				var id = new CommandID(groupGuid, menuId);
				menuService.ShowContextMenu(id, x, y);

			}
			finally
			{
				service.IsHintsEnabled = true;
			}
			return;
		}

		public override void OnChangeCaretLine(IVsTextView view, int line, int col)
		{
			base.OnChangeCaretLine(view, line, col);
		}

		/// <summary>
		/// Реализация Find All References поиска всех вхождений (в перспективе включая поиск и по не-Nemerle проектам)
		/// с заполнением окошка "Find Symbol Results" студии
		/// </summary>
		/// <remarks>
		/// Вызываем Source.Goto, и подготавливаем результаты поиска
		/// Потом передаём уже готовые результаты поиска в _library через метод NemerleLibraryManager
		/// Затем вызываем IVsObjectSearch.Find - интерфейс отвечающий за поиски, который найдёт и вызовет _library.GetList2(),
		/// IVsObjectSearch в свою очередь должен поискать и в остальных проектах (пока не реализовано, т.к. нет чёткого понятия какими должны быть VSOBSEARCHCRITERIA),
		/// и вывести все результаты поиска в окошке Find Symbol Results (уже выводит)
		///
		/// Обсуждения в форумах по теме:
		/// http://social.msdn.microsoft.com/Forums/en-US/vsx/thread/951158dd-fc98-4325-b07d-bab65b372603/
		/// http://social.msdn.microsoft.com/Forums/en-US/vsx/thread/793f916d-80a6-4944-b058-7166d48d3a32
		/// http://social.msdn.microsoft.com/Forums/en-US/vsx/thread/3d85e968-f735-420c-b9c8-d57ed7839d36
		///
		/// Возможно для поиска по всему проекту IVsObjectSearch.Find придётся заменить на IVsFindSymbol.DoSearch
		/// http://msdn.microsoft.com/en-us/library/microsoft.visualstudio.shell.interop.ivsfindsymbol.dosearch.aspx
		/// есть какая-то инфа про глюки в методе Find
		/// http://social.msdn.microsoft.com/Forums/en-US/vsx/thread/08b71611-2c94-40e7-a79e-3be843c974ea/
		/// </remarks>
		private void FindReferences()
		{
			var findSvc = (IVsObjectSearch)Source.Service.GetService(typeof(SVsObjectSearch));

			if(findSvc != null)
			{
				string caption;
                var at = this.TextView.ToITextView().Caret.Position.BufferPosition.ToLocation();
                var infos = Source.GetGotoInfo(TextView, false, at, out caption);
				if((infos != null) && (infos.Length > 0))
				{
					var criteria = new[]
					{
						new VSOBSEARCHCRITERIA
						{
							eSrchType = VSOBSEARCHTYPE.SO_ENTIREWORD,
							grfOptions = (uint)_VSOBSEARCHOPTIONS.VSOBSO_CASESENSITIVE,
							szName = "<dummy>",
							dwCustom = Library.FindAllReferencesMagicNum,
						}
					};

					var inlm = Source.Service.GetService(typeof(INemerleLibraryManager));
					if(inlm != null)
					{
						var nlm = (NemerleLibraryManager)inlm;

						var libSearchResults = new LibraryNode("<dummy2>", LibraryNode.LibraryNodeType.References, LibraryNode.LibraryNodeCapabilities.None, null);

						foreach(var i in infos)
						{
							var inner = new GotoInfoLibraryNode((NemerleLanguageService)Source.LanguageService, i, caption);
							libSearchResults.AddNode(inner);
						}

						nlm.OnFindAllReferencesDone(libSearchResults);
						IVsObjectList results;
						var hr = findSvc.Find((uint)__VSOBSEARCHFLAGS.VSOSF_EXPANDREFS, criteria, out results);
					}
				}
			}
		}

		protected override int ExecCommand(ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
		{
			if (guidCmdGroup != VSConstants.GUID_VSStandardCommandSet97 && guidCmdGroup != Microsoft.VisualStudio.Project.VsMenus.guidStandardCommandSet2K)
			{
				if (guidCmdGroup == MenuCmd.guidNemerleProjectCmdSet)
				{
				}
				else
				{
				}
			}

			if(guidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
			{
				var cmdId = (VSConstants.VSStd97CmdID)nCmdId;

				switch(cmdId)
				{
					case VSConstants.VSStd97CmdID.FindReferences:
						FindReferences();
						return VSConstants.S_OK;
					case VSConstants.VSStd97CmdID.SolutionCfg:
						break;
					case VSConstants.VSStd97CmdID.SearchCombo:
						break;
					default:
						break;
				}
			}

			//Debug.WriteLine(guidCmdGroup + " " + nCmdId);
			const VSConstants.VSStd2KCmdID ShowSmartTag = (VSConstants.VSStd2KCmdID)147;

			if (guidCmdGroup == VSConstants.VSStd2K)
			{
				switch ((VSConstants.VSStd2KCmdID)nCmdId)
				{
					case VSConstants.VSStd2KCmdID.CANCEL:
						Source.SetUsageHighlighting(Enumerable.Empty<GotoInfo>());
						Source.Service.Hint.Close();
						break; // go trocess ESC

					//case ShowSmartTag:
					//	var textView = TextView.ToITextView();
					//	var smartTagBroker = textView.GetSmartTagBroker();
                    //
					//	foreach (var session in smartTagBroker.GetSessions(textView))
					//	{
					//		var span = session.ApplicableToSpan.GetSpan(textView.TextSnapshot);
                    //
					//		if (span.Contains(textView.Caret.Position.BufferPosition.Position))
					//		{
					//			session.State = SmartTagState.Expanded;
					//			return VSConstants.S_OK;
					//		}
					//	}
                    //
					//	return VSConstants.S_OK;
				}
			}

			// hi_octane : found a lot of mistakes comparing the switch
			// ids with PkgCmdID.h and NemerleMenus.cs
			// decided modify the code
			// leaving only two files required to be synchronized

			if (guidCmdGroup == MenuCmd.guidNemerleProjectCmdSet)
			{
				var nemerleCmd = (MenuCmd.CmdId)nCmdId;

				string txt = null;

				switch (nemerleCmd)
				{
					case MenuCmd.CmdId.ImportSymbolCompletion:
						break;
					case MenuCmd.CmdId.IplementInterface:
						break;
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
					case MenuCmd.CmdId.Inline: // cmdIdInline
						RunInlineRefactoring();
						return VSConstants.S_OK;
					case MenuCmd.CmdId.Options:
						// cmdIdOptions
						ShowOptions();
						return VSConstants.S_OK;
					case MenuCmd.CmdId.AddHighlighting: // cmdIdAddHighlighting
						HighlightSymbol();
						return VSConstants.S_OK;
					case MenuCmd.CmdId.RemoveLastHighlighting: // cmdIdRemoveLastHighlighting
						Source.SetUsageHighlighting(Enumerable.Empty<GotoInfo>());
						Source.Service.Hint.Close();
						return VSConstants.S_OK;
				}

				Trace.Assert(txt == null, "Implement the menu!\r\nID: " + txt);
			}

			_executingCommand = (VsCommands2K)nCmdId;
			try
			{
				if (guidCmdGroup == Microsoft.VisualStudio.Project.VsMenus.guidStandardCommandSet2K)
				{
					if (_executingCommand == VsCommands2K.TAB)
					{
                        var at = TextView.ToITextView().Caret.Position.BufferPosition.ToLocation();
                        if (!Source.CompletionSet.IsDisplayed && this.Source.TryDoTableFormating(this, at))
							return VSConstants.S_OK;
					}

					switch (_executingCommand)
					{
						case VsCommands2K.SolutionPlatform:
							break;
                        case VsCommands2K.GOTOBRACE:
                            GoToBrace();
                            break;
                        default:
							break;
					}
				}

				var result = base.ExecCommand(ref guidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut);
				return result;
			}
			finally { _executingCommand = 0; }
		}

        private void GoToBrace()
        {
            var textView = TextView.ToITextView();
            var textBuffer = textView.TextBuffer;
            var taggerKey = typeof(MatchingBracesTagger);
            MatchingBracesTagger tagger;
            if (textBuffer.Properties.TryGetProperty(taggerKey, out tagger))
            {
                var pos = textView.Caret.Position.BufferPosition;
                var posSpan = new Microsoft.VisualStudio.Text.Span(pos.Position, 0);
                var matchingBraces = tagger.MatchingBraces.ToArray();
                foreach (var (open, close) in matchingBraces)
                {
                    if (open.Start == pos.Position)
                    {
                        NavigateTo(close.End);
                        return;
                    }
                }
                foreach (var (open, close) in matchingBraces)
                {
                    if (close.End == pos.Position)
                    {
                        NavigateTo(open.Start);
                        return;
                    }
                }
                foreach (var (open, close) in matchingBraces)
                {
                    if (close.Start == pos.Position)
                    {
                        NavigateTo(open.Start);
                        return;
                    }
                    if (open.End == pos.Position)
                    {
                        NavigateTo(close.End);
                        return;
                    }
                }
                return;
            }
        }

        void NavigateTo(SnapshotPoint point)
        {
            var textView = TextView.ToITextView();
            textView.Caret.MoveTo(point);
            textView.ViewScroller.EnsureSpanVisible(new SnapshotSpan(point.Snapshot, point.Position, 0));
        }


        VsCommands2K _executingCommand;

		private List<Location> _selectionsStack;

		private int _currentSelection;
		public int CurrentSelection
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
			//TODO: В этом нужно разбираться и или выкидывать, или реализовывать по человечески

			//TextSpan selection = GetSelection();
			//if (!SelectionIsInStack(selection))
			//{
			//  _selectionsStack = GetSelectionsStack(selection);
			//  AddWordSpan(selection);
			//  _currentSelection = _selectionsStack.Count;
			//}
			//CurrentSelection--;
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
					return;
                var spans = TextView.ToITextView().Selection.SelectedSpans;
                if (spans.Count == 1)
				    source.GetEngine().BeginHighlightUsages(source, spans.First().ToLocation());
			}
		}

		private bool WarnAboutErrors()
		{
			var projectInfo = Source.ProjectInfo;

			if (projectInfo == null)
				return false;

			if (projectInfo.ErrorCount > 0)
				return MessageBox.Show(TextEditorWindow,
					"The project contaions error[s]. Are you sure you want to proceed (it's unsafe!)?",
					"",
					MessageBoxButtons.YesNo,
					MessageBoxIcon.Exclamation
					) == DialogResult.Yes;

			return true;
		}

		private void RunInlineRefactoring()
		{
			//TODO: Создать задачу для данного вида рефакторинга и перенести эту реализацию в нее

			//if (Source == null)
			//  return;

			//var proj = Source.ProjectInfo.Project;
			//var engine = Source.ProjectInfo.Engine;

			//if(!WarnAboutErrors(proj))
			//  return;

			//int lineIndex;
			//int colIndex;
			//TextView.GetCaretPos(out lineIndex, out colIndex);
			//GotoInfo[] definitions = engine.GetGotoInfo(Source,
			//                             lineIndex + 1,
			//                             colIndex + 1,
			//                             Nemerle.Compiler.Utils.Async.GotoKind.Definition);
			//if (definitions == null || definitions.Length == 0)
			//  return;

			//if (definitions.Length > 1)
			//{
			//  MessageBox.Show(TextEditorWindow, "More than one definition found. Cannot inline.");
			//  return;
			//}

			//if (proj.CanInlineExpressionAt(definitions[0]))
			//{
			//  HighlightSymbol();

			//  using (var undoTransaction = new LinkedUndoTransaction("Inline refactoring", Source.Service.Site))
			//  {
			//    // replacing usages with initializer
			//    var tuple = proj.GetReplacementStringForInline(definitions[0]);
			//    var defLocation = tuple.Field0;
			//    var initLocation = tuple.Field1;
			//    var replacement = Source.GetText(Utils.SpanFromLocation(initLocation));
			//    var shouldEmbrace = tuple.Field2;
			//    var usages = engine.GetGotoInfo(Source,
			//                       lineIndex + 1,
			//                       colIndex,
			//                       GotoKind.Usages)
			//                .Where(usage => usage.UsageType == UsageType.Usage)
			//                .ToArray();
			//    using (var frm = new InlineRefactoringPreview(proj))
			//    {
			//      frm.Usages = usages;
			//      frm.ExpressionToInline = shouldEmbrace
			//                    ? ("(" + replacement + ")")
			//                    : replacement;
			//      if (frm.ShowDialog(TextEditorWindow) != DialogResult.OK)
			//      {
			//        RemoveLastHighlighting();
			//        return;
			//      }
			//    }
			//    Source.RenameSymbols(replacement, usages, false);

			//    // replacing definition with empty string
			//    Source.RenameSymbols("", new[] { new GotoInfo(defLocation) });

			//    Source.DeleteEmptyStatementAt(defLocation.Line - 1);

			//    undoTransaction.Commit();
			//  }
			//  RemoveLastHighlighting();
			//}
			//else
			//{
			//  MessageBox.Show(TextEditorWindow, "Only simple def's can be inlined at the moment.");
			//}
		}

		private void RunRenameRefactoring()
		{
			if (Source == null || Source.ProjectInfo == null)
				return;

			var engine = Source.ProjectInfo.Engine;
			if (!WarnAboutErrors())
				return;

			var allUsages = engine.GetGotoInfo(Source, TextView.ToITextView().Caret.Position.BufferPosition.ToLocation(), GotoKind.Usages);
			var usages = allUsages.Distinct().ToArray();

			if (usages == null || usages.Length == 0)
			{
				Source.ProjectInfo.ShowMessage("No symbol to rename", MessageType.Error);
				return;
			}

			var definitionCount = usages.Count(usage => usage.UsageType == UsageType.Definition);

			if (definitionCount == 0)
			{
				Source.ProjectInfo.ShowMessage("Cannot find definition.", MessageType.Error);
				return;
			}

			using (var frm = new RenameRefactoringDlg(engine, usages))
			{
				if (frm.ShowDialog(TextEditorWindow) == DialogResult.OK)
					Source.RenameSymbols(frm.NewName, usages);
			}
		}

		private void FindInheritors()
		{
			if (Source == null)
				return;

			var ourLanguageService = Source.LanguageService as NemerleLanguageService;

			if (ourLanguageService == null)
				return;

            var at = this.TextView.ToITextView().Caret.Position.BufferPosition.ToLocation();
            var infos = Source.ProjectInfo.Engine.GetInheritorsGotoInfo(Source, at);

			// If we have only one found usage, then jump directly to it.
			if (infos.Length == 1)
				ourLanguageService.GotoLocation(infos[0].Location);
			else if (infos.Length > 0) // otherwise show a form to let user select entry manually
				using (GotoUsageForm popup = new GotoUsageForm(infos))
					if (popup.ShowDialog(TextEditorWindow) == DialogResult.OK)
						ourLanguageService.GotoLocation(popup.Result.Location);
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

		protected override int QueryCommandStatus(ref Guid guidCmdGroup, uint nCmdId)
		{
			if(guidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
			{
				var cmdId = (VSConstants.VSStd97CmdID)nCmdId;

				switch(cmdId)
				{
					case VSConstants.VSStd97CmdID.FindReferences:
						return (int)(OLECMDF.OLECMDF_SUPPORTED | OLECMDF.OLECMDF_ENABLED);

					default:
						break;
				}
			}
			else if (guidCmdGroup == VSConstants.VSStd2K)
			{
				var cmdId = (VSConstants.VSStd2KCmdID)nCmdId;

				switch (cmdId)
				{
					case VSConstants.VSStd2KCmdID.INSERTSNIPPET:
					case VSConstants.VSStd2KCmdID.SURROUNDWITH:
					case VSConstants.VSStd2KCmdID.RENAME:
						return (int)(OLECMDF.OLECMDF_SUPPORTED | OLECMDF.OLECMDF_ENABLED);

					default:
						break;
				}
			}
			else if (guidCmdGroup == MenuCmd.guidNemerleProjectCmdSet)
			{
				var cmdId = (MenuCmd.CmdId)nCmdId;
				switch (cmdId)
				{
					case MenuCmd.CmdId.ESC:
					case MenuCmd.CmdId.SetAsMain:
					case MenuCmd.CmdId.ExtendSelection:
					case MenuCmd.CmdId.ShrinkSelection:
					case MenuCmd.CmdId.FindInheritors:
					case MenuCmd.CmdId.Options:
					case MenuCmd.CmdId.AstToolWindow:
					case MenuCmd.CmdId.AddHighlighting:
					case MenuCmd.CmdId.RemoveLastHighlighting:
					case MenuCmd.CmdId.FindInheritorsCtxt:
					case MenuCmd.CmdId.GoToFile:
					case MenuCmd.CmdId.GoToType:
					case MenuCmd.CmdId.SourceOutlinerWindow:
						break;

					case MenuCmd.CmdId.IplementInterface:
					case MenuCmd.CmdId.ReloadProject:
					case MenuCmd.CmdId.ImportSymbolCompletion:
					case MenuCmd.CmdId.Inline:
					case MenuCmd.CmdId.RefactoringTopMenu:
						return (int)OLECMDF.OLECMDF_SUPPORTED | (int)OLECMDF.OLECMDF_ENABLED;

					default:
						break;
				}
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
			// GetSelection() inverts negative spans (bug #1176)
			TextSpan ts = GetSelection();

			// workaround for bug #1050
			// TextView.GetSelection adds caret position to the selection span,
			// so when we select whole block of text (caret is at the first column),
			// one extra line is added to the selection (where the caret is left)
			if (ts.iEndIndex == 0 && ts.iEndLine - ts.iStartLine > 0)
				ts.iEndLine--;

			return ts;
		}

		public override bool HandlePreExec(
			ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
		{
			var cmd = (VsCommands2K)nCmdId;

			//// we're goona to erase some symbol from existence.
			//// In some cases we need to know what it was (auto-deletion of paired token)
			//if (cmd == VsCommands2K.BACKSPACE)
			//  Source.RememberCharBeforeCaret(TextView);
			//else
			//  Source.ClearRememberedChar();

			_startLine = -1;

			if (guidCmdGroup == MenuCmd.guidNemerleProjectCmdSet)
			{
				var nemerleCmd = (MenuCmd.CmdId)nCmdId;

				switch (nemerleCmd)
				{
					case MenuCmd.CmdId.ReloadProject:
						var projectInfo = ((NemerleSource)Source).ProjectInfo;

						if (projectInfo != null)
							projectInfo.Engine.BeginBuildTypesTree();

						return true;
					case MenuCmd.CmdId.ImportSymbolCompletion:
                        Source.ImportCompletion(TextView, TextView.ToITextView().Caret.Position.BufferPosition.ToLocation());
						return true;
				}
			}

			if (guidCmdGroup == VSConstants.VSStd2K)
			{
				if (Source.MethodData != null && Source.MethodData.IsDisplayed)
					TextView.GetCaretPos(out _startLine, out _startPos);

				switch (cmd)
				{
					case VSConstants.VSStd2KCmdID.RENAME:
						RunRenameRefactoring();
						return true;
					case VsCommands2K.COMPLETEWORD:
                        Source.Completion(TextView, TextView.ToITextView().Caret.Position.BufferPosition.ToLocation(), false);
                        return true;
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
			int line;
			int idx;
			TextView.GetCaretPos(out line, out idx);

			return Source.SmartIndent.At(line);
		}

		public override void HandlePostExec(ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut, bool bufferWasChanged)
		{
			VsCommands2K cmd = (VsCommands2K)nCmdId;

			// Special handling of "Toggle all outlining" command
			//CodingUnit: 2010.02.19 normal action back in Toggle All Outlining
			/*if (guidCmdGroup == typeof(VsCommands2K).GUID)
			{
				if ((VsCommands2K)nCmdId == VsCommands2K.OUTLN_TOGGLE_ALL)
				{
					Source.CollapseAllRegions();
					return;
				}
			}*/

			base.HandlePostExec(ref guidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut, bufferWasChanged);

			if (guidCmdGroup == VSConstants.VSStd2K)
			{
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
							Source.MethodData.AdjustCurrentParameter(1);
						else
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
	}
}
