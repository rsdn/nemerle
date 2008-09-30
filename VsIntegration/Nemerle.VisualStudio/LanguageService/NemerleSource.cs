using System;
using System.Collections;
using System.Collections.Generic;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.TextManager.Interop;
using Nemerle.Builtins;
using Nemerle.Compiler;
using System.Linq;
using Nemerle.Completion2.CodeFormatting;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;

using Nemerle.Completion2;

using Nemerle.VisualStudio.Project;

namespace Nemerle.VisualStudio.LanguageService
{
	public delegate AuthoringScope ScopeCreatorCallback(ParseRequest request);

	public class NemerleSource : Source
	{
		#region Init

		public NemerleSource(NemerleLanguageService service, IVsTextLines textLines, Colorizer colorizer)
			: base(service, textLines, colorizer)
		{
			string path = GetFilePath();

			_service	 = service;
			_projectInfo = ProjectInfo.FindProject(path);

			if (_projectInfo != null)
			{
				_projectInfo.AddSource(this);

				_fileIndex = _projectInfo.Project.CompileUnits.GetFileIndex(path);
			}

			LastParseTime = 0;

			_scanner = colorizer.Scanner as NemerleScanner;

			if (_scanner != null)
				_scanner._source = this;
		}

		public override void Dispose()
		{
			if (_projectInfo != null)
				_projectInfo.RemoveSource(this);

			base.Dispose();
		}

		#endregion

		#region Properties

		private NemerleLanguageService _service;
		public  NemerleLanguageService  Service
		{
			get { return _service; }
		}

		private NemerleScanner _scanner;
		public  NemerleScanner  Scanner
		{
			get { return _scanner; }
		}

		private ScopeCreatorCallback _scopeCreator;
		public  ScopeCreatorCallback  ScopeCreator
		{
			get { return _scopeCreator;  }
			set { _scopeCreator = value; }
		}

		private int _timeStamp;
		public  int  TimeStamp
		{
			get { return _timeStamp;  }
		}

		private ProjectInfo _projectInfo;
		public  ProjectInfo  ProjectInfo
		{
			get { return _projectInfo;  }
		}

		private int _fileIndex = -1;
		public  int  FileIndex
		{
			get { return _fileIndex;  }
			set { _fileIndex = value; }
		}

		#endregion

		#region OnChangeLineText

		public override void OnChangeLineText(TextLineChange[] lineChange, int last)
		{
			_timeStamp++;

			//TODO: Сюда нужно вставить код обновляющий локейшоны в дереве типов если редактирование
			// происходит внутри выражений и обнулять дерево типов в обратоном случае.
			// Если дерево типов нет, то ничего не делаем.

			string	  fileName	= GetFilePath();
			ProjectInfo projectInfo = ProjectInfo.FindProject(fileName);

			if (projectInfo == null)
				return;

			for (int i = 0; i < lineChange.Length; i++)
			{
				TextLineChange changes = lineChange[i];

				projectInfo.AddRelocation(
					fileName,
					changes.iNewEndIndex + 1,
					changes.iNewEndLine  + 1,
					changes.iOldEndIndex + 1,
					changes.iOldEndLine  + 1,
					changes.iStartIndex  + 1,
					changes.iStartLine   + 1);
			}

			if (_scanner != null && _scanner.GetLexer().ClearHoverHighlights())
			{
				int lineCount;
				GetTextLines().GetLineCount(out lineCount);
				Recolorize(1, lineCount);
			}

			base.OnChangeLineText(lineChange, last);
		}

		#endregion

		#region Overrides

		public override CommentInfo GetCommentFormat()
		{
			CommentInfo commentInfo = new CommentInfo();

			commentInfo.UseLineComments = true;
			commentInfo.LineStart	   = "//";
			commentInfo.BlockStart	  = "/*";
			commentInfo.BlockEnd		= "*/";

			return commentInfo;
		}

		public override TextSpan CommentLines(TextSpan span, string lineComment)
		{
			// Calculate minimal position of non-space char
			// at lines in selected span.
			var minNonEmptyPosition = 0;
			for (var i = span.iStartLine; i <= span.iEndLine; ++i)
			{
				var line = GetLine(i);
				if (line.Trim().Length <= 0) continue;
				var spaceLen = line.Replace(line.TrimStart(), "").Length;
				if (minNonEmptyPosition == 0 || spaceLen < minNonEmptyPosition)
					minNonEmptyPosition = spaceLen;
			}

			// insert line comment at calculated position.
			var editMgr = new EditArray(this, null, true, "CommentLines");
			for (var i = span.iStartLine; i <= span.iEndLine; ++i)
			{
				var commentSpan = new TextSpan();
				commentSpan.iStartLine = commentSpan.iEndLine = i;
				commentSpan.iStartIndex = commentSpan.iEndIndex = minNonEmptyPosition;
				editMgr.Add(new EditSpan(commentSpan, lineComment));
			}
			editMgr.ApplyEdits();

			// adjust original span to fit comment symbols
			span.iEndIndex += lineComment.Length;
			return span;
		}

		public override AuthoringSink CreateAuthoringSink(ParseReason reason, int line, int col)
		{
			int maxErrors = LanguageService.Preferences.MaxErrorMessages;
			return new NemerleAuthoringSink(this, reason, line, col, maxErrors);
		}

#if DEBUG
		public override void ProcessHiddenRegions(ArrayList hiddenRegions)
		{
			LanguageService.Preferences.MaxRegionTime = 1000 * 60 * 10;

			base.ProcessHiddenRegions(hiddenRegions);
		}
#endif

		public override void ReformatSpan(EditArray mgr, TextSpan span)
		{
			string filePath = GetFilePath();
			ProjectInfo projectInfo = ProjectInfo.FindProject(filePath);
			Engine engine = projectInfo.Engine;

			ReformatSpan_internal(mgr, span, engine, filePath);
			//ReformatSpan_internal(mgr, span, engine, filePath);
			//ReformatSpan_internal(mgr, span, engine, filePath);
			//base.ReformatSpan(mgr, span);
		}
		private static void ReformatSpan_internal(EditArray _mgr, TextSpan span, Engine engine, string filePath)
		{
			List<FormatterResult> results =
				Formatter.FormatSpan(span.iStartLine + 1,
						   span.iStartIndex + 1,
						   span.iEndLine + 1,
						   span.iEndIndex + 1,
						   engine,
						   filePath);
			EditArray mgr = new EditArray(_mgr.Source, _mgr.TextView, true, "formatting");
			foreach (FormatterResult res in results)
			{
				TextSpan loc = new TextSpan();
				loc.iStartIndex = res.StartCol - 1;
				loc.iStartLine = res.StartLine - 1;
				loc.iEndIndex = res.EndCol - 1;
				loc.iEndLine = res.EndLine - 1;

				mgr.Add(new EditSpan(loc, res.ReplacementString));
			}
			mgr.ApplyEdits();
		}

		private MethodData _methodData;
		public  MethodData  MethodData
		{
			get { return _methodData; }
		}

		public override MethodData CreateMethodData()
		{
			return _methodData = base.CreateMethodData();
		}

		#region Paired chars insertion and deletion

		// TODO: maybe refactor this part so that it will share code with BracketFinder
		private readonly char[][] _pairedChars = new char[][]
			{
				new char[] {'{', '}'},
				new char[] {'(', ')'},
				new char[] {'\'', '\''},
				new char[] {'[', ']'},
				new char[] {'"', '"'}
			};

		private bool IsOpeningPairedChar(char ch)
		{
			foreach (char[] charPair in _pairedChars)
			{
				if(charPair[0] == ch)
					return true;
			}
			return false;
		}

		private bool IsClosingPairedChar(char ch)
		{
			foreach (char[] charPair in _pairedChars)
			{
				if(charPair[1] == ch)
					return true;
			}
			return false;
		}
		
		private char GetClosingChar(char ch)
		{
			foreach (char[] charPair in _pairedChars)
			{
				if(charPair[0] == ch)
					return charPair[1];
			}
			throw new ApplicationException("Paired char not found for '" + ch + "'");
		}

		private char _rememberedChar;

		public void RememberCharBeforeCaret(IVsTextView textView)
		{
			int line;
			int idx;
			textView.GetCaretPos(out line, out idx);
			if(idx > 0)
				_rememberedChar = GetText(line, idx - 1, line, idx)[0];
		}

		public void ClearRememberedChar()
		{
			_rememberedChar = (char) 0;
		}


		#endregion

		private void RemoveCharAt(int line, int idx)
		{
			SetText(line, idx, line, idx + 1, "");
		}

		public override void OnCommand(IVsTextView textView, VsCommands2K command, char ch)
		{
			if (textView == null || _service == null || !_service.Preferences.EnableCodeSense)
				return;

			bool backward = (command == VsCommands2K.BACKSPACE || command == VsCommands2K.BACKTAB ||
				command == VsCommands2K.LEFT || command == VsCommands2K.LEFT_EXT);

			int line, idx;
			textView.GetCaretPos(out line, out idx);

			//ScanLexer sl = ((NemerleScanner) (GetColorizer().Scanner)).GetLexer();
			
			//Tuple<GlobalEnv, TypeBuilder, int, int> ret =
			//	_projectInfo.Project.GetActiveEnv(FileIndex, line + 1);
			//sl.SetLine(line + 1, source, 0, ret.Field0, ret.Field1);
			TokenInfo tokenBeforeCaret = GetTokenInfo(line, idx);
			TokenInfo tokenAfterCaret = GetTokenInfo(line, idx + 1);

			//HandlePairedSymbols(textView, command, line, idx, ch);

			if ((tokenBeforeCaret.Trigger & TokenTriggers.MemberSelect) != 0 && (command == VsCommands2K.TYPECHAR))
			{
				ParseReason reason = ((tokenBeforeCaret.Trigger & TokenTriggers.MatchBraces) == TokenTriggers.MatchBraces) ?
					ParseReason.MemberSelectAndHighlightBraces :
					ParseReason.MemberSelect;

				Completion(textView, tokenBeforeCaret, reason);
			}
			TryHighlightBraces(textView, command, line, idx, tokenBeforeCaret, tokenAfterCaret);

			if (!_methodData.IsDisplayed &&
				(tokenBeforeCaret.Trigger & TokenTriggers.MethodTip) != 0 &&
				command == VsCommands2K.TYPECHAR &&
				_service.Preferences.ParameterInformation)
			{
				MethodTip(textView, line, idx, tokenBeforeCaret);
			}
		}

		private void TryHighlightBraces(IVsTextView textView, VsCommands2K command, int line, int idx,
										TokenInfo tokenInfo)
		{
			// Highlight brace to the left from the caret
			if ((tokenInfo.Trigger & TokenTriggers.MatchBraces) != 0 && _service.Preferences.EnableMatchBraces)
			{
				if ( (command != VsCommands2K.BACKSPACE) && 
					(/*(command == VsCommands2K.TYPECHAR) ||*/ 
					_service.Preferences.EnableMatchBracesAtCaret)) 
				{
					MatchBraces(textView, line, idx, tokenInfo);
				}
			}
		}
		private void TryHighlightBraces(IVsTextView textView, VsCommands2K command, int line, int idx,
										TokenInfo tokenBeforeCaret, TokenInfo tokenAfterCaret)
		{
			TryHighlightBraces(textView, command, line, idx + 1, tokenAfterCaret);
			TryHighlightBraces(textView, command, line, idx, tokenBeforeCaret);
		}

		public void TryHighlightBraces(IVsTextView textView)
		{
			var colorizer = GetColorizer() as NemerleColorizer;
			if (colorizer != null && colorizer.IsClosed)
				return;
			
			int line, idx;
			textView.GetCaretPos(out line, out idx);

			TokenInfo tokenBeforeCaret = GetTokenInfo(line, idx);
			TokenInfo tokenAfterCaret = GetTokenInfo(line, idx + 1);

			TryHighlightBraces(textView, VsCommands2K.UP, line, idx, tokenBeforeCaret, tokenAfterCaret);
		}

		private void HandlePairedSymbols(IVsTextView textView, VsCommands2K command, int line, int idx, char ch)
		{
			// insert paired symbols here
			if(command == VsCommands2K.TYPECHAR)
			{
				if(IsOpeningPairedChar(ch))
				{
					SetText(line, idx, line, idx, GetClosingChar(ch).ToString());
					textView.SetCaretPos(line, idx);
				}
				// if we just typed closing char and the char after caret is the same then we just remove 
				// one of them
				if(IsClosingPairedChar(ch))
				{
					char charAfterCaret = GetText(line, idx, line, idx + 1)[0];
					if (ch == charAfterCaret/*IsClosingPairedChar(charAfterCaret)*/)
						RemoveCharAt(line, idx);
				}
			}
			// delete closing char if opened char was just backspaced and closing one is right next
			if(command == VsCommands2K.BACKSPACE)
			{
				char closingChar = GetText(line, idx, line, idx + 1)[0];
				if(IsOpeningPairedChar(_rememberedChar) && IsClosingPairedChar(closingChar))
					RemoveCharAt(line, idx);
			}
		}

		#endregion

		internal static int HiddenRegionCookie = 25;

		internal void CollapseAllRegions()
		{
			IVsHiddenTextSession session = GetHiddenTextSession();
			IVsEnumHiddenRegions ppenum;
			TextSpan[] aspan = new TextSpan[1];
			aspan[0] = GetDocumentSpan();
			ErrorHandler.ThrowOnFailure(session.EnumHiddenRegions((uint)FIND_HIDDEN_REGION_FLAGS.FHR_BY_CLIENT_DATA, (uint)NemerleSource.HiddenRegionCookie, aspan, out ppenum));
			IVsHiddenRegion[] aregion = new IVsHiddenRegion[1];
			using (new CompoundAction(this, "ToggleAllRegions"))
			{
				uint fetched;
				while (ppenum.Next(1, aregion, out fetched) == NativeMethods.S_OK && fetched == 1)
				{
					uint dwState;
					aregion[0].GetState(out dwState);
					//dwState &= ~(uint)HIDDEN_REGION_STATE.hrsExpanded;
					dwState = (uint)HIDDEN_REGION_STATE.hrsDefault;
					ErrorHandler.ThrowOnFailure(aregion[0].SetState(dwState,
						(uint)CHANGE_HIDDEN_REGION_FLAGS.chrDefault));
				}
			}
		}
		internal void RenameSymbols(string newName, GotoInfo[] usages)
		{
			RenameSymbols(newName, usages, true);
		}

		internal void RenameSymbols(string newName, GotoInfo[] usages, bool wrapInTransaction)
		{
			if(wrapInTransaction)
			{
				using (var undoTransaction = new LinkedUndoTransaction("Rename refactoring", Service.Site))
				{	
					RenameSymbolsInternal(newName, usages);
					undoTransaction.Commit();
				}
			}
			else 
				RenameSymbolsInternal(newName, usages);
		}

		private void RenameSymbolsInternal(string newName, GotoInfo[] usages)
		{
			var distinctFiles = (from us in usages
								 select us.Location.File).Distinct();

			foreach (string filePath in distinctFiles)
			{
				var tmpFilePath = filePath;
				var mgr = new EditArray(ProjectInfo.GetSource(tmpFilePath), null, true, "Renaming");
				var thisFileUsages = from use in usages
									 where use.Location.File == tmpFilePath
									 select use;

				foreach (var usage in thisFileUsages)
				{
					var span = Utils.SpanFromLocation(usage.Location);
					mgr.Add(new EditSpan(span, newName));
				}
				mgr.ApplyEdits();
			}
		}

		public void DeleteEmptyStatementAt(int lineIndex)
		{
			var txt = GetLine(lineIndex);
			if(txt.Trim() == ";")
			{
				//var len = GetLineLength(lineIndex);
				SetText(lineIndex, 0, lineIndex + 1, 0, "");
			}
		}
	}
}
