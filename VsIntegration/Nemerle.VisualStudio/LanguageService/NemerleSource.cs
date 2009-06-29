using System;
using System.Collections;
using System.Diagnostics;
using System.Collections.Generic;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.TextManager.Interop;
//using Nemerle.Builtins;
using Nemerle.Compiler;
using System.Linq;
using Nemerle.Completion2.CodeFormatting;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;

using Nemerle.Completion2;

using Nemerle.VisualStudio.Project;
using System.Runtime.InteropServices;

namespace Nemerle.VisualStudio.LanguageService
{
	public delegate AuthoringScope ScopeCreatorCallback(ParseRequest request);

	public class NemerleSource : Source
	{
		#region Init

		public NemerleSource(NemerleLanguageService service, IVsTextLines textLines, Colorizer colorizer)
			: base(service, textLines, colorizer)
		{
			FileIndex = -1;
			string path = GetFilePath();

			Service = service;
			ProjectInfo = ProjectInfo.FindProject(path);

			if (ProjectInfo != null)
			{
				ProjectInfo.AddSource(this);

				FileIndex = ProjectInfo.Project.CompileUnits.GetFileIndex(path);

				ProjectInfo.MakeCompilerMessagesTextMarkers(textLines, FileIndex);
			}

			Scanner = colorizer.Scanner as NemerleScanner;

			if (Scanner != null)
				Scanner._source = this;

			ReparseDelta  = TimeSpan.FromSeconds(1.5);
			LastDirtyTime = DateTime.Now;
		}

		#endregion

		#region Properties

		public     DateTime               LastDirtyTime { get; private set; }
		public new DateTime               LastParseTime { get; private set; }
		public     TimeSpan               ReparseDelta  { get; set; }
		public     NemerleLanguageService Service       { get; private set; }
		public     NemerleScanner         Scanner       { get; private set; }
		public     ScopeCreatorCallback   ScopeCreator  { get;         set; }
		public     ProjectInfo            ProjectInfo   { get; private set; }
		public     int                    FileIndex     { get;         set; }
		public     MethodData             MethodData    { get; private set; }
		public     int                    TimeStamp     { get; private set; }

		#endregion

		#region Fields

		bool          _isRegionsProcessed;
		MemberBuilder _resetedMember;

		#endregion

		#region OnChangeLineText

		public override void OnChangeLineText(TextLineChange[] lineChange, int last)
		{
      var oldLastDirtyTime = LastDirtyTime;
			base.OnChangeLineText(lineChange, last);
			TimeStamp++;

			//TODO: —юда нужно вставить код обновл€ющий локейшоны в дереве типов если редактирование
			// происходит внутри выражений и обнул€ть дерево типов в обратоном случае.
			// ≈сли дерево типов нет, то ничего не делаем.

			string	  fileName	= GetFilePath();
			ProjectInfo projectInfo = ProjectInfo.FindProject(fileName);

			if (projectInfo == null)
				return;

			TextLineChange changes = lineChange[0];
			var resetedMember = projectInfo.AddRelocation(
				fileName,
				changes.iNewEndIndex + 1,
				changes.iNewEndLine  + 1,
				changes.iOldEndIndex + 1,
				changes.iOldEndLine  + 1,
				changes.iStartIndex  + 1,
				changes.iStartLine   + 1);

			if (resetedMember != null)
			{
				//LastDirtyTime = oldLastDirtyTime;
				_resetedMember = resetedMember;
			}
				//this.IsDirty = false;

			if (Scanner != null && Scanner.GetLexer().ClearHoverHighlights())
			{
				int lineCount;
				GetTextLines().GetLineCount(out lineCount);
				Recolorize(1, lineCount);
			}
		}

		#endregion

		#region Overrides

		public override bool IsDirty
		{
			get { return base.IsDirty; }
			set
			{
				Debug.WriteLine("IsDirty = " + value);
				base.IsDirty = value;
				if (value)
					LastDirtyTime = System.DateTime.Now;
				//else
				//	LastParseTime = System.DateTime.Now;
			}
		}

		public override void OnIdle(bool periodic)
		{
			var completionSetIsDisplayed = CompletionSet != null && CompletionSet.IsDisplayed;
			var methodDataIsDisplayed = MethodData != null && MethodData.IsDisplayed;
			var projectInfo = ProjectInfo.FindProject(GetFilePath());
			var engine = projectInfo == null ? null : projectInfo.Engine;

			if (engine != null && !completionSetIsDisplayed && !methodDataIsDisplayed)
			{
				//VladD2: ѕроизсодим парсинг и построение дерева типв если это необходимо и прошло 
				//        некоторое врем€.
				if (periodic && !engine.IsProjectAvailable && engine.LastResetAstTime >= LastParseTime)
				{
					var delta = System.DateTime.Now - engine.LastResetAstTime;

					if (delta > ReparseDelta)
					{
						IVsTextView view = this.Service.GetPrimaryViewForSource(this);
						BeginParse(0, 0, new TokenInfo(), (ParseReason)ParseReason2.BuildTypeTree,
							view, new ParseResultHandler(this.HandleParseResponse));
					}
				}
			}

			// Kick of a background parse, but only in the periodic intervals
			if (!periodic || Service == null || Service.LastActiveTextView == null)
				return;

			if (!_isRegionsProcessed)
			{
				_isRegionsProcessed = true;
				IVsTextView view = this.Service.GetPrimaryViewForSource(this);
				BeginParse(0, 0, new TokenInfo(), (ParseReason)ParseReason2.ProcessRegions,
					view, new ParseResultHandler(this.HandleParseResponse));
				return;
			}

			if (IsDirty && LastDirtyTime >= LastParseTime)
			{
				var delta = DateTime.Now - LastDirtyTime;

				if (delta > ReparseDelta) // нам надо перепарсить исходник!
				{
					// Don't do background parsing while intellisense completion is going on
					if (completionSetIsDisplayed || methodDataIsDisplayed)
						return;

					if (engine == null || !engine.IsProjectAvailable)
						return;

					// ј здесь нам нужно парсить метод? код которого изменилс€ за это врем€, чтобы
					// отображались ошибки внесенные пользователем до прошлого парсинга.
					if (!Service.IsParsing)
					{
						if (_resetedMember != null)
						{
							LastParseTime = DateTime.Now;
							IVsTextView view = this.Service.GetPrimaryViewForSource(this);
							BeginParse(0, 0, new TokenInfo(), (ParseReason)ParseReason2.CheckRelocatedMember,
								view, new ParseResultHandler(this.HandleParseResponse));
						}
					}
				}
			}
		}

		internal void CheckRelocatedMember()
		{
			try
			{
				var methodBuilder = _resetedMember as Nemerle.Completion2.Factories.IntelliSenseModeMethodBuilder;

				if (methodBuilder != null)
					methodBuilder.EnsureCompiled();
				else
				{
					; // здесь нужно разбиратьс€ с инициализаторами полей
				}
				//BeginParse();
			}
			finally { _resetedMember = null; }
		}

		/// <include file='doc\Source.uex' path='docs/doc[@for="Source.BeginParse"]/*' />
		public override void BeginParse()
		{
			IVsTextView view = this.Service.GetPrimaryViewForSource(this);
			this.BeginParse(0, 0, new TokenInfo(), ParseReason.Check, view,
				new ParseResultHandler(this.HandleParseResponse));
			this.IsDirty = false;
		}

		public override void OnChangesCommitted(uint reason, TextSpan[] changedArea)
		{
			//var tskProv = GetTaskProvider();
			//var span = new TextSpan();
			//span.iStartLine  = 1;
			//span.iEndLine    = 1;
			//span.iStartIndex = 1;
			//span.iEndIndex   = 10;
			//var error = new DocumentTask(Service.Site, GetTextLines(), MARKERTYPE.MARKER_CODESENSE_ERROR,
			//  span, GetFilePath());
			//error.Text = "test";
			//error.Priority = Microsoft.VisualStudio.Shell.TaskPriority.High;
			//error.ErrorCategory = Microsoft.VisualStudio.Shell.TaskErrorCategory.Error;
			//tskProv.Tasks.Add(error);
			//tskProv.Refresh();

			Debug.WriteLine("OnChangesCommitted");
		}

		public override void Dispose()
		{
			if (ProjectInfo != null)
				ProjectInfo.RemoveSource(this);

			base.Dispose();
		}

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

		public override TokenInfo GetTokenInfo(int line, int col)
		{
			//get current line 
			TokenInfo info = new TokenInfo();
			var colorizer = GetColorizer() as NemerleColorizer;

			if (colorizer == null)
				return info;

			colorizer.SetCurrentLine(line);

			//get line info
			TokenInfo[] lineInfo = colorizer.GetLineInfo(this.GetTextLines(), line, this.ColorState);

			if (lineInfo != null)
			{
				//get character info      
				if (col > 0) 
					col--;

				this.GetTokenInfoAt(lineInfo, col, ref info);
			}

			return info;
		}

		public override void ProcessHiddenRegions(ArrayList hiddenRegions)
		{
			// TranslateMe:ss
			//VladD2: ѕриходитс€ переписывать реализацию от ћ—, так как она практически не расшир€етс€ как нужно нам.
			throw new NotImplementedException();
		}

		public void ProcessHiddenRegions(IList<NewHiddenRegion> hiddenRegions)
		{
#if DEBUG
			LanguageService.Preferences.MaxRegionTime = 1000 * 60 * 10;
#endif
			//this.hiddenRegions = hiddenRegions;

			if (!this.OutliningEnabled)
				return;

			// Compare the existing regions with the new regions and 
			// remove any that do not match the new regions.
			IVsHiddenTextSession session = GetHiddenTextSession();
			IVsEnumHiddenRegions ppenum;
			TextSpan[] aspan = new TextSpan[1];
			aspan[0] = GetDocumentSpan();
			ErrorHandler.ThrowOnFailure(session.EnumHiddenRegions((uint)FIND_HIDDEN_REGION_FLAGS.FHR_BY_CLIENT_DATA, (uint)HiddenRegionCookie, aspan, out ppenum));
			uint fetched;
			IVsHiddenRegion[] aregion = new IVsHiddenRegion[1];
			int matched = 0;
			int removed = 0;
			int added = 0;
			int pos = 0;

			// Create a list of IVsHiddenRegion objects, sorted in the same order that the
			// authoring sink sorts.  This is necessary because VS core editor does NOT return
			// the regions in the same order that we add them.
			ArrayList regions = new ArrayList();
			ArrayList spans = new ArrayList();

			while (ppenum.Next(1, aregion, out fetched) == NativeMethods.S_OK && fetched == 1)
			{
				ErrorHandler.ThrowOnFailure(aregion[0].GetSpan(aspan));
				TextSpan s = aspan[0];
				int i = spans.Count - 1;
				while (i >= 0)
				{
					TextSpan s2 = (TextSpan)spans[i];
					if (TextSpanHelper.StartsAfterStartOf(s, s2))
						break;
					i--;
				}
				spans.Insert(i + 1, s);
				regions.Insert(i + 1, aregion[0]);
			}

			// Now merge the list found with the list in the AuthoringSink to figure out
			// what matches, what needs to be removed, and what needs to be inserted.
			NewHiddenRegion r = pos < hiddenRegions.Count ? (NewHiddenRegion)hiddenRegions[pos] : new NewHiddenRegion();
			for (int i = 0, n = regions.Count; i < n; i++)
			{

				IVsHiddenRegion region = (IVsHiddenRegion)regions[i];
				TextSpan span = (TextSpan)spans[i];

				// In case we're inserting a new region, scan ahead to matching start line
				while (r.tsHiddenText.iStartLine < span.iStartLine)
				{
					pos++;
					if (pos >= hiddenRegions.Count)
					{
						r = new NewHiddenRegion();
						break;
					}
					else
					{
						r = (NewHiddenRegion)hiddenRegions[pos];
					}
				}
				if (TextSpanHelper.IsSameSpan(r.tsHiddenText, span) && HasSameBanner(r, region))
				{
					// This region is already there.
					matched++;
					hiddenRegions.RemoveAt(pos);
					r = (pos < hiddenRegions.Count) ? (NewHiddenRegion)hiddenRegions[pos] : new NewHiddenRegion();
				}
				else
				{
					removed++;
					ErrorHandler.ThrowOnFailure(region.Invalidate((int)CHANGE_HIDDEN_REGION_FLAGS.chrNonUndoable));
				}
			}

			int start = Environment.TickCount;
			if (hiddenRegions.Count > 0)
			{
				int count = hiddenRegions.Count;
				// For very large documents this can take a while, so add them in chunks of 
				// 1000 and stop after 5 seconds. 
				int maxTime = this.LanguageService.Preferences.MaxRegionTime;
				int chunkSize = 1000;
				NewHiddenRegion[] chunk = new NewHiddenRegion[chunkSize];
				int i = 0;
				while (i < count && Utils.TimeSince(start) < maxTime)
				{
					int j = 0;
					while (i < count && j < chunkSize)
					{
						r = (NewHiddenRegion)hiddenRegions[i];
						if (!TextSpanHelper.ValidSpan(this, r.tsHiddenText))
						{
#if	LANGTRACE
							Debug.Assert(false, "Invalid span " + r.tsHiddenText.iStartLine + "," + r.tsHiddenText.iStartIndex + "," + r.tsHiddenText.iEndLine + "," + r.tsHiddenText.iEndIndex);
#endif
							break;
						}
						else
						{
							chunk[j] = r;
							added++;
						}
						i++;
						j++;
					}
					int hr = session.AddHiddenRegions((int)CHANGE_HIDDEN_REGION_FLAGS.chrNonUndoable, j, chunk, null);
					if (ErrorHandler.Failed(hr))
					{
						break; // stop adding if we start getting errors.
					}
				}
			}

#if PERFTRACE
			int diff = TimeUtilities.TimeSince(start);
			Trace.WriteLine(String.Format(CultureInfo.InvariantCulture, "Hidden Regions: Matched={0}, Removed={1}, Addded={2}/{3} in {4} ms", matched, removed, added, hiddenRegions.Count, diff));
#endif
			Marshal.ReleaseComObject(ppenum);
		}

		//Banner comparison: in the case of editor controlled region, this is a noop
		//otherwise, compare banners
		//caveat is that GetBanner rountrips null to "...".
		private bool HasSameBanner(NewHiddenRegion r, IVsHiddenRegion region)
		{
			uint behavior;
			region.GetBehavior(out behavior);
			if (behavior == (uint)HIDDEN_REGION_BEHAVIOR.hrbEditorControlled && r.dwBehavior == (uint)HIDDEN_REGION_BEHAVIOR.hrbEditorControlled)
			{
				return true; //the banner text is always a fixed string, which is "..." by default
			}
			string currBanner;
			region.GetBanner(out currBanner);
			//<STRIP>DevDiv185498: Regression from RTM: Collapsed portions of XAML do not stay collapsed</STRIP>
			return r.pszBanner == currBanner || (r.pszBanner == null && currBanner == "...");
		}

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

		public override MethodData CreateMethodData()
		{
			return MethodData = base.CreateMethodData();
		}

		#region Paired chars insertion and deletion

		// TODO: maybe refactor this part so that it will share code with BracketFinder
		private readonly static char[][] _pairedChars = new char[][]
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
			if (textView == null || Service == null || !Service.Preferences.EnableCodeSense)
				return;

			bool backward = (command == VsCommands2K.BACKSPACE || command == VsCommands2K.BACKTAB ||
				command == VsCommands2K.LEFT || command == VsCommands2K.LEFT_EXT);

			int line, idx;
			textView.GetCaretPos(out line, out idx);

			//ScanLexer sl = ((NemerleScanner) (GetColorizer().Scanner)).GetLexer();
			
			//Tuple<GlobalEnv, TypeBuilder, int, int> ret =
			//	ProjectInfo.Project.GetActiveEnv(FileIndex, line + 1);
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

			if (!MethodData.IsDisplayed &&
				(tokenBeforeCaret.Trigger & TokenTriggers.MethodTip) != 0 &&
				command == VsCommands2K.TYPECHAR &&
				Service.Preferences.ParameterInformation)
			{
				MethodTip(textView, line, idx, tokenBeforeCaret);
			}
		}

		private void TryHighlightBraces(IVsTextView textView, VsCommands2K command, int line, int idx,
										TokenInfo tokenInfo)
		{
			// Highlight brace to the left from the caret
			if ((tokenInfo.Trigger & TokenTriggers.MatchBraces) != 0 && Service.Preferences.EnableMatchBraces)
			{
				if ( (command != VsCommands2K.BACKSPACE) && 
					(/*(command == VsCommands2K.TYPECHAR) ||*/
					Service.Preferences.EnableMatchBracesAtCaret)) 
				{		
					//if (!this.LanguageService.IsParsing)
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

		#region Implementation

		public void OnProjectReloaded()
		{
			_isRegionsProcessed = false;
		}

		internal static int HiddenRegionCookie = 25;

		internal void HandleParseResponse(ParseRequest req)
		{
			try
			{
				Trace.WriteLine("HandleParseResponse:" + req.Timestamp);
				if (this.Service == null) return;

				if (req.Timestamp == this.ChangeCount)
				{
					var sink = (NemerleAuthoringSink)req.Sink;
					// If the request is out of sync with the buffer, then the error spans
					// and hidden regions could be wrong, so we ignore this parse and wait 
					// for the next OnIdle parse.
					//!!ReportTasks(req.Sink.errors);
					if (req.Sink.ProcessHiddenRegions)
						ProcessHiddenRegions(sink.HiddenRegionsList);
				}
				this.Service.OnParseComplete(req);

			}
			catch (Exception e)
			{
				Trace.WriteLine("HandleParseResponse exception: " + e.Message);
			}
		}

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

		#endregion
	}
}
