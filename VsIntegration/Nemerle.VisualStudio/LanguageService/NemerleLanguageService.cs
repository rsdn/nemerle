using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Compiler;
using Nemerle.Completion2;

using Nemerle.VisualStudio.Project;

using VsShell = Microsoft.VisualStudio.Shell.VsShellUtilities;
using Nemerle.VisualStudio.GUI;
using Nemerle.Utility;

using Nemerle.VisualStudio.Properties;
using Microsoft.VisualStudio.Package;

namespace Nemerle.VisualStudio.LanguageService
{
	///<summary>
	/// This is the base class for a language service that supplies language features including syntax highlighting, brace matching, auto-completion, IntelliSense support, and code snippet expansion.
	///</summary>
	[Guid(NemerleConstants.LanguageServiceGuidString)]
	public class NemerleLanguageService : Microsoft.VisualStudio.Package.LanguageService
	{
		#region Fields

		public static Engine DefaultEngine { get; private set; }
		
		#endregion
		
		#region Init
		
		public NemerleLanguageService()
		{
			CompiledUnitAstBrowser.ShowLocation += GotoLocation;
			AstToolControl.ShowLocation += GotoLocation;

			if (DefaultEngine == null)
			{
				DefaultEngine = new Engine(EngineCallbackStub.Default,
					new ProjectManager(this), new TraceWriter());

				DefaultEngine.InitDefaulteEngine();
			}
		}

		///<summary>
		///Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
		///</summary>
		public override void Dispose()
		{
			try
			{
				foreach (NemerleColorizer colorizer in _colorizers.Values)
					colorizer.Dispose();

				_colorizers.Clear();

				if (_preferences != null)
				{
					_preferences.Dispose();
					_preferences = null;
				}
			}
			finally
			{
				base.Dispose();
			}
		}

		#endregion

		#region ParseSource

		#region ParseSource()

		public override AuthoringScope ParseSource(ParseRequest request)
		{
			if (request == null)
				throw new ArgumentNullException("request");

			Debug.Print(
				"File '{0}' ParseSource at ({1}:{2}), reason {3}, Timestamp {4}",
				Path.GetFileName(request.FileName), request.Line, request.Col, request.Reason,
				request.Timestamp);

			switch (request.Reason)
			{
				case (ParseReason)ParseReason2.CheckRelocatedMember: 
				                               return CheckRelocatedMember(request);
				case (ParseReason)ParseReason2.ProcessRegions:
																			 return ProcessRegions(request);
				case (ParseReason)ParseReason2.BuildTypeTree:
																			 return BuildTypeTree(request);

				case ParseReason.Check:        return Check(request);

				case ParseReason.MemberSelect:
				case ParseReason.CompleteWord: return GetCompleteWord(request);

				case ParseReason.Goto:
				case ParseReason.QuickInfo:    return GetMethodScope(request);

				case ParseReason.MethodTip:    return GetMethodTip(request);

				case ParseReason.HighlightBraces:
				case ParseReason.MatchBraces:  return MatchBraces(request);

				case ParseReason.Autos:
				case ParseReason.CodeSpan:
				case ParseReason.DisplayMemberList:
				case ParseReason.None:
				case ParseReason.MemberSelectAndHighlightBraces:
					Trace.WriteLine("Request reason '" + request.Reason + "' not handled.");
					break;
			}

			NemerleSource source = (NemerleSource)GetSource(request.View);

			if (source != null && source.ScopeCreator != null)
				return source.ScopeCreator(request);

			return GetDefaultScope(request);
		}

		private AuthoringScope BuildTypeTree(ParseRequest request)
		{
			Trace.WriteLine(">>>> ##### ProcessRegions!");
			try
			{
				ProjectInfo projectInfo = ProjectInfo.FindProject(request.FileName);
				if (projectInfo == null)
					return null;
			
				projectInfo.Engine.BuildTypeTree();

				// Now compiler messages set to Error List window automatically (by IEngineCallback).
				// We don't need return it in AuthoringScope.
				return GetDefaultScope(request);
			}
			catch (Exception e)
			{
				Trace.WriteLine("!!! ProcessRegions() throw Exception " + e.Message);
				return GetDefaultScope(request); //	VladD2: 2 IT: Don't re-throw exception here! It leads to check loophole!!!
			}
			finally { Trace.WriteLine("<<<< ##### ProcessRegions!"); }
		}
 
		#endregion


		#region MatchBraces

		#region MatchBraces()

		private AuthoringScope MatchBraces(ParseRequest request)
		{
			if (!request.Sink.BraceMatching)
				return GetDefaultScope(request);

			// Steps: 
			// 1. Find token under text caret.
			// 2. Determine that it is a paired token.
			// 3. Determine paired token.
			// 4. Find paired token in the source file.
			// 5. Set info about paired tokens Sink and return it in AuthoringScope.

			#region Init vars

			ProjectInfo projectInfo = GetProjectInfo(request);

			var source = (NemerleSource)GetSource(request.View);
			IVsTextColorState colorState = source.ColorState;
			Colorizer colorizer = source.GetColorizer();
			var scanner = (NemerleScanner)colorizer.Scanner;
			string lineText = source.GetLine(request.Line);
			scanner.SetSource(lineText, 0);

			#endregion

			// Steps: 1-3
			BracketFinder bracketFinder = new BracketFinder(source, request.Line + 1,
				request.Col + 1, scanner, colorState);

			// 4. Find paired token in the source file.
			ScanTokenInfo matchBraceInfo = bracketFinder.FindMatchBraceInfo();

			if (matchBraceInfo != null)
			{
				// 5. Set info about paired tokens Sink and return it in AuthoringScope.

				request.Sink.FoundMatchingBrace = true;

				// Fix a bug in MPF: Correct location of left token.
				// It need for correct navigation (to left side of token).
				//
				Token    matchToken    = matchBraceInfo.Token;
				Location matchLocation = request.Reason == ParseReason.MatchBraces 
					&& !BracketFinder.IsOpenToken(matchToken)
					? matchToken.Location.FromEnd() : matchToken.Location;

				// Set tokens position info
				//
				request.Sink.MatchPair(
					Utils.SpanFromLocation(bracketFinder.StartBraceInfo.Token.Location),
					Utils.SpanFromLocation(matchLocation), 0);

				return new NemerleAuthoringScope(
					projectInfo, request.Sink, request.FileName,
					new SourceTextManager(source)); // projectInfo.GetSource(request.FileName)
			}

			return GetDefaultScope(request); // we don't find paired token
		}
		
		#endregion

		#region BracketFinder class

		/// <summary>
		/// Helper class which match paired token (brackets, brace, etc.)
		/// </summary>
		private class BracketFinder
		{
			#region Fields
			
			public  readonly ScanTokenInfo     StartBraceInfo;
			private readonly IVsTextColorState ColorState;
			private readonly NemerleSource     Source;
			private readonly int               StartLine;
			private readonly ScanLexer         Lex;
			private readonly NemerleScanner    Scanner;

			#endregion

			public BracketFinder(NemerleSource source, int startLine,
				int startCol, NemerleScanner scanner, IVsTextColorState colorState)
			{
				#region Init fields

				Scanner = scanner;
				Source = source;
				StartLine = startLine;
				Lex = scanner.GetLexer();
				Lex.SetFileName(source.GetFilePath());
				ColorState = colorState;
				
				#endregion

				#region 2. Determine that it is a paired token. 3. Determine paired token.
				
				// Get tokens of line under text carret into dynamic array.
				List<ScanTokenInfo> lineToks = GetLineTokens(startLine, true);
				// Find index of token which located under text carret.
				int index = FindIndex(lineToks,
					delegate(ScanTokenInfo x)
					{ return x.Token.Location.Contains(startLine, startCol); });

				// If index is corret get corresponding token.
				ScanTokenInfo startBraceInfo = index < 0 ? null : lineToks[index];
				// Remember it, if token have paired token.
				if (IsPairedToken(startBraceInfo.Token))
					StartBraceInfo = startBraceInfo;
				else
				{
					// otherwise try get right-hand token...
					startBraceInfo = RightHand(lineToks, index);
					// Remember it, if token have paired token.
					if (IsPairedToken(startBraceInfo.Token))
						StartBraceInfo = startBraceInfo;
				} 

				#endregion
			}

			public ScanTokenInfo FindMatchBraceInfo()
			{
				if (StartBraceInfo == null)
					return null;

				// 4. Find paired token in the source file.

				Token tok = StartBraceInfo.Token;
				Predicate<Token> isStartBrace = GetMatchBracePredicate(tok);
				Predicate<Token> isEndBrace = GetMatchBracePredicate(GetPairedToken(tok));
				int nestedLevel = 1;

				foreach (ScanTokenInfo tokInfo in GetTokenIter())
				{
					if (isEndBrace(tokInfo.Token))
						nestedLevel--;
					else if (isStartBrace(tokInfo.Token))
						nestedLevel++;
					if (nestedLevel == 0)
						return tokInfo; // Match found!
				}

				return null; // Match not found.
			}

			#region GetTokenIter()

			/// <summary>
			/// Return iterator which allow iterate throw tokens of curent 
			/// source file. Iteration process start from token behind current 
			/// token and go forward or backward depending on type of start token.
			/// If start token is open token (for example, "(" or "{") iteration
			/// execute forward. If start token is close token (for example, ")" 
			/// or "]") iteration execute backward.
			/// </summary>
			private IEnumerable<ScanTokenInfo> GetTokenIter()
			{
				bool isScanForward = IsOpenToken(StartBraceInfo.Token);
				int startLine = StartLine;
				// 1. Для первой строки находим токен и начиная от него сканируем вперед или назад.
				List<ScanTokenInfo> tokInfs = new List<ScanTokenInfo>(
					GetLineTokens(startLine, isScanForward));

				int tokIndex = FindIndex(tokInfs, delegate(ScanTokenInfo ti)
					{ return ti.Token.Location == StartBraceInfo.Token.Location; });
				if (tokIndex < 0)
					throw new Exception("tokInfs.IndexOf(startBraceInfo)");

				// Scan first line from "start bracket" index.
				for (int i = tokIndex + 1; i < tokInfs.Count; i++)
					yield return tokInfs[i];

				if (isScanForward) // Scan next lines.
					for (int i = startLine + 1, count = Source.GetLineCount(); i <= count; i++)
						foreach (ScanTokenInfo tokInf in GetLineTokens(i, true))
							yield return tokInf;
				else // Scan previous lines.
					for (int i = startLine - 1; i > 0; i--)
						foreach (ScanTokenInfo tokInf in GetLineTokens(i, false))
							yield return tokInf;
			}
 
			#endregion

			#region GetLineTokens()

			/// <summary>
			/// Get tokens of specified line.
			/// </summary>
			/// <param name="line">Line number which tokens it is necessary retrieve</param>
			/// <param name="isForward">Direction of iteration (true is forward)</param>
			/// <returns>Token list</returns>
			List<ScanTokenInfo> GetLineTokens(int line, bool isForward)
			{
				ScanState scanState = GetLineState(line);
				string lineText = Source.GetLine(line - 1);
				Scanner.SetSource(lineText, 0);
				Lex.SetLine(line, lineText, 0, null, null);
				List<ScanTokenInfo> lst = new List<ScanTokenInfo>();

				foreach (ScanTokenInfo var in GetLineTokens(Lex, scanState))
					lst.Add(var.Clone());

				if (!isForward)
					lst.Reverse();

				return lst;
			}

			/// <summary>Return colorer lexer state for specified line.</summary>
			private ScanState GetLineState(int line)
			{
				int state;
				ErrorHandler.ThrowOnFailure(
					ColorState.GetColorStateAtStartOfLine(line, out state));
				return (ScanState)state;
			}

			/// <summary>
			/// Implementation of<c>GetLineTokens</c>(). Don't use this method directly!
			/// </summary>
			IEnumerable<ScanTokenInfo> GetLineTokens(ScanLexer lex, ScanState scanState)
			{
				ScanTokenInfo info = lex.GetToken(scanState);
				scanState = info.State;
				while (!info.IsEndOfLine)
				{
					yield return info;
					info = lex.GetToken(scanState);
					scanState = info.State;
				}
			}
 
			#endregion

			#region Paired token identification functions

			public static bool IsOpenToken(Token token)
			{
				if (token is Token.BeginBrace)  return true;
				if (token is Token.BeginQuote)  return true;
				if (token is Token.BeginRound)  return true;
				if (token is Token.BeginSquare) return true;

				if (token is Token.EndBrace)  return false;
				if (token is Token.EndQuote)  return false;
				if (token is Token.EndRound)  return false;
				if (token is Token.EndSquare) return false;

				Token.Keyword kwd = token as Token.Keyword;

				if (kwd != null)
				{
					if (kwd.name == "if")   return true;
					if (kwd.name == "else") return false;
				}

				throw new ArgumentException(string.Format("The token '{0}' not a brace!", token), "token");
			}

			private static bool IsPairedToken(Token token)
			{
				if (token is Token.BeginBrace  || token is Token.BeginQuote
					|| token is Token.BeginRound || token is Token.BeginSquare
					|| token is Token.EndBrace   || token is Token.EndQuote
					|| token is Token.EndRound   || token is Token.EndSquare
				)
					return true;

				var kwd = token as Token.Keyword;

				if (kwd != null && (kwd.name == "if" || kwd.name == "else"))
					return true;

				return false;
			}

			/// <summary>
			/// Return predicate function which check it argument is same token .
			/// </summary>
			private static Predicate<Token> GetMatchBracePredicate(Token token)
			{
				if (token is Token.BeginBrace)  return t => t is Token.BeginBrace;
				if (token is Token.BeginQuote)  return t => t is Token.BeginQuote;
				if (token is Token.BeginRound)  return t => t is Token.BeginRound;
				if (token is Token.BeginSquare) return t => t is Token.BeginSquare;

				if (token is Token.EndBrace)    return t => t is Token.EndBrace;
				if (token is Token.EndQuote)    return t => t is Token.EndQuote;
				if (token is Token.EndRound)    return t => t is Token.EndRound;
				if (token is Token.EndSquare)   return t => t is Token.EndSquare;

				Token.Keyword kwd = token as Token.Keyword;

				if (kwd != null)
				{
					if (kwd.name == "if")
						return delegate(Token t)
						{
							Token.Keyword kwd1 = t as Token.Keyword;

							return kwd1 != null && kwd1.name == "if";
						};
					if (kwd.name == "else")
						return delegate(Token t)
						{
							Token.Keyword kwd1 = t as Token.Keyword;
							return kwd1 != null && kwd1.name == "else";
						};
				}

				return null;
			}

			private static Token GetPairedToken(Token token)
			{
				if (token is Token.BeginBrace) return new Token.EndBrace(true);
				if (token is Token.BeginQuote) return new Token.EndQuote();
				if (token is Token.BeginRound) return new Token.EndRound();
				if (token is Token.BeginSquare) return new Token.EndSquare();

				if (token is Token.EndBrace) return new Token.BeginBrace(true);
				if (token is Token.EndQuote) return new Token.BeginQuote();
				if (token is Token.EndRound) return new Token.BeginRound();
				if (token is Token.EndSquare) return new Token.BeginSquare();

				Token.Keyword kwd = token as Token.Keyword;

				if (kwd != null)
				{
					if (kwd.name == "if")   return new Token.Keyword("else");
					if (kwd.name == "else") return new Token.Keyword("if");
				}

				return null;
			}
 
			#endregion

			#region Utils

			static T RightHand<T>(IList<T> lst, int index) where T : new()
			{
				int nextIndex = index + 1;
				if (nextIndex >= lst.Count)
					return new T();

				return lst[nextIndex];
			}

			static int FindIndex<T>(IList<T> lst, Predicate<T> predicate) where T : class
			{
				for (int i = 0; i < lst.Count; i++)
					if (predicate(lst[i]))
						return i;

				return -1;
			}
 
			#endregion
		}

		#endregion 

		#endregion

		#region Check

		private AuthoringScope CheckRelocatedMember(ParseRequest request)
		{
			var source = (NemerleSource)GetSource(request.View);

			if (source != null)
				source.CheckRelocatedMember();

			// Now compiler messages set to Error List window automatically (by IEngineCallback).
			// We don't need return it in AuthoringScope.
			return GetDefaultScope(request);
		}

		private AuthoringScope ProcessRegions(ParseRequest request)
		{
			Trace.WriteLine(">>>> ##### ProcessRegions!");
			try
			{
				ProjectInfo projectInfo = ProjectInfo.FindProject(request.FileName);
				//TODO: To eliminate dependence from ProjectInfo.
				if (projectInfo == null)
					return null;

				NemerleSource source = GetSource(request.View) as NemerleSource;

				if (source == null)
					return null;

				bool allExpanded = source.TimeStamp > 0;

				request.Sink.ProcessHiddenRegions = true;

				projectInfo.Project.Check(
					request.FileName,
					new SourceTextManager(source),
					(location, text, isExpanded) =>
					{
						if (location.Line < location.EndLine)
						{
							var r = new NewHiddenRegion
							{
								tsHiddenText = Utils.SpanFromLocation(location),
								iType = (int)HIDDEN_REGION_TYPE.hrtCollapsible,
								dwBehavior = (int)HIDDEN_REGION_BEHAVIOR.hrbEditorControlled, //.hrbClientControlled;
								pszBanner = string.IsNullOrEmpty(text) ? null : text,
								dwClient = 25,
								dwState = (uint)(allExpanded || isExpanded ?
									HIDDEN_REGION_STATE.hrsExpanded : HIDDEN_REGION_STATE.hrsDefault)
							};

							request.Sink.AddHiddenRegion(r);
						}
					});

				var scope = GetDefaultScope(request);
				var tool = AstToolWindow.AstTool;

				if (tool != null && tool.IsAutoUpdate)
					tool.ShowInfo(source);

				return scope;
			}
			catch (Exception e)
			{
				Trace.WriteLine("!!! ProcessRegions() throw Exception " + e.Message);
				return GetDefaultScope(request); //	VladD2: 2 IT: Don't re-throw exception here! It leads to check loophole!!!
			}
			finally { Trace.WriteLine("<<<< ##### ProcessRegions!"); }
		}

		private AuthoringScope Check(ParseRequest request)
		{
			Trace.WriteLine(">>>> ##### Check!");
			try
			{
				ProjectInfo projectInfo = ProjectInfo.FindProject(request.FileName);

				if (projectInfo == null)
					return null;

				projectInfo.ChekOneMethodFromMethodsCheckQueue(request.View);

				return GetDefaultScope(request); // Now compiler messages set to Error List window automatically (by IEngineCallback). // We don't need return it in AuthoringScope.
			}
			catch (Exception e)
			{
				Trace.WriteLine("!!! Check() throw Exception " + e.Message);
				return GetDefaultScope(request); //	VladD2: 2 IT: Don't re-throw exception here! It leads to check loophole!!!
			}
			finally { Trace.WriteLine("<<<< ##### Check!"); }
		}
		
		#endregion

		#region Highlight

		private bool NowIsTerminalSession()
		{
			return false;
		}

		// HiglightUsages finds usages of a token and highlights it.
		// It needs a file index and a location of a cursor
		private void HighlightUsages(ParseRequest request)
		{
			ProjectInfo projectInfo = GetProjectInfo(request);

			if (projectInfo == null)
				return;

			if (Settings.Default.HighlightUsages)
				if (!Settings.Default.HighlightUsagesUnlessTerminalSession || !NowIsTerminalSession())
					projectInfo.HighlightUsages(request.FileName,
												request.Line,
												request.Col,
												new SourceTextManager(projectInfo.GetSource(request.FileName)),
												false);
		}

		#endregion

		#region GetCompleteWord

		private AuthoringScope GetCompleteWord(ParseRequest request)
		{
			try
			{
				ProjectInfo projectInfo = GetProjectInfo(request);

				if (projectInfo == null)
					return null;

				CompletionElem[] overloads = projectInfo.CompleteWord(
					request.FileName, request.Line, request.Col,
					new SourceTextManager(projectInfo.GetSource(request.FileName)));

				if (overloads.Length > 0)
					return new NemerleAuthoringScope(projectInfo, request.Sink, overloads);
			}
			catch (Exception ex)
			{
				Debug.Assert(false, ex.ToString());
				Trace.WriteLine(ex);
			}

			return GetDefaultScope(request);
		}
 
		#endregion

		#region GetMethodScope

		private AuthoringScope GetMethodScope(ParseRequest request)
		{
			HighlightUsages(request);

			string text;

			int res = request.View.GetTextStream(
				request.Line, request.Col, request.Line, request.Col + 1, out text);

			if (res != VSConstants.S_OK || text.Length == 0 || text[0] == ' ' || text[0] == '\t')
				return null;

			ProjectInfo projectInfo = GetProjectInfo(request);

			if (projectInfo == null)
				return null;

			return new NemerleAuthoringScope(
				projectInfo, request.Sink, request.FileName,
				new SourceTextManager(projectInfo.GetSource(request.FileName)));
		}
 
		#endregion

		#region GetMethodTip

		private AuthoringScope GetMethodTip(ParseRequest request)
		{
			ProjectInfo projectInfo = GetProjectInfo(request);

			if (projectInfo == null)
				return null;

			int col = request.Col;

			if (request.TokenInfo != null &&
				(request.TokenInfo.Trigger & TokenTriggers.ParameterStart) == TokenTriggers.ParameterStart &&
				request.Col == request.TokenInfo.StartIndex)
			{
				col++;
			}

			NemerleMethods methods = projectInfo.GetMethodTip(
				request.FileName, request.Line, col,
				new SourceTextManager(projectInfo.GetSource(request.FileName)));

			if (methods != null)
			{
				if (methods.StartName.EndLine > 0)
				{
					request.Sink.StartName(Utils.SpanFromLocation(methods.StartName), methods.GetName(0));
					request.Sink.StartParameters(Utils.SpanFromLocation(methods.StartParameters));

					foreach (Location loc in methods.NextParameters)
						request.Sink.NextParameter(Utils.SpanFromLocation(loc));

					request.Sink.EndParameters(Utils.SpanFromLocation(methods.EndParameters));
				}
				else
				{
					TextSpan ts = new TextSpan();

					ts.iStartIndex = request.Line;
					ts.iEndIndex = request.Line;
					ts.iStartIndex = request.Col - 1;
					ts.iEndIndex = request.Col + 1;

					request.Sink.StartName(ts, methods.GetName(0));
				}

				return new NemerleAuthoringScope(
					ProjectInfo.FindProject(request.FileName),
					request.Sink, methods);
			}

			return GetDefaultScope(request);
		}
 
		#endregion

		#region Utils

		private AuthoringScope GetDefaultScope(ParseRequest request)
		{
			ProjectInfo projectInfo = ProjectInfo.FindProject(request.FileName);

			if (projectInfo != null)
				return new NemerleAuthoringScope(
					projectInfo, request.Sink, request.FileName,
					new SourceTextManager(projectInfo.GetSource(request.FileName)));

			return null;
		}

		private ProjectInfo GetProjectInfo(ParseRequest request)
		{
			ProjectInfo projectInfo = ProjectInfo.FindProject(request.FileName);

			if (projectInfo != null)
				projectInfo.UpdateFile(request);

			return projectInfo;
		}

		#endregion

		#endregion

		#region Colorizing

		// This array contains the definition of the colorable items provided by
		// this language service.
		// This specific language does not really need to provide colorable items
		// because it does not define any item different from the default ones,
		// but the base class has an empty implementation of
		// IVsProvideColorableItems, so any language service that derives from
		// it must implement the methods of this interface, otherwise there are
		// errors when the shell loads an editor to show a file associated to
		// this language.
		private static readonly NemerleColorableItem[] _colorableItems = 
		{
			// The sequential order of these items should be consistent with the ScanTokenColor enum.
			//
			new NemerleColorableItem("Keyword",				  COLORINDEX.CI_BLUE),
			new NemerleColorableItem("Comment",				  COLORINDEX.CI_DARKGREEN),
			new NemerleColorableItem("Identifier"),
			new NemerleColorableItem("String",				   COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("Number"),
			new NemerleColorableItem("Text"),

			new NemerleColorableItem("Operator"),
			new NemerleColorableItem("Preprocessor Keyword",	 COLORINDEX.CI_BLUE,   Color.FromArgb(  0, 51, 204)),
			new NemerleColorableItem("StringEx",				 COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),
			new NemerleColorableItem("String (@ Verbatim)",   2, COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("StringEx (@ Verbatim)", 2, COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),

			new NemerleColorableItem("User Types",			   COLORINDEX.CI_CYAN,   Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("User Types (Delegates)",   COLORINDEX.CI_CYAN,   Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("User Types (Enums)",	   COLORINDEX.CI_CYAN,   Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("User Types (Interfaces)",  COLORINDEX.CI_CYAN,   Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("User Types (Value types)", COLORINDEX.CI_CYAN,   Color.FromArgb(43, 145, 175)),

			new NemerleColorableItem("Quotation",			 0, COLORINDEX.CI_BROWN),

			new NemerleColorableItem("<[ Text ]>",			0),
			new NemerleColorableItem("<[ Keyword ]>",		 0, COLORINDEX.CI_BLUE),
			new NemerleColorableItem("<[ Comment ]>",		 0, COLORINDEX.CI_DARKGREEN),
			new NemerleColorableItem("<[ Identifier ]>",	  0),
			new NemerleColorableItem("<[ String ]>",		  0, COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("<[ Number ]>",		  0),
			new NemerleColorableItem("<[ Operator ]>",		0),
			new NemerleColorableItem("<[ StringEx ]>",		0, COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),
			new NemerleColorableItem("<[ String (@) ]>",	  1, COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("<[ StringEx (@) ]>",	1, COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),

			new NemerleColorableItem("<[ User Types ]>",			   0, COLORINDEX.CI_CYAN, Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("<[ User Types (Delegates) ]>",   0, COLORINDEX.CI_CYAN, Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("<[ User Types (Enums) ]>",	   0, COLORINDEX.CI_CYAN, Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("<[ User Types (Interfaces) ]>",  0, COLORINDEX.CI_CYAN, Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("<[ User Types (Value types) ]>", 0, COLORINDEX.CI_CYAN, Color.FromArgb(43, 145, 175)),

			new NemerleColorableItem("Highlight One", COLORINDEX.CI_BLACK, COLORINDEX.CI_YELLOW, Color.Empty, Color.FromArgb(135, 206, 250)),
			new NemerleColorableItem("Highlight Two", COLORINDEX.CI_BLACK, COLORINDEX.CI_GREEN, Color.Empty, Color.FromArgb(255, 182, 193)),

			new NemerleColorableItem("TODO comment", COLORINDEX.CI_BLUE, Color.FromArgb(0,  175, 255)),
			new NemerleColorableItem("BUG comment",  COLORINDEX.CI_RED,  Color.FromArgb(255, 75,  75), FONTFLAGS.FF_BOLD),
			new NemerleColorableItem("HACK comment", COLORINDEX.CI_RED,  Color.FromArgb(145,  0,   0)),

			new NemerleColorableItem("<[ TODO comment ]>", 0, COLORINDEX.CI_BLUE, Color.FromArgb(0,  175, 255)),
			new NemerleColorableItem("<[ BUG comment ]>",  0, COLORINDEX.CI_RED,  Color.FromArgb(255, 75,  75), FONTFLAGS.FF_BOLD),
			new NemerleColorableItem("<[ HACK comment ]>", 0, COLORINDEX.CI_RED,  Color.FromArgb(145,  0,   0)),

			new NemerleColorableItem("String (<# #>)",		 2, COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("StringEx (<# #>)",	   2, COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),
			new NemerleColorableItem("<[ String (<# #>) ]>",   1, COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("<[ StringEx (<# #>) ]>", 1, COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),
			
			new NemerleColorableItem("Field Identifier", COLORINDEX.CI_DARKBLUE, Color.FromArgb(128, 0, 128)),
			new NemerleColorableItem("Event Identifier", COLORINDEX.CI_MAGENTA, Color.FromArgb(255, 0, 255)),
			new NemerleColorableItem("Method Identifier", COLORINDEX.CI_DARKBLUE, Color.FromArgb(0, 139, 139)),
			new NemerleColorableItem("Property Identifier", COLORINDEX.CI_DARKBLUE, Color.FromArgb(128, 0, 128)),

		};

		Dictionary<IVsTextLines, NemerleColorizer> _colorizers = new Dictionary<IVsTextLines,NemerleColorizer>();

		public override Colorizer GetColorizer(IVsTextLines buffer)
		{
			NemerleColorizer colorizer;

			if (!_colorizers.TryGetValue(buffer, out colorizer))
			{
				colorizer = new NemerleColorizer(this, buffer, (NemerleScanner)GetScanner(buffer));

				_colorizers.Add(buffer, colorizer);
			}

			return colorizer;
		}

		public override IScanner GetScanner(IVsTextLines buffer)
		{
			return new NemerleScanner(this, buffer);
		}

		// Implementation of IVsProvideColorableItems.
		//
		public override int GetItemCount(out int count)
		{
			count = _colorableItems.Length;
			return VSConstants.S_OK;
		}

		public override int GetColorableItem(int index, out IVsColorableItem item)
		{
			if (index < 1)
				throw new ArgumentOutOfRangeException("index");

			item = _colorableItems[index - 1];

			return VSConstants.S_OK;
		}

		#endregion

		#region Source

		public override string Name
		{
			get { return Resources.Nemerle; }
		}

		public override Source CreateSource(IVsTextLines buffer)
		{
			return new NemerleSource(this, buffer, GetColorizer(buffer));
		}

		public override CodeWindowManager CreateCodeWindowManager(IVsCodeWindow codeWindow, Source source)
		{
			CodeWindowManager m = base.CreateCodeWindowManager(codeWindow, source);

			// It compiles the source right after it's loaded.
			//
			source.BeginParse();

			return m;
		}

		#endregion

		#region Snippets

		private int classNameCounter = 0;

		public override ExpansionFunction CreateExpansionFunction(
			ExpansionProvider provider, string functionName)
		{
			ExpansionFunction function = null;

			if (functionName == "GetName")
			{
				++classNameCounter;
				function = new NemerleGetNameExpansionFunction(provider, classNameCounter);
			}

			return function;
		}

		private List<VsExpansion> _expansionsList;
		private List<VsExpansion> ExpansionsList
		{
			get
			{
				if (_expansionsList != null)
					return _expansionsList;

				GetSnippets();
				return _expansionsList;
			}
		}

		// Disable the "DoNotPassTypesByReference" warning.
		//
		public void AddSnippets(ref NemerleDeclarations declarations)
		{
			if (null == ExpansionsList)
				return;

			foreach (VsExpansion expansionInfo in ExpansionsList)
			{
				//declarations.AddDeclaration(new Declaration(expansionInfo));
				throw new NotImplementedException();
			}
		}

		private void GetSnippets()
		{
			if (null == _expansionsList)
				_expansionsList = new List<VsExpansion>();
			else
				_expansionsList.Clear();

			IVsTextManager2 textManager = 
				Microsoft.VisualStudio.Shell.Package.GetGlobalService(
				typeof(SVsTextManager)) as IVsTextManager2;

			if (textManager == null)
				return;

			SnippetsEnumerator enumerator = new SnippetsEnumerator(
				textManager, GetLanguageServiceGuid());

			foreach (VsExpansion expansion in enumerator)
				if (!string.IsNullOrEmpty(expansion.shortcut))
					_expansionsList.Add(expansion);
		}

		internal class NemerleGetNameExpansionFunction : ExpansionFunction
		{
			private int nameCount;

			public NemerleGetNameExpansionFunction(ExpansionProvider provider, int counter)
				: base(provider)
			{
				nameCount = counter;
			}

			public override string GetCurrentValue()
			{
				string name = "MyClass";
				name += nameCount.ToString(CultureInfo.InvariantCulture);
				return name;
			}
		}

		#endregion

		#region Navigation DropDown

		public override TypeAndMemberDropdownBars CreateDropDownHelper(IVsTextView forView)
		{
			if (Preferences.ShowNavigationBar)
				return new NemerleTypeAndMemberDropdownBars(this, forView);
			else
				return null;
		}

		/// <include file='doc\LanguageService.uex' path='docs/doc[@for="LanguageService.OnCaretMoved"]/*' />
		/// Переопределяем этот метод, чтобы вызвать SynchronizeDropdowns у 
		/// NemerleTypeAndMemberDropdownBars, а не CodeWindowManager.
		public override void OnCaretMoved(CodeWindowManager mgr, IVsTextView textView, int line, int col)
		{
			if (mgr.DropDownHelper != null)
			{
				var dropDownHelper = (NemerleTypeAndMemberDropdownBars)mgr.DropDownHelper;
				dropDownHelper.SynchronizeDropdownsRsdn(textView, line, col);
			}
		}

		#endregion

		#region LanguagePreferences

		LanguagePreferences _preferences;

		public override LanguagePreferences GetLanguagePreferences()
		{
			if (_preferences == null)
			{
				_preferences = new LanguagePreferences(Site, typeof(NemerleLanguageService).GUID, Name);

				// Setup default values.
				//
				_preferences.ShowNavigationBar	 = true;

				// Load from the registry.
				//
				_preferences.Init();
				_preferences.EnableFormatSelection = true;

				// TODO: Find out how to enable "Smart" radio option in 
				// Tools->Options->Text editor->Nemerle->Tabs
				//_preferences.IndentStyle = IndentingStyle.Smart;

				//VladD2: Switch on synchronous mode for debugging purpose!
				//TODO: Comment it if necessary.
				_preferences.EnableAsyncCompletion = false;
			}

			return _preferences;
		}

		#endregion

		#region Debugging

		#region IVsLanguageDebugInfo methods

		public override int GetLocationOfName(string name, out string pbstrMkDoc, TextSpan[] spans)
		{
			pbstrMkDoc = null;
			return NativeMethods.E_NOTIMPL;
		}

		public override int GetNameOfLocation(
			IVsTextBuffer buffer,
			int line,
			int col,
			out string name,
			out int lineOffset)
		{
			name = null;
			lineOffset = 0;
			/*
		 TRACE1( "LanguageService(%S)::GetNameOfLocation", m_languageName );
		OUTARG(lineOffset);
		OUTARG(name);
		INARG(textBuffer);

		HRESULT hr;
		IScope* scope = NULL;
		hr = GetScopeFromBuffer( textBuffer, &scope );
		if (FAILED(hr)) return hr;
  
		long realLine = line;
		hr = scope->Narrow( line, idx, name, &realLine );
		RELEASE(scope);
		if (hr != S_OK) return hr;

		*lineOffset = line - realLine;
		return S_OK;
	  */
			return NativeMethods.S_OK;
		}

		public override int GetProximityExpressions(
			IVsTextBuffer buffer,
			int line,
			int col,
			int cLines,
			out IVsEnumBSTR ppEnum)
		{
			ppEnum = null;
			/*
		TRACE2( "LanguageService(%S)::GetProximityExpressions: line %i", m_languageName, line );
		OUTARG(exprs);
		INARG(textBuffer);

		//check the linecount
		if (lineCount <= 0) lineCount = 1;

		//get the source 
		//TODO: this only works for sources that are opened in the environment
		HRESULT hr;
		Source* source = NULL;
		hr = GetSource( textBuffer, &source );
		if (FAILED(hr)) return hr;

		//parse and find the proximity expressions
		StringList* strings = NULL;
		hr = source->GetAutos( line, line + lineCount, &strings );
		RELEASE(source);
		if (FAILED(hr)) return hr;

		hr = strings->QueryInterface( IID_IVsEnumBSTR, reinterpret_cast<void**>(exprs) );
		RELEASE(strings);
		if (FAILED(hr)) return hr;
  
		return S_OK;
	  */
			return NativeMethods.S_FALSE;
		}

		public override int IsMappedLocation(IVsTextBuffer buffer, int line, int col)
		{
			return NativeMethods.S_FALSE;
		}

		public override int ResolveName(string name, uint flags, out IVsEnumDebugName ppNames)
		{
			ppNames = null;
			return NativeMethods.E_NOTIMPL;
		}

		public override int ValidateBreakpointLocation(
			IVsTextBuffer buffer, 
			int		   line, 
			int		   col, 
			TextSpan[]	pCodeSpan
		)
		{
			if (pCodeSpan != null)
			{
				pCodeSpan[0].iStartLine  = line;
				pCodeSpan[0].iStartIndex = col;
				pCodeSpan[0].iEndLine	= line;
				pCodeSpan[0].iEndIndex   = col;

				if (buffer != null)
				{
					int length;

					buffer.GetLengthOfLine(line, out length);

					pCodeSpan[0].iStartIndex = 0;
					pCodeSpan[0].iEndIndex   = length;
				}

				return VSConstants.S_OK;
			}
			else
			{
				return VSConstants.S_FALSE;
			}
		}

		#endregion

		public override ViewFilter CreateViewFilter(CodeWindowManager mgr, IVsTextView newView)
		{
			// This call makes sure debugging events can be received by our view filter.
			//
			GetIVsDebugger();
			return new NemerleViewFilter(mgr, newView);
		}

		#endregion

		#region OnIdle

		public override void OnIdle(bool periodic)
		{
			if (LastActiveTextView == null)
				return;

			Source src = GetSource(LastActiveTextView);

			if (src != null && src.LastParseTime == int.MaxValue)
				src.LastParseTime = 0;

			base.OnIdle(periodic);
		}

		#endregion

		#region Filter List

		public override string GetFormatFilterList()
		{
			return Resources.NemerleFormatFilter;
		}

		#endregion

		#region ShowLocation event handler

		public void GotoLocation(Location loc)
		{
			TextSpan span = new TextSpan();

			span.iStartLine  = loc.Line - 1;
			span.iStartIndex = loc.Column - 1;
			span.iEndLine	= loc.EndLine - 1;
			span.iEndIndex   = loc.EndColumn - 1;

			uint		   itemID;
			IVsUIHierarchy hierarchy;
			IVsWindowFrame docFrame;
			IVsTextView	textView;

			VsShell.OpenDocument(Site, loc.File, VSConstants.LOGVIEWID_Code, 
				out hierarchy, out itemID, out docFrame, out textView);

			ErrorHandler.ThrowOnFailure(docFrame.Show());

			if (textView != null)
			{
				try
				{
					ErrorHandler.ThrowOnFailure(textView.SetCaretPos(span.iStartLine, span.iStartIndex));
					TextSpanHelper.MakePositive(ref span);
					ErrorHandler.ThrowOnFailure(textView.SetSelection(span.iStartLine, span.iStartIndex, span.iEndLine, span.iEndIndex));
					ErrorHandler.ThrowOnFailure(textView.EnsureSpanVisible(span));
				}
				catch (Exception ex)
				{
					Trace.WriteLine(ex.Message);
				}
			}
		}

		#endregion
	}
}
