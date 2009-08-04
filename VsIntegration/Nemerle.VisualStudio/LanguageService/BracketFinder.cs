using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TextManager.Interop;
using Nemerle.Completion2;
using Nemerle.Compiler;
using Microsoft.VisualStudio;

namespace Nemerle.VisualStudio.LanguageService
{
	/// <summary>
	/// Helper class which match paired token (brackets, brace, etc.)
	/// </summary>
	class BracketFinder
	{
		#region Fields

		public readonly ScanTokenInfo StartBraceInfo;
		private readonly IVsTextColorState ColorState;
		private readonly NemerleSource Source;
		private readonly int StartLine;
		private readonly ScanLexer Lex;
		private readonly NemerleScanner Scanner;

		#endregion

		public BracketFinder(NemerleSource source, int startLine,
			int startCol, NemerleScanner scanner, IVsTextColorState colorState)
		{
			#region Init fields

			Scanner = scanner;
			Source = source;
			StartLine = startLine;
			Lex = scanner.GetNewLexer();
			Lex.SetFileName(source.GetFilePath());
			ColorState = colorState;
			_lineCount = source.GetLineCount();
			var line = startLine - 1;
			_buffer = new string[1] { source.GetText(line, 0, line, source.GetLineLength(line)) };
			_startBufferLine = line;

			#endregion

			#region 2. Determine that it is a paired token. 3. Determine paired token.

			// Get tokens of line under text carret into dynamic array.
			List<ScanTokenInfo> lineToks = GetLineTokens(startLine, true);
			// Find index of token which located under text carret.
			int index = FindIndex(lineToks, x => x.Token.Location.Contains(startLine, startCol));

			if (index < 0)
				return;

			// If index is corret get corresponding token.
			ScanTokenInfo startBraceInfo = lineToks[index];
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
				for (int i = startLine + 1, count = _lineCount; i <= count; i++)
					foreach (ScanTokenInfo tokInf in GetLineTokens(i, true))
						yield return tokInf;
			else // Scan previous lines.
				for (int i = startLine - 1; i > 0; i--)
					foreach (ScanTokenInfo tokInf in GetLineTokens(i, false))
						yield return tokInf;
		}

		#endregion

		#region GetLineTokens()

		const int BufferLen = 100;
		string[] _buffer;
		int _startBufferLine;
		int _lineCount;

		/// <summary>
		/// Get tokens of specified line.
		/// </summary>
		/// <param name="line">Line number which tokens it is necessary retrieve</param>
		/// <param name="isForward">Direction of iteration (true is forward)</param>
		/// <returns>Token list</returns>
		List<ScanTokenInfo> GetLineTokens(int nline, bool isForward)
		{
			int line = nline - 1;
			ScanState scanState = GetLineState(nline);
			string lineText;

			int index = line - _startBufferLine;

			// читаем буферами по BufferLen стрк, чтобы ускорить работу
			if (index < 0 || index >= _buffer.Length)
			{
				int start, end;

				if (isForward)
				{
					end = Math.Min(_lineCount - 1, line + BufferLen);
					start = line;
				}
				else
				{
					start = Math.Max(0, line - BufferLen);
					end = line;
				}

				var str = Source.GetText(start, 0, end, Source.GetLineLength(end));
				_buffer = str.Split(new string[] { "\r\n", "\n", "\r" }, StringSplitOptions.None);
				_startBufferLine = start;
				index = line - _startBufferLine;
			}
				
			lineText = _buffer[index];

			Scanner.SetSource(lineText, 0);
			Lex.SetLine(nline, lineText, 0, null, null);
			List<ScanTokenInfo> lst = new List<ScanTokenInfo>();

			foreach (ScanTokenInfo var in GetLineTokens(Lex, scanState))
				lst.Add(var);

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
			if (token is Token.BeginBrace) return true;
			if (token is Token.BeginQuote) return true;
			if (token is Token.BeginRound) return true;
			if (token is Token.BeginSquare) return true;

			if (token is Token.EndBrace) return false;
			if (token is Token.EndQuote) return false;
			if (token is Token.EndRound) return false;
			if (token is Token.EndSquare) return false;

			Token.Keyword kwd = token as Token.Keyword;

			if (kwd != null)
			{
				if (kwd.name == "if") return true;
				if (kwd.name == "else") return false;
			}

			throw new ArgumentException(string.Format("The token '{0}' not a brace!", token), "token");
		}

		private static bool IsPairedToken(Token token)
		{
			if (token is Token.BeginBrace || token is Token.BeginQuote
				|| token is Token.BeginRound || token is Token.BeginSquare
				|| token is Token.EndBrace || token is Token.EndQuote
				|| token is Token.EndRound || token is Token.EndSquare
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
			if (token is Token.BeginBrace) return t => t is Token.BeginBrace;
			if (token is Token.BeginQuote) return t => t is Token.BeginQuote;
			if (token is Token.BeginRound) return t => t is Token.BeginRound;
			if (token is Token.BeginSquare) return t => t is Token.BeginSquare;

			if (token is Token.EndBrace) return t => t is Token.EndBrace;
			if (token is Token.EndQuote) return t => t is Token.EndQuote;
			if (token is Token.EndRound) return t => t is Token.EndRound;
			if (token is Token.EndSquare) return t => t is Token.EndSquare;

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
				if (kwd.name == "if") return new Token.Keyword("else");
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
}
