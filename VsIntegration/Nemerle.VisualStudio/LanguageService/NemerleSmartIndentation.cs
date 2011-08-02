using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using Microsoft.VisualStudio.TextManager.Interop;

namespace Nemerle.VisualStudio.LanguageService
{
	// Smart Indentation implementation for Nemerle
	//   '{', '|', ':' increases indentation (see _indentTokens)
	//   '}' decreases indentation
	class NemerleSmartIndentation
	{
		private NemerleSource _source;
		private bool          _useTabs = false;
		private int           _tabSize = 2;
		private static readonly char[] _indentTokens = new char[] { '{', ':', '|' };

		public NemerleSmartIndentation(NemerleSource source)
		{
			_source = source;
			_useTabs = _source.LanguageService.Preferences.InsertTabs;
			_tabSize = _source.LanguageService.Preferences.TabSize;
		}

		public bool At(int line)
		{
			try
			{
				return IndentLine(line + 1); // indenter use 1-based indexes
			}
			catch (Exception ex)
			{
				Trace.WriteLine("Error while processing smart indentation\nException : " + ex.Message);
				return false;
			}
		}

		private bool IndentLine(int line)
		{
			var currentLine = _source.GetLine(line);

			var prevLine = GetPreviousNonEmptyLine(line);

			if (string.IsNullOrEmpty(prevLine))
				return false;

			var indentString = GetIndentString(prevLine);

			if (!string.IsNullOrEmpty(currentLine) && currentLine.Trim().StartsWith("}", StringComparison.InvariantCulture))
			{
				if (!prevLine.EndsWith("{", StringComparison.InvariantCultureIgnoreCase))
					indentString = Unindent(indentString);
			}
			else if (_indentTokens.Contains(prevLine[prevLine.Length - 1]))
			{
				indentString = Indent(indentString);
			}

			// replace existent indentation
			var lineWithoutIndentation = currentLine.TrimStart(' ', '\t');

			var span = new TextSpan();
			span.iStartIndex = 0;
			span.iEndIndex = currentLine.Length - lineWithoutIndentation.Length;
			span.iStartLine = span.iEndLine = line - 1;

			_source.SetText(span, indentString);

			return true;
		}

		private string Indent(string text)
		{
			return text + (_useTabs ? "\t" : new string(' ', _tabSize));
		}

		private string Unindent(string text)
		{
			if (string.IsNullOrEmpty(text))
				return string.Empty;

			if (text.EndsWith("\t", StringComparison.InvariantCultureIgnoreCase))
				return text.Substring(0, text.Length - 1);
			else
				return text.Substring(0, text.Length - Math.Min(text.Length, _tabSize));
		}

		private string GetIndentString(string prevLineText)
		{
			if (string.IsNullOrEmpty(prevLineText))
				return string.Empty;

			var nonTabPosition = 0;

			while (nonTabPosition <= prevLineText.Length - 1)
			{
				if (prevLineText[nonTabPosition] == ' ' || prevLineText[nonTabPosition] == '\t')
					nonTabPosition++;
				else
					break;
			}

			return prevLineText.Substring(0, nonTabPosition);
		}

		private string GetPreviousNonEmptyLine(int line)
		{
			var prevLine = line - 1;
			while (prevLine >= 0)
			{
				var lineString = TrimLineComment(_source.GetLine(prevLine));

				if (!string.IsNullOrEmpty(lineString.Trim()))
					return lineString;

				prevLine--;
			}

			return null;
		}

		private string TrimLineComment(string line)
		{
			if (string.IsNullOrEmpty(line))
				return string.Empty;

			var commentPosition = line.IndexOf("//", StringComparison.InvariantCultureIgnoreCase);

			if (commentPosition >= 0)
				line = line.Substring(0, commentPosition);

			return line;
		}
	}
}
