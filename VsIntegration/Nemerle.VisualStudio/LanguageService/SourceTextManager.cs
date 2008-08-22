using System;
using System.Diagnostics;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Builtins;

using Nemerle.Completion2;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.LanguageService
{
	class SourceTextManager : ISource
	{
		public int FileIndex { get { return _source.FileIndex; } }

		public SourceTextManager(NemerleSource source)
		{
			_source  = source;
		}

		public  IVsTextLines  TextLines
		{
			get { return Source.GetTextLines(); }
		}

		private NemerleSource _source;
		public  NemerleSource  Source
		{
			get { return _source; }
		}

		public string GetText()
		{
			return Source.GetText();
		}

		public string GetRegion(int lineStart, int colStart, int lineEnd, int colEnd)
		{
			return Source.GetText(lineStart - 1, colStart - 1, lineEnd - 1, colEnd - 1);
		}

		public string GetRegion(Location loc)
		{
			return GetRegion(loc.Line, loc.Column, loc.EndLine, loc.EndColumn);
		}

		/// <summary>
		/// Get text of line frome text bufer of IDE.
		/// </summary>
		/// <param name="line">Line position (first line is 1).</param>
		/// <returns>The text of line.</returns>
		public string GetLine(int line)
		{
			line--; // Convert to zero based index.

#if DEBUG
			//int lineCount = LineCount;

			//if (line >= lineCount) // just for debugging purpose.
			//	Debug.Assert(line < lineCount);
#endif

			return Source.GetLine(line);
		}

		public int GetPositionOfLineIndex(int line, int col)
		{
			return Source.GetPositionOfLineIndex(line - 1, col - 1);
		}

		public Tuple<int, int> GetLineIndexOfPosition(int pos)
		{
			int line, col;

			Source.GetLineIndexOfPosition(pos, out line, out col);

			return new Tuple<int,int>(line + 1, col + 1);
		}

		public int LineCount
		{
			get
			{
				int lineCount;
				int hr1 = Source.GetTextLines().GetLineCount(out lineCount);

				ErrorHandler.ThrowOnFailure(hr1);

				return lineCount;
			}
		}
	}
}
