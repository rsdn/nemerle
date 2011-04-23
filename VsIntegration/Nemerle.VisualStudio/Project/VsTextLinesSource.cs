using System;
using System.Collections.Generic;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Compiler;

namespace Nemerle.VisualStudio.Project
{
	internal sealed class VsTextLinesSource : ISource
	{
		public VsTextLinesSource(int fileIndex, IVsTextLines textLines)
		{
			ErrorHelper.ThrowIsNull(textLines, "textLines");
			_fileIndex = fileIndex;
			_textLines = textLines;
		}

		private readonly int _fileIndex;

		public int FileIndex
		{
			get { return _fileIndex; }
		}

		private readonly IVsTextLines _textLines;

		public string GetText()
		{
			int totalLines;
			ErrorHandler.ThrowOnFailure(_textLines.GetLineCount(out totalLines));

			if (totalLines > 0)
			{
				int lastLineLength;
				ErrorHandler.ThrowOnFailure(_textLines.GetLengthOfLine(totalLines - 1, out lastLineLength));

				string text;
				ErrorHandler.ThrowOnFailure(_textLines.GetLineText(0, 0, totalLines - 1, lastLineLength, out text));

				return text;
			}
			return "";
		}
	}
}
