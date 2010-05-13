/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Collections.Generic;
using System.Text;

using Microsoft.VisualStudio.TextManager.Interop;

using VSConstants = Microsoft.VisualStudio.VSConstants;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using System.IO;
using System.Text.RegularExpressions;
using System.Globalization;

namespace Nemerle.VisualStudio.LanguageService
{
	internal class CodeBlocksEnumerator : IVsEnumCodeBlocks
	{
		private int currentIndex;
		private List<TextSpanAndCookie> blocks;

		/// <summary>
		/// Builds a new enumerator from a text buffer.
		/// </summary>
		public CodeBlocksEnumerator(IVsTextLines buffer)
		{
			if (null == buffer)
			{
				throw new ArgumentNullException("buffer");
			}
			blocks = new List<TextSpanAndCookie>();
			SearchForCodeBlocks(buffer);
		}

		/// <summary>
		/// Private constructor used for the Clone functionality.
		/// </summary>
		private CodeBlocksEnumerator(CodeBlocksEnumerator original)
		{
			this.currentIndex = original.currentIndex;
			this.blocks = new List<TextSpanAndCookie>(original.blocks);
		}

		#region Parser

		private enum ParseState
		{
			WaitForBlockStart,
			WaitForBlockEnd,
		}

		private Regex _linePragmaRegex = new Regex(@"\s*\#line (\d*) \""(.*)\""", RegexOptions.Singleline | RegexOptions.Compiled);

		// »щет в исходном файле все блоки, обрамленные прагмой #line N / #line default
		private void SearchForCodeBlocks(IVsTextLines buffer)
		{
			ErrorHandler.ThrowOnFailure(buffer.LockBufferEx((uint)BufferLockFlags.BLF_READ));
			try
			{
				int totalLines;
				ErrorHandler.ThrowOnFailure(buffer.GetLineCount(out totalLines));

				var state = ParseState.WaitForBlockStart;
				var blockSpan = new TextSpanAndCookie();

				for (int line = 0; line < totalLines; ++line)
				{
					int lineLen;
					ErrorHandler.ThrowOnFailure(buffer.GetLengthOfLine(line, out lineLen));

					string lineText;
					ErrorHandler.ThrowOnFailure(buffer.GetLineText(line, 0, line, lineLen, out lineText));

					if (state == ParseState.WaitForBlockStart)
					{
						var match = _linePragmaRegex.Match(lineText);

						if (match.Success)
						{
							blockSpan.ulHTMLCookie = uint.Parse(match.Groups[1].Value, NumberStyles.Integer, CultureInfo.InvariantCulture);
							blockSpan.CodeSpan = new TextSpan();
							blockSpan.CodeSpan.iStartLine = line + 1;
							blockSpan.CodeSpan.iStartIndex = 0;

							state = ParseState.WaitForBlockEnd;
						}
					}
					else
					{
						if (lineText.Trim().StartsWith("#line default", StringComparison.InvariantCultureIgnoreCase))
						{
							blockSpan.CodeSpan.iEndLine = line - 1;
							buffer.GetLengthOfLine(blockSpan.CodeSpan.iEndLine, out blockSpan.CodeSpan.iEndIndex);

							blocks.Add(blockSpan);

							blockSpan = new TextSpanAndCookie();

							state = ParseState.WaitForBlockStart;
						}
					}
				}
			}
			finally
			{
				// Make sure that the buffer is always unlocked when we exit this function.
				buffer.UnlockBufferEx((uint)BufferLockFlags.BLF_READ);
			}
		}

		#endregion

		public int Clone(out IVsEnumCodeBlocks ppEnum)
		{
			ppEnum = new CodeBlocksEnumerator(this);
			return VSConstants.S_OK;
		}

		public int Next(uint celt, TextSpanAndCookie[] rgelt, out uint pceltFetched)
		{
			if (0 == celt)
			{
				pceltFetched = 0;
				return VSConstants.S_OK;
			}
			if ((null == rgelt) || (0 == rgelt.Length))
			{
				throw new ArgumentNullException("rgelt");
			}
			if (rgelt.Length < celt)
			{
				throw new System.ArgumentException("rgelt");
			}
			pceltFetched = 0;
			while ((currentIndex < blocks.Count) && (pceltFetched < celt))
			{
				rgelt[pceltFetched].ulHTMLCookie = blocks[currentIndex].ulHTMLCookie;
				rgelt[pceltFetched].CodeSpan = blocks[currentIndex].CodeSpan;
				++currentIndex;
				++pceltFetched;
			}
			return (pceltFetched == celt) ? VSConstants.S_OK : VSConstants.S_FALSE;
		}

		public int Reset()
		{
			currentIndex = 0;
			return VSConstants.S_OK;
		}

		public int Skip(uint celt)
		{
			currentIndex += (int)celt;
			if ((currentIndex > blocks.Count) || (currentIndex < 0))
			{
				currentIndex = blocks.Count;
			}
			return VSConstants.S_OK;
		}
	}
}
