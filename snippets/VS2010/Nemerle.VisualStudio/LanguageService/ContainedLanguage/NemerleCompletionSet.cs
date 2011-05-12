/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Windows.Forms;

using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using Microsoft.VisualStudio;

namespace Nemerle.VisualStudio.LanguageService
{
	internal sealed class NemerleCompletionSet : CompletionSet
	{
		internal TextViewWrapper view;

		internal NemerleCompletionSet(ImageList imageList, Source source)
			: base(imageList, source)
		{
		}

		public override void Init(IVsTextView textView, Declarations declarations, bool completeWord)
		{
			view = textView as TextViewWrapper;
			base.Init(textView, declarations, completeWord);
		}

		public override int GetInitialExtent(out int line, out int startIdx, out int endIdx)
		{
			int returnCode = base.GetInitialExtent(out line, out startIdx, out endIdx);

			if (ErrorHandler.Failed(returnCode) || (view == null))
			{
				return returnCode;
			}

			TextSpan secondary = new TextSpan();
			secondary.iStartLine = secondary.iEndLine = line;
			secondary.iStartIndex = startIdx;
			secondary.iEndIndex = endIdx;

			TextSpan primary = view.GetPrimarySpan(secondary);
			line = primary.iStartLine;
			startIdx = primary.iStartIndex;
			endIdx = primary.iEndIndex;

			return returnCode;
		}
	}
}

