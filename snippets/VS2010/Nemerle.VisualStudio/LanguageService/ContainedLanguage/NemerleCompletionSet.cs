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
using Nemerle.Completion2;

namespace Nemerle.VisualStudio.LanguageService
{
	internal sealed class NemerleCompletionSet : CompletionSet
	{
		public NemerleSource Source { get; private set; }
		internal TextViewWrapper view;

		internal NemerleCompletionSet(ImageList imageList, NemerleSource source)
			: base(imageList, source)
		{
			Source = source;
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

		public override int OnCommit(string textSoFar, int index, int selected, ushort commitChar, out string completeWord)
		{
			try
			{
				var decls = (NemerleDeclarations)Declarations;

				if (decls.Result.ImportCompletion)
				{
					var env = decls.Result.CompletionResult.Env;
					var elem = decls.Result.CompletionResult.CompletionList[index];

					var usingInfo = NemerleCompletionResult.CalcUsingDeclarationInfo(env, elem.Overloads[0]);

					if (!usingInfo.NeedUsing && !usingInfo.Hiden && string.IsNullOrEmpty(usingInfo.Alias))
						return base.OnCommit(textSoFar, index, selected, commitChar, out completeWord);

					if (!string.IsNullOrEmpty(usingInfo.Alias))
					{
						var result = base.OnCommit(textSoFar, index, selected, commitChar, out completeWord);
						completeWord = usingInfo.Alias + "." + completeWord;
						return result;
					}

					if (usingInfo.Hiden)
					{
						var result = base.OnCommit(textSoFar, index, selected, commitChar, out completeWord);
						completeWord = usingInfo.Namespase + "." + completeWord;
						return result;
					}

					if (usingInfo.NeedUsing)
					{
						var result = base.OnCommit(textSoFar, index, selected, commitChar, out completeWord);
						
						if (result == VSConstants.S_OK)
						{
							var cu = Source.CompileUnit;

							var line = cu != null
								? NemerleCompletionResult.CalcUsingDeclarationInsertionLine(usingInfo.Namespase, cu) - 1
								: 0;
							//if (Source.CompletedFirstParse && cu == null)
							Source.SetText(line, 0, line, 0, "using " + usingInfo.Namespase + ";" + Environment.NewLine);
						}

						return result;
					}
				}
			}
			catch (Exception)
			{
			}
			return base.OnCommit(textSoFar, index, selected, commitChar, out completeWord);
		}
	}
}

