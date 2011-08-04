using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Completion2;
using Nemerle.Compiler;
using System.Diagnostics;
using Nemerle.Compiler.Utils.Async;

namespace Nemerle.VisualStudio.LanguageService
{
	public class NemerleDeclarations : Declarations
	{
		CompletionElem[] OverloadPossibility { get; set; }
		Location ComlitionLocation { get; set; }

		public CompletionAsyncRequest Result { get; private set; }
		public NemerleSource Source { get; private set; }


		public NemerleDeclarations(CompletionAsyncRequest result, NemerleSource source)
		{
			OverloadPossibility = result.CompletionResult.CompletionList;
			Result              = result;
			Source              = source;
			ComlitionLocation   = result.ComlitionLocation;
			Sort();
		}

		public override bool GetInitialExtent(IVsTextView textView, out int line, out int startIdx, out int endIdx)
		{
			if (ComlitionLocation != Location.Default)
			{
				Debug.Assert(ComlitionLocation.Line == ComlitionLocation.EndLine);

				line     = ComlitionLocation.Line      - 1;
				startIdx = ComlitionLocation.Column    - 1;
				endIdx   = ComlitionLocation.EndColumn - 1;

				return true;
			}

			return base.GetInitialExtent(textView, out line, out startIdx, out endIdx);
		}

		public override int GetCount()
		{
			return OverloadPossibility.Length;
		}

		public override string GetDisplayText(int index)
		{
			return OverloadPossibility[index].DisplayName;
		}

		public override string GetDescription(int index)
		{
			return OverloadPossibility[index].Description;
		}

		public override string GetName(int index)
		{
			if (index < 0 )
				return "";
			return OverloadPossibility[index].DisplayName;
		}

		public override int GetGlyph(int index)
		{
			return OverloadPossibility[index].GlyphType;
		}

		class ByNameComparer : IComparer<CompletionElem>
		{
			public static readonly ByNameComparer Instance = new ByNameComparer();

			public int Compare(CompletionElem x, CompletionElem y)
			{
				return x.DisplayName.CompareTo(y.DisplayName);
			}
		}

		public void Sort()
		{
			Array.Sort(OverloadPossibility, ByNameComparer.Instance);
		}

		// This method is called to get the string to commit to the source buffer.
		// Note that the initial extent is only what the user has typed so far.
		// Disable the "ParameterNamesShouldMatchBaseDeclaration" warning.
		//
		public override string OnCommit(
			IVsTextView textView, string textSoFar, char commitCharacter, int index, ref TextSpan initialExtent)
		{
			// We intercept this call only to get the initial extent
			// of what was committed to the source buffer.

			//commitSpan = initialExtent;
			return base.OnCommit(textView, textSoFar, commitCharacter, index, 
				ref initialExtent);
		}

		// This method is called after the string has been committed to the source buffer.
		//
		public override char OnAutoComplete(IVsTextView textView, 
			string committedText, char commitCharacter, int index)
		{
			//const char defaultReturnValue = '\0';
			//Declaration item = declarations[index] as Declaration;
			//if (item == null)
			//  return defaultReturnValue;

			//// In this example, NemerleDeclaration identifies types with an enum.
			//// You can choose a different approach.
			//if (item.Type != Declaration.DeclarationType.Snippet)
			//  return defaultReturnValue;
			
			//Source src = languageService.GetSource(textView);
			//if (src == null)
			//  return defaultReturnValue;
			
			//ExpansionProvider ep = src.GetExpansionProvider();
			//if (ep == null)
			//  return defaultReturnValue;
			
			//string title;
			//string path;
			//int commitLength = commitSpan.iEndIndex - commitSpan.iStartIndex;
			//if (commitLength < committedText.Length)
			//{
			//  // Replace everything that was inserted so calculate the span of the
			//  // full
			//  // insertion, taking into account what was inserted when the 
			//  // commitSpan was obtained in the first place.
			//  commitSpan.iEndIndex += (committedText.Length - commitLength);
			//}

			//if (ep.FindExpansionByShortcut(textView, committedText, commitSpan,
			//							   true, out title, out path) >= 0)
			//{
			//  ep.InsertNamedExpansion(textView, title, path, commitSpan, false);
			//}
			//return defaultReturnValue;
			//throw new NotImplementedException();

			return base.OnAutoComplete(textView, committedText, 
				commitCharacter, index);
		}
	}
}
