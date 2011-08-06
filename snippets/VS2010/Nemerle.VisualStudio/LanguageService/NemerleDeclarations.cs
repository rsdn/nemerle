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
		CompletionElem[] OverloadPossibility              { get; set; }
		Location         ComlitionLocation                { get; set; }

		public CompletionAsyncRequest Result              { get; private set; }
		public NemerleSource          Source              { get; private set; }
		public bool                   IsMemeberComplation { get; private set; }


		public NemerleDeclarations(CompletionAsyncRequest result, NemerleSource source, bool isMemeberComplation)
		{
			OverloadPossibility = result.CompletionResult.CompletionList;
			Result              = result;
			Source              = source;
			ComlitionLocation   = result.ComlitionLocation;
			IsMemeberComplation = isMemeberComplation;
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
			if (Result.ImportCompletion && OverloadPossibility[index].Overloads != null && OverloadPossibility[index].Overloads.Count > 0)
				return NemerleCompletionResult.MekeDisplayText(OverloadPossibility[index].Overloads[0]);

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
		public override string OnCommit(IVsTextView textView, string textSoFar, char commitCharacter, int index, ref TextSpan initialExtent)
		{
			// We intercept this call only to get the initial extent
			// of what was committed to the source buffer.

			//commitSpan = initialExtent;
			return base.OnCommit(textView, textSoFar, commitCharacter, index, 
				ref initialExtent);
		}

		/// This method is called after the string has been committed to the source buffer.
		public override char OnAutoComplete(IVsTextView textView, string committedText, char commitCharacter, int index)
		{
			return base.OnAutoComplete(textView, committedText, commitCharacter, index);
		}
	}
}
