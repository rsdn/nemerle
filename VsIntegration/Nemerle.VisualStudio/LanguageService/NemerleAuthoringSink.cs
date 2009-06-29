using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using System.Collections.Generic;

namespace Nemerle.VisualStudio.LanguageService
{
	///<summary>
	/// This class is used by the parser to gather information about the source being parsed.
	///</summary>
	public class NemerleAuthoringSink : AuthoringSink
	{
		public NemerleAuthoringSink(
			NemerleSource source,
			ParseReason   reason,
			int           line,
			int           col,
			int           maxErrors)
			: base(reason, line, col, maxErrors)
		{
			Source    = source;
			MaxErrors = maxErrors;
			HiddenRegionsList = new List<NewHiddenRegion>();
		}

		public int                   MaxErrors     { get;         set; }
		public NemerleSource         Source        { get; private set; }
		public List<NewHiddenRegion> HiddenRegionsList { get; private set; }

		public override void AddHiddenRegion(NewHiddenRegion r)
		{
			//if (!HiddenRegions)
			//	return;

			// Sort the regions by their start positions so that if they add more than 
			// MaxRegions then they get the outer top level ones first.
			int i = this.HiddenRegionsList.Count - 1;
			while (i >= 0)
			{
				NewHiddenRegion s = this.HiddenRegionsList[i];
				if (TextSpanHelper.StartsAfterStartOf(r.tsHiddenText, s.tsHiddenText))
					break;
				i--;
			}
			
			HiddenRegionsList.Insert(i + 1, r);
		}
	}
}
