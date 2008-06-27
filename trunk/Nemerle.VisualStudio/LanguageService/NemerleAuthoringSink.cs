using Microsoft.VisualStudio.Package;

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
			_source	= source;
			_maxErrors = maxErrors;
		}

		private int _maxErrors;
		public  int  MaxErrors
		{
			get { return _maxErrors;  }
			set { _maxErrors = value; }
		}

		private NemerleSource _source;
		public  NemerleSource  Source
		{
			get { return _source; }
		}
	}
}
