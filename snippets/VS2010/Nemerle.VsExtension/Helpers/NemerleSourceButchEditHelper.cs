using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Package;
using Nemerle.VisualStudio.LanguageService;
using Microsoft.VisualStudio.TextManager.Interop;
using Nemerle.Compiler;
using System.Diagnostics;

namespace Nemerle.VisualStudio.Helpers
{
	class NemerleSourceButchEditHelper : EditArray
	{
		readonly int _fileIndex;

		public NemerleSourceButchEditHelper(
			NemerleSource source,
			IVsTextView   view,
			bool          merge,
			string        description) : base(source, view, merge, description)
		{
			_fileIndex = source.FileIndex;
		}

		public new NemerleSource Source
		{
			get { return (NemerleSource)base.Source; }
		}

		public void Add(Location loc, string text)
		{
			ErrorHelper.ThrowIfFalse(loc.FileIndex == _fileIndex, "loc.FileIndex");

			base.Add(new EditSpan(loc.ToTextSpan(), text ?? ""));
		}

		public void Add(TextPoint point, string text)
		{
			base.Add(new EditSpan(point.ToTextSpan(), text ?? ""));
		}

		public void Add(TextPoint startPoint, TextPoint endPoint, string text)
		{
			base.Add(new EditSpan(Utils.ToTextSpan(startPoint, endPoint), text ?? ""));
		}
	}
}
