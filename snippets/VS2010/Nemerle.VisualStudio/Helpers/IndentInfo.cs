using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Package;

namespace Nemerle.VisualStudio
{
	internal class IndentInfo
	{
		public IndentInfo(bool insertTabs, int indentSize, int tabSize)
		{
			InsertTabs = insertTabs;
			IndentSize = indentSize;
			TabSize    = tabSize;
		}

		public IndentInfo(LanguagePreferences languagePreferences)
		{
			InsertTabs = languagePreferences.InsertTabs;
			IndentSize = languagePreferences.IndentSize;
			TabSize    = languagePreferences.TabSize;
		}
	
		public bool InsertTabs { get; private set; }
		public int  IndentSize { get; private set; }
		public int  TabSize    { get; private set; }
	}
}
