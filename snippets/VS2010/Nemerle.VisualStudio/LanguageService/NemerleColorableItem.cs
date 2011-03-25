using System.Drawing;

//using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;

namespace Nemerle.VisualStudio.LanguageService
{
	class NemerleColorableItem : ColorableItem
	{
		static readonly Color[] QuotationBackColor = new Color[] {
			Color.FromArgb(230, 237, 228), // Quotation
			Color.FromArgb(240, 230, 220), // Quotation Verbatim String
			Color.FromArgb(250, 232, 232)  // Verbatim String
		};

		public NemerleColorableItem(string displayName)
			: this(displayName, COLORINDEX.CI_SYSPLAINTEXT_FG, COLORINDEX.CI_USERTEXT_BK, Color.Empty, Color.Empty)
		{
		}

		public NemerleColorableItem(string displayName, COLORINDEX foreColor)
			: this(displayName, foreColor, COLORINDEX.CI_USERTEXT_BK, Color.Empty, Color.Empty)
		{
		}

		public NemerleColorableItem(string displayName, COLORINDEX foreColor, Color hiForeColor)
			: this(displayName, foreColor, COLORINDEX.CI_USERTEXT_BK, hiForeColor, Color.Empty)
		{
		}

		public NemerleColorableItem(string displayName, COLORINDEX foreColor, Color hiForeColor, FONTFLAGS fontFlags)
			: this(displayName, foreColor, COLORINDEX.CI_USERTEXT_BK, hiForeColor, Color.Empty, fontFlags)
		{
		}

		public NemerleColorableItem(string displayName, int n)
			: this(displayName, COLORINDEX.CI_SYSPLAINTEXT_FG, COLORINDEX.CI_SYSWIDGETMGN_BK, Color.Empty, QuotationBackColor[n])
		{
		}

		public NemerleColorableItem(string displayName, int n, COLORINDEX foreColor)
			: this(displayName, foreColor, COLORINDEX.CI_SYSWIDGETMGN_BK, Color.Empty, QuotationBackColor[n])
		{
		}

		public NemerleColorableItem(string displayName, int n, COLORINDEX foreColor, Color hiForeColor)
			: this(displayName, foreColor, COLORINDEX.CI_SYSWIDGETMGN_BK, hiForeColor, QuotationBackColor[n])
		{
		}

		public NemerleColorableItem(string displayName, int n, COLORINDEX foreColor, Color hiForeColor, FONTFLAGS fontFlags)
			: this(displayName, foreColor, COLORINDEX.CI_SYSWIDGETMGN_BK, hiForeColor, QuotationBackColor[n], fontFlags)
		{
		}

		public NemerleColorableItem(
			string displayName, COLORINDEX foreColor, COLORINDEX backColor, Color hiForeColor, Color hiBackColor)
			: this(
				displayName,
				foreColor,
				backColor,
				hiForeColor,
				hiBackColor,
				FONTFLAGS.FF_DEFAULT)
		{
		}
		public NemerleColorableItem(
			string displayName, COLORINDEX foreColor, COLORINDEX backColor, Color hiForeColor, Color hiBackColor, FONTFLAGS fontFlags)
			: base(
			    displayName,
				displayName,
				foreColor,   backColor,
				hiForeColor, hiBackColor,
				fontFlags)
		{
		}
	}
}
