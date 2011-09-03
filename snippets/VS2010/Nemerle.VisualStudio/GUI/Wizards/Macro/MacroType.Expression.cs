using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Nemerle.VisualStudio.GUI.Wizards
{
	public abstract partial class MacroType
	{
		public class Expression : MacroType
		{
			public Expression()
			{
			}

			public override void FillReplacementsDictionary(Dictionary<string, string> replacementsDictionary)
			{
				replacementsDictionary["$IsMacroAttribute$"] = "False";
				replacementsDictionary["$MacroReturnType$"]  = "PExpr";

				base.FillReplacementsDictionary(replacementsDictionary);
			}

			public override string ToString() { return "Expression level"; }
		}
	}
}
