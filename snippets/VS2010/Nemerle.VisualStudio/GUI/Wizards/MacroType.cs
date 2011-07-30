using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Nemerle.VisualStudio.GUI.Wizards
{
	public abstract class MacroType
	{
		public class Attribute : MacroType
		{
			public MacroPhase       MacroPhase { get; set; }
			public AttributeTargets ValidOn    { get; set; }

			public Attribute()
			{
				ValidOn = AttributeTargets.Assembly;
				MacroPhase = Nemerle.MacroPhase.BeforeTypedMembers;
			}

			public override void FillReplacementsDictionary(Dictionary<string, string> replacementsDictionary)
			{
				replacementsDictionary["$MacroPhase$"] = "MacroPhase." + MacroPhase;
				replacementsDictionary["$ValidOn$"]    = "MacroTargets." + this.ValidOn;

				base.FillReplacementsDictionary(replacementsDictionary);
			}

			protected override IEnumerable<string> GenerateParametersCode()
			{
				return base.GenerateParametersCode().Concat();
			}

			public override string ToString() { return "Macro Attribute"; }
		}

		public class Expression : MacroType
		{
			public Expression()
			{
			}

			public override void FillReplacementsDictionary(Dictionary<string, string> replacementsDictionary)
			{
				replacementsDictionary["$syntax$"] = DefineSyntax ? "\r\n    syntax (\"define your syntax here\")" : "";
				base.FillReplacementsDictionary(replacementsDictionary);
			}

			public override string ToString() { return "Expression level"; }
		}

		public bool DefineSyntax { get; set; }
		public string[][] Parameters { get; set; }

		public virtual void FillReplacementsDictionary(Dictionary<string, string> replacementsDictionary)
		{
			replacementsDictionary["$syntax$"] = DefineSyntax ? "\r\n    syntax (\"define your syntax here\")" : "";
			replacementsDictionary["$syntax_defined$"] = DefineSyntax.ToString();

			var parameters = GenerateParametersCode();
			replacementsDictionary["$parameters$"] = string.Join(", ", parameters);
		}

		protected virtual IEnumerable<string> GenerateParametersCode()
		{
			return Parameters.Select(p => GenerateParameterCode(p[0], p[1], p[2]));
		}

		private string GenerateParameterCode(string name, string type, string defaulteValue)
		{
			return name + " : " + type + (string.IsNullOrWhiteSpace(defaulteValue) ? "" : " = " + defaulteValue);
		}
	}
}
