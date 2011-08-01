using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Nemerle.VisualStudio.GUI.Wizards
{
	public abstract partial class MacroType
	{
		public bool           DefineSyntax { get; set; }
		public ParameterDef[] Parameters   { get; set; }

		#region Code generation helpers

		private string GenerateMacroParameterDefinition(ParameterDef parameterDef)
		{
			return (parameterDef.IsParameterArray ? "params " : "")
				+ parameterDef.Name + " : " + parameterDef.Type
				+ (string.IsNullOrWhiteSpace(parameterDef.DefaultValue) ? "" : " = " + parameterDef.DefaultValue);
		}

		private string GenerateMethodParameterDefinition(ParameterDef parameterDef)
		{
			return parameterDef.Name + " : " + parameterDef.Type
				+ (string.IsNullOrWhiteSpace(parameterDef.DefaultValue) ? "" : " = " + parameterDef.DefaultValue);
		}

		private string GenerateParametersReference(ParameterDef parameterDef)
		{
			return parameterDef.Name;
		}

		private string GenerateParametersFukeUse(ParameterDef parameterDef)
		{
			return "_ = " + parameterDef.Name;
		}

		#endregion

		public virtual void FillReplacementsDictionary(Dictionary<string, string> replacementsDictionary)
		{
			var userParametersReference = string.Join(", ", Parameters.Select(GenerateParametersReference));

			replacementsDictionary["$IsSyntaxDefined$"] = DefineSyntax ? "True" : "False";
			replacementsDictionary["$Syntax$"]          = DefineSyntax ? ("syntax (\"define_your_syntax_here\", " + userParametersReference + ")") : "";

			var parameters = GetParameterDefs();

			replacementsDictionary["$MacroParametersDefinition$"]  = string.Join(", ", parameters.Select(GenerateMacroParameterDefinition));
			replacementsDictionary["$MethodParametersDefinition$"] = string.Join(", ", parameters.Select(GenerateMethodParameterDefinition));
			replacementsDictionary["$ParametersReference$"]        = string.Join(", ", parameters.Select(GenerateParametersReference));
			replacementsDictionary["$UserParametersReference$"]    = userParametersReference;
			replacementsDictionary["$ParametersFukeUse$"]          = string.Join("; ", parameters.Select(GenerateParametersFukeUse));
		}

		protected virtual IEnumerable<ParameterDef> GetParameterDefs()
		{
			return Parameters;
		}
	}
}
