using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;

namespace Nemerle.VisualStudio.GUI.Wizards
{
	public abstract partial class MacroType
	{
		public class Attribute : MacroType
		{
			public MacroPhase MacroPhase { get; set; }
			public AttributeTargets ValidOn { get; set; }

			#region Init the MacroTarget/MacroPhase map

			const int MacroTargetsCount = 7;
			const int MacroPhasesCount = 3;

			static string[,] _parametersMapDef = new string[MacroTargetsCount, MacroPhasesCount + 1]
				{
					// MacroTarget                                                  MacroPhase
					//             BeforeInheritance                           BeforeTypedMembers                          WithTypedMembers
					{ "Type",      "TypeBuilder",                              "TypeBuilder",                              "TypeBuilder" },
					{ "Method",    "TypeBuilder;ParsedMethod",                 "TypeBuilder;ParsedMethod",                 "TypeBuilder;MethodBuilder" },
					{ "Field",     "TypeBuilder;ParsedField",                  "TypeBuilder;ParsedField",                  "TypeBuilder;FieldBuilder" },
					{ "Property",  "TypeBuilder;ParsedProperty",               "TypeBuilder;ParsedProperty",               "TypeBuilder;PropertyBuilder" },
					{ "Event",     "TypeBuilder;ParsedEvent",                  "TypeBuilder;ParsedEvent",                  "TypeBuilder;EventBuilder" },
					{ "Parameter", "TypeBuilder;ParsedMethod;ParsedParameter", "TypeBuilder;ParsedMethod;ParsedParameter", "TypeBuilder;MethodBuilder;ParameterBuilder" },
					{ "Assembly",  "",                                         "",                                         "" },
				};

			/// string = MacroTarget 
			static Dictionary<string, ParameterDef[][]> _parametersMap = new Dictionary<string, ParameterDef[][]>();

			static Dictionary<string, string> _typesMap = new Dictionary<string, string>()
				{
					{ "TypeBuilder",      "TypeBuilder" },
					{ "MethodBuilder",    "MethodBuilder" },
					{ "FieldBuilder",     "FieldBuilder" },
					{ "PropertyBuilder",  "PropertyBuilder" },
					{ "EventBuilder",     "EventBuilder" },
					{ "ParsedField",      "ClassMember.Field" },
					{ "ParsedMethod",     "ClassMember.Function" },
					{ "ParsedProperty",   "ClassMember.Property" },
					{ "ParsedEvent",      "ClassMember.Event" },
					{ "ParsedParameter",  "PParameter" },
					{ "ParameterBuilder", "TParameter" },
				};

			static Dictionary<string, string> _namesMap = new Dictionary<string, string>()
				{
					{ "TypeBuilder",      "typeBuilder" },
					{ "MethodBuilder",    "method" },
					{ "FieldBuilder",     "field" },
					{ "PropertyBuilder",  "property" },
					{ "EventBuilder",     "evnt" },
					{ "ParsedField",      "field" },
					{ "ParsedMethod",     "method" },
					{ "ParsedProperty",   "property" },
					{ "ParsedEvent",      "evnt" },
					{ "ParsedParameter",  "parameter" },
					{ "ParameterBuilder", "parameter" },
				};

			static Attribute()
			{
				for (int i = 0; i < MacroTargetsCount; i++)
				{
					_parametersMap.Add(
						_parametersMapDef[i, 0], 
						new ParameterDef[][] 
						{ 
							ParseParamsDefs(_parametersMapDef[i, 1]),
							ParseParamsDefs(_parametersMapDef[i, 2]),
							ParseParamsDefs(_parametersMapDef[i, 3])
						});
				}
			}

			static ParameterDef[] ParameterDefs(MacroPhase phase, AttributeTargets validOn)
			{
				var parameterDefs = _parametersMap[Utils.ValidOnToString(validOn)][(int)phase - 1];
				return parameterDefs;
			}

			private static ParameterDef[] ParseParamsDefs(string parameterDefStr)
			{
				if (string.IsNullOrWhiteSpace(parameterDefStr))
					return new ParameterDef[0];

				var parametersTyps = parameterDefStr.Split(';');
				Debug.Assert(parametersTyps.Length >= 1 && parametersTyps.Length <= 3);
				try
				{
					return parametersTyps.Select(t => new ParameterDef(_namesMap[t], _typesMap[t], t, "")).ToArray();
				}
				catch (Exception ex)
				{
					Debug.Assert(false, ex.Message);
					throw;
				}
			}

			#endregion

			public Attribute()
			{
				ValidOn = AttributeTargets.Assembly;
				MacroPhase = Nemerle.MacroPhase.BeforeTypedMembers;
			}

			public override void FillReplacementsDictionary(Dictionary<string, string> replacementsDictionary)
			{
				base.FillReplacementsDictionary(replacementsDictionary);

				replacementsDictionary["$IsMacroAttribute$"] = "True";
				replacementsDictionary["$MacroPhase$"]       = "MacroPhase." + MacroPhase;
				replacementsDictionary["$ValidOn$"]          = "MacroTargets." + this.ValidOn;
				replacementsDictionary["$MacroReturnType$"]  = "void";
			}

			protected override IEnumerable<ParameterDef> GetParameterDefs()
			{
				var stdParameters = ParameterDefs(MacroPhase, ValidOn);
				return stdParameters.Concat(base.GetParameterDefs());
			}

			public override string ToString() { return "Macro Attribute"; }
		}
	}
}
