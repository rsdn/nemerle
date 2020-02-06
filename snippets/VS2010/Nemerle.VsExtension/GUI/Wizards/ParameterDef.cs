using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Nemerle.VisualStudio.GUI.Wizards
{
	public class ParameterDef
	{
		public ParameterDef(string name, string type, string typeAlias, string defaultValue)
		{
			const string ParamsPrefix = "params ";

			if (type.StartsWith(ParamsPrefix, StringComparison.InvariantCulture))
			{
				Type = type.Substring(ParamsPrefix.Length);
				IsParameterArray = true;
			}
			else
				Type = type;

			Name = name;
			TypeAlias = typeAlias;
			DefaultValue = defaultValue;
		}

		public string Name             { get; private set; }
		public string Type             { get; private set; }
		public string TypeAlias        { get; private set; }
		public string DefaultValue     { get; private set; }
		public bool   IsParameterArray { get; private set; }
	}
}
