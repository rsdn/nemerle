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
			Name = name;
			Type = type;
			TypeAlias = typeAlias;
			DefaultValue = defaultValue;
		}

		public string Name         { get; private set; }
		public string Type         { get; private set; }
		public string TypeAlias    { get; private set; }
		public string DefaultValue { get; private set; }
	}
}
