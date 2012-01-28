using System;
using System.Collections.Generic;
using System.Linq;

namespace Tests
{
	class P
	{
		[SqlFunction("PI", ServerSideOnly = true)]
		static void Main()
		{
		}
	}

	[SerializableAttribute]
	[AttributeUsageAttribute(AttributeTargets.Method | AttributeTargets.Property, AllowMultiple = true, Inherited = false)]
	public class SqlFunctionAttribute : Attribute
	{
		public SqlFunctionAttribute(string name)
		{
			Name = name;
		}
		public string Name { get; set; }
		public bool ServerSideOnly { get; set; }
	}
}
