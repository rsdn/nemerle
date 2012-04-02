// REFERENCE: System.Core
// REFERENCE: Nemerle.Linq

using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Reflection;
using System.Text;

public struct NameOrIndexParameter
{
	public NameOrIndexParameter(string n) { _name = n; }
	public readonly string _name;
}

public class P
{
	static void Main()
	{
		Expression<Func<int>> mi;

		mi = () => 2;

		var _body = Expression.Call(null, ((MethodCallExpression)mi.Body).Method);
	}
}