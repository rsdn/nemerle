// REFERENCE: System.Core

using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Reflection;
using System.Text;

namespace LinqToDB.Data.Linq
{
	using B = System.Boolean; // FIXME: using B = Boolean;
	using C = System.Char;
	using S = System.String;
	using I = System.Int32;
	using O = System.Object;
	using D = System.DateTime;
	using T = System.TimeSpan;
	using F = System.Double;
	using M = System.Decimal;

	public static class Expressions
	{
		public static void MapMember(MemberInfo _memberInfo, LambdaExpression _expression)
		{
			//MapMember("", memberInfo, expression);
		}

		public static void MapMember(string _providerName, MemberInfo _memberInfo, LambdaExpression _expression)
		{
		}

		public static void MapMember(Expression<Func<object>> memberInfo, LambdaExpression expression)
		{
			MapMember("", M(memberInfo), expression);
		}

		public static MemberInfo M<T>(Expression<Func<T, object>> _func)
		{
			return null;
		}

		public static MemberInfo M(Expression<Func<object>> _func)
		{
			return null;
		}
	}
}

public class P
{
	static void Main()
	{
	}
}