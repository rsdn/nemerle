//
// NemerleCodeProvider.cs
//
// Author:
//	Atsushi Enomoto <atsushi@ximian.com>
//
// Too short to be copyrighted.
//
using System;
using System.CodeDom.Compiler;
using System.ComponentModel;

namespace Nemerle.Contrib
{
	public class NemerleCodeProvider : CodeDomProvider
	{
		public NemerleCodeProvider ()
		{
		}

		public override string FileExtension {
			get {
				return "n";
			}
		}

		public override ICodeCompiler CreateCompiler ()
		{
			return new NemerleCodeCompiler ();
		}

		public override ICodeGenerator CreateGenerator ()
		{
			return new NemerleCodeGenerator();
		}

		public override TypeConverter GetConverter (Type Type)
		{
			throw new NotImplementedException ();
		}
	}
}
