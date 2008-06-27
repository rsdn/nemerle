using System;

using EnvDTE;

namespace Nemerle.VisualStudio.FileCodeModel
{
	internal class VoidCodeTypeRef : CodeTypeRef
	{
		public readonly static CodeTypeRef Value = new VoidCodeTypeRef();

		private VoidCodeTypeRef() {}

		public CodeTypeRef CreateArrayType(int Rank)
		{
			throw new NotImplementedException();
		}

		public DTE         DTE        { get { throw new NotImplementedException(); } }
		public object      Parent     { get { throw new NotImplementedException(); } }
		public vsCMTypeRef TypeKind   { get { return vsCMTypeRef.vsCMTypeRefVoid;  } }
		public string      AsString   { get { return String.Empty; ;               } }
		public string      AsFullName { get { return String.Empty;                 } }

		public CodeType CodeType
		{
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}

		public CodeTypeRef ElementType
		{
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}

		public int Rank
		{
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}
	}
}