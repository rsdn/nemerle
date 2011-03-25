using System;
using System.CodeDom;
using System.Collections.Generic;

using EnvDTE;

using CodeNamespace = EnvDTE.CodeNamespace;
using System.Diagnostics.CodeAnalysis;

namespace Nemerle.VisualStudio.FileCodeModel
{
	class CodeDomCodeVariant : CodeDomCodeClass
	{
		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "0#dte")]
		public CodeDomCodeVariant(DTE dte, CodeElement parent, string name, object bases, object interfaces, vsCMAccess access)
			: base(dte, parent, name, bases, interfaces, access)
		{ }

		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "0#dte")]
		public CodeDomCodeVariant(DTE dte, CodeElement parent, CodeTypeDeclaration declaration)
			: base(dte, parent, declaration)
		{ }
	}
}
