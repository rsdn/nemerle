using System;
using System.CodeDom;

using EnvDTE;
using System.Collections.Generic;

namespace Nemerle.VisualStudio.FileCodeModel
{
	internal abstract class CodeTypeElement : CodeElementBase
	{
		protected readonly CodeTypeDeclaration codeType;

		public CodeTypeElement(DTE dte, ProjectItem projectItem, CodeTypeDeclaration codeType, CodeElementBase parent)
			: base(dte, projectItem, parent)
		{
			this.codeType = codeType;
		}

		public override string Name
		{
			get { return codeType.Name;  }
			set { codeType.Name = value; }
		}

		public static CodeTypeElement Create(
			DTE dte, ProjectItem projectItem, CodeTypeDeclaration codeType, CodeElementBase parent)
		{
			if(codeType is CodeTypeDelegate)
				return new CodeTypeDelegateElement(dte, projectItem, (CodeTypeDelegate)codeType, parent);
			else if(codeType.IsClass)
			{
				string topDecl = codeType.UserData["Nemerle.TopDeclaration"] as string;
				switch(topDecl)
				{
					case "Variant":
						return new CodeVariantElement(dte, projectItem, codeType, parent);

					case "VariantOption":
						return new CodeVariantElement(dte, projectItem, codeType, parent);

					default:
						return new CodeClassElement(dte, projectItem, codeType, parent);
				}
			}
			else if(codeType.IsEnum)
				return new CodeEnumElement(dte, projectItem, codeType, parent);
			else if(codeType.IsInterface)
				return new CodeInterfaceElement(dte, projectItem, codeType, parent);
			else if(codeType.IsStruct)
				return new CodeStructElement(dte, projectItem, codeType, parent);			
			else
				return new CodeClassElement(dte, projectItem, codeType, parent);
		}		

		protected CodeElements LoadMembers()
		{
			List<CodeElement> elements = new List<CodeElement>(codeType.Members.Count);

			foreach(CodeTypeMember member in codeType.Members)
				elements.Add(CodeTypeMemberElement.Create(DTE, ProjectItem, member, this));

			return new NemerleCodeElements(DTE, ProjectItem, this, elements.ToArray());
		}

		protected CodeElements LoadBases()
		{
			List<CodeElement> elements = new List<CodeElement>(codeType.BaseTypes.Count);

			foreach(CodeTypeReference typeReference in codeType.BaseTypes)
				elements.Add(new CodeTypeReferenceElement(DTE, ProjectItem, typeReference, this));

			return new NemerleCodeElements(DTE, ProjectItem, this, elements.ToArray());
		}
	}
}