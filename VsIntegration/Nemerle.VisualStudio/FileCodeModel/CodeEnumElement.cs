using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using EnvDTE;

using CodeNamespace = EnvDTE.CodeNamespace;


namespace Nemerle.VisualStudio.FileCodeModel
{
	class CodeEnumElement : CodeTypeElement, CodeEnum
	{
		public CodeEnumElement(
			DTE                 dte,
			ProjectItem         projectItem,
			CodeTypeDeclaration codeType,
			CodeElementBase     parent)
			: base(dte, projectItem, codeType, parent)
		{
		}

		public override vsCMElement Kind
		{
			get { return vsCMElement.vsCMElementEnum; }
		}

		#region CodeEnum Members

		object CodeEnum.Parent { get { return Parent; } }
		CodeNamespace CodeEnum.Namespace { get { return Parent as CodeNamespace; } }
		CodeElements CodeEnum.Bases { get { return LoadBases(); } }
		CodeElements CodeEnum.Members { get { return LoadMembers(); } }

		vsCMAccess CodeEnum.Access
		{
			get { return CodeDomUtils.GetCMAccess(codeType.Attributes & MemberAttributes.AccessMask); }
			set { throw new NotImplementedException(); }
		}

		public CodeAttribute AddAttribute(string Name, string Value, object Position)
		{
			throw new NotImplementedException();
		}

		public CodeElement AddBase(object Base, object Position)
		{
			throw new NotImplementedException();
		}

		public CodeVariable AddMember(string Name, object Value, object Position)
		{
			throw new NotImplementedException();
		}

		public CodeElements Attributes
		{
			get { throw new NotImplementedException(); }
		}		

		public string Comment
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		public CodeElements DerivedTypes
		{
			get { throw new NotImplementedException(); }
		}

		public string DocComment
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		public void RemoveBase(object Element)
		{
			throw new NotImplementedException();
		}

		public void RemoveMember(object Element)
		{
			throw new NotImplementedException();
		}

		public bool get_IsDerivedFrom(string FullName)
		{
			throw new NotImplementedException();
		}

		#endregion
	}
}
