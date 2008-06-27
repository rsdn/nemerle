using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using EnvDTE;

using CodeNamespace = EnvDTE.CodeNamespace;

namespace Nemerle.VisualStudio.FileCodeModel
{
	/// <summary>
	/// Represents an interface in source code.
	/// </summary>
	class CodeInterfaceElement : CodeTypeElement, CodeInterface
	{
		public CodeInterfaceElement(
			DTE                 dte,
			ProjectItem         projectItem,
			CodeTypeDeclaration codeType,
			CodeElementBase     parent)
			: base(dte, projectItem, codeType, parent)
		{
		}

		public override vsCMElement Kind
		{
			get { return vsCMElement.vsCMElementInterface; }
		}		

		#region CodeInterface Members

		object CodeInterface.Parent { get { return Parent; } }
		CodeNamespace CodeInterface.Namespace { get { return Parent as CodeNamespace; } }
		CodeElements CodeInterface.Bases { get { return LoadBases(); } }
		CodeElements CodeInterface.Members { get { return LoadMembers(); } }

		vsCMAccess CodeInterface.Access
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

		public CodeFunction AddFunction(string Name, vsCMFunction Kind, object Type, object Position, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public CodeProperty AddProperty(string GetterName, string PutterName, object Type, object Position, vsCMAccess Access, object Location)
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
