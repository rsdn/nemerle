using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using EnvDTE;

using CodeNamespace = EnvDTE.CodeNamespace;

namespace Nemerle.VisualStudio.FileCodeModel
{
	internal class CodeTypeDelegateElement : CodeTypeElement, CodeDelegate
	{
		public CodeTypeDelegateElement(
			DTE                 dte,
			ProjectItem         projectItem,
			CodeTypeDelegate codeType,
			CodeElementBase     parent)
			: base(dte, projectItem, codeType, parent)
		{
		}

		public override vsCMElement Kind
		{
			get { return vsCMElement.vsCMElementDelegate; }
		}

		#region CodeDelegate Members

		object CodeDelegate.Parent { get { return Parent; } }
		CodeNamespace CodeDelegate.Namespace { get { return Parent as CodeNamespace; } }
		CodeElements CodeDelegate.Bases { get { return LoadBases(); } }
		CodeElements CodeDelegate.Members { get { return LoadMembers(); } }

		vsCMAccess CodeDelegate.Access
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

		public CodeParameter AddParameter(string Name, object Type, object Position)
		{
			throw new NotImplementedException();
		}

		public CodeElements Attributes
		{
			get { throw new NotImplementedException(); }
		}

		public CodeClass BaseClass
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

		public CodeElements Parameters
		{
			get { throw new NotImplementedException(); }
		}

		public void RemoveBase(object Element)
		{
			throw new NotImplementedException();
		}

		public void RemoveMember(object Element)
		{
			throw new NotImplementedException();
		}

		public void RemoveParameter(object Element)
		{
			throw new NotImplementedException();
		}

		public CodeTypeRef Type
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

		public bool get_IsDerivedFrom(string FullName)
		{
			throw new NotImplementedException();
		}

		public string get_Prototype(int Flags)
		{
			throw new NotImplementedException();
		}

		#endregion
	}
}
