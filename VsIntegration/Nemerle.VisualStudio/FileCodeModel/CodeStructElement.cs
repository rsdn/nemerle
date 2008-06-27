using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using EnvDTE;

using CodeNamespace = EnvDTE.CodeNamespace;

namespace Nemerle.VisualStudio.FileCodeModel
{
	class CodeStructElement : CodeTypeElement, CodeStruct
	{
		public CodeStructElement(
			DTE                 dte,
			ProjectItem         projectItem,
			CodeTypeDeclaration codeType,
			CodeElementBase     parent)
			: base(dte, projectItem, codeType, parent)
		{
		}

		public override vsCMElement Kind
		{
			get { return vsCMElement.vsCMElementStruct; }
		}

		#region CodeStruct Members

		object CodeStruct.Parent { get { return Parent; } }
		CodeNamespace CodeStruct.Namespace { get { return Parent as CodeNamespace; } }
		CodeElements CodeStruct.Bases { get { return LoadBases(); } }
		CodeElements CodeStruct.Members { get { return LoadMembers(); } }

		vsCMAccess CodeStruct.Access
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

		public CodeClass AddClass(string Name, object Position, object Bases, object ImplementedInterfaces, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public CodeDelegate AddDelegate(string Name, object Type, object Position, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public CodeEnum AddEnum(string Name, object Position, object Bases, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public CodeFunction AddFunction(string Name, vsCMFunction Kind, object Type, object Position, vsCMAccess Access, object Location)
		{
			throw new NotImplementedException();
		}

		public CodeInterface AddImplementedInterface(object Base, object Position)
		{
			throw new NotImplementedException();
		}

		public CodeProperty AddProperty(string GetterName, string PutterName, object Type, object Position, vsCMAccess Access, object Location)
		{
			throw new NotImplementedException();
		}

		public CodeStruct AddStruct(string Name, object Position, object Bases, object ImplementedInterfaces, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public CodeVariable AddVariable(string Name, object Type, object Position, vsCMAccess Access, object Location)
		{
			throw new NotImplementedException();
		}

		public CodeElements Attributes
		{
			get { throw new NotImplementedException(); }
		}

		public CodeElements Bases
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

		public CodeElements ImplementedInterfaces
		{
			get { throw new NotImplementedException(); }
		}

		public bool IsAbstract
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

		public void RemoveInterface(object Element)
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
