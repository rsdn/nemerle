using System;
using System.CodeDom;
using System.Collections.Generic;

using EnvDTE;

using CodeNamespace = EnvDTE.CodeNamespace;

namespace Nemerle.VisualStudio.FileCodeModel
{
	class CodeVariantElement : CodeTypeElement, CodeClass
	{
		public CodeVariantElement(
			DTE                 dte,
			ProjectItem         projectItem,
			CodeTypeDeclaration codeType,
			CodeElementBase     parent)
			: base(dte, projectItem, codeType, parent)
		{
		}

		public override vsCMElement Kind
		{//here we use Union element, since C# has no variants
			 get { return vsCMElement.vsCMElementUnion; }
		}

		#region CodeClass

		CodeElement CodeClass.AddBase(object baseName, object position)
		{
			throw new NotImplementedException();
		}

		CodeAttribute CodeClass.AddAttribute(string name, string value, object position)
		{
			throw new NotImplementedException();
		}

		void CodeClass.RemoveBase(object element)
		{
			throw new NotImplementedException();
		}

		void CodeClass.RemoveMember(object element)
		{
			throw new NotImplementedException();
		}

		CodeInterface CodeClass.AddImplementedInterface(object Base, object Position)
		{
			throw new NotImplementedException();
		}

		CodeFunction CodeClass.AddFunction(
			string       Name,
			vsCMFunction Kind,
			object       Type,
			object       Position,
			vsCMAccess   Access,
			object       Location)
		{
			throw new NotImplementedException();
		}

		CodeVariable CodeClass.AddVariable(
			string     Name,
			object     Type,
			object     Position,
			vsCMAccess Access,
			object     Location)
		{
			throw new NotImplementedException();
		}

		CodeProperty CodeClass.AddProperty(
			string     GetterName,
			string     PutterName,
			object     Type,
			object     Position,
			vsCMAccess Access,
			object     Location)
		{
			throw new NotImplementedException();
		}

		CodeClass CodeClass.AddClass(
			string     Name,
			object     Position,
			object     Bases,
			object     ImplementedInterfaces,
			vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		CodeStruct CodeClass.AddStruct(
			string     Name,
			object     Position,
			object     Bases,
			object     ImplementedInterfaces,
			vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		CodeEnum CodeClass.AddEnum(string Name, object Position, object Bases, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		CodeDelegate CodeClass.AddDelegate(string Name, object Type, object Position, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		void CodeClass.RemoveInterface(object Element)
		{
			throw new NotImplementedException();
		}

		bool CodeClass.get_IsDerivedFrom(string FullName)
		{
			throw new NotImplementedException();
		}

		object        CodeClass.Parent    { get { return Parent;                  } }
		CodeNamespace CodeClass.Namespace { get { return Parent as CodeNamespace; } }
		CodeElements  CodeClass.Bases     { get { return LoadBases();             } }
		CodeElements  CodeClass.Members   { get { return LoadMembers();           } }	

		vsCMAccess CodeClass.Access
		{
			get { return CodeDomUtils.GetCMAccess(codeType.Attributes & MemberAttributes.AccessMask); }
			set { throw new NotImplementedException(); }
		}

		CodeElements CodeClass.Attributes
		{
			 get { throw new NotImplementedException(); }
		}
		
		string CodeClass.DocComment
		{
			get { throw new NotImplementedException(); } 
			set { throw new NotImplementedException(); }
		}
		
		string CodeClass.Comment
		{
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}
		
		CodeElements CodeClass.DerivedTypes
		{
			 get { throw new NotImplementedException(); }
		}
		
		CodeElements CodeClass.ImplementedInterfaces
		{
			 get { throw new NotImplementedException(); }
		}

		bool CodeClass.IsAbstract
		{
			get
			{
				return (codeType.Attributes & MemberAttributes.Abstract) == MemberAttributes.Abstract;
			}
			set
			{
				if (value) codeType.Attributes |= MemberAttributes.Abstract;
				else       codeType.Attributes ^= MemberAttributes.Abstract;
			}
		}

		#endregion
	}
}