using System;
using System.CodeDom;

using EnvDTE;

using CodeNamespace=EnvDTE.CodeNamespace;

namespace Nemerle.VisualStudio.FileCodeModel
{
	internal class CodeTypeReferenceElement : CodeElementBase, CodeClass
	{
		readonly CodeTypeReference _typeReference;

		public CodeTypeReferenceElement(
			DTE               dte,
			ProjectItem       projectItem,
			CodeTypeReference typeReference,
			CodeElementBase  parent)
			: base(dte, projectItem, parent)
		{
			_typeReference = typeReference;
		}

		public override string Name
		{
			get { return _typeReference.BaseType; }
			set { _typeReference.BaseType = value; }
		}
		
		public override vsCMElement Kind
		{
			 get { return vsCMElement.vsCMElementClass; }
		}

		#region CodeClass

		CodeElement CodeClass.AddBase(object Base, object Position)
		{
			throw new NotImplementedException();
		}

		CodeAttribute CodeClass.AddAttribute(string Name, string Value, object Position)
		{
			throw new NotImplementedException();
		}

		void CodeClass.RemoveBase(object Element)
		{
			throw new NotImplementedException();
		}

		void CodeClass.RemoveMember(object Element)
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

		object        CodeClass.Parent     { get { throw new NotImplementedException(); } }
		CodeNamespace CodeClass.Namespace  { get { throw new NotImplementedException(); } }
		CodeElements  CodeClass.Bases      { get { throw new NotImplementedException(); } }
		CodeElements  CodeClass.Members    { get { throw new NotImplementedException(); } }
		CodeElements  CodeClass.Attributes { get { throw new NotImplementedException(); } }

		vsCMAccess CodeClass.Access
		{
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
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
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}

		#endregion
	}
}
