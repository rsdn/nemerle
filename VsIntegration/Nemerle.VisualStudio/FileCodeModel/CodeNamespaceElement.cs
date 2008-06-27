using System;
using System.CodeDom;
using System.Collections.Generic;

using EnvDTE;

using CodeNamespace=System.CodeDom.CodeNamespace;

namespace Nemerle.VisualStudio.FileCodeModel
{
	internal class CodeNamespaceElement : CodeElementBase, EnvDTE.CodeNamespace
	{
		readonly CodeNamespace _codeNamespace;

		public CodeNamespaceElement(
			DTE           dte,
			ProjectItem   projectItem,
			CodeNamespace codeNamespace)
			: base(dte, projectItem)
		{
			_codeNamespace = codeNamespace;
		}

		public override string Name
		{
			get { return _codeNamespace.Name;  }
			set { _codeNamespace.Name = value; }
		}
		
		public override vsCMElement Kind
		{
			 get { return vsCMElement.vsCMElementNamespace; }
		}

		#region CodeNamespace

		EnvDTE.CodeNamespace EnvDTE.CodeNamespace.AddNamespace(string Name, object Position)
		{
			throw new NotImplementedException();
		}

		CodeClass EnvDTE.CodeNamespace.AddClass(
			string     Name,
			object     Position,
			object     Bases,
			object     ImplementedInterfaces,
			vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		CodeInterface EnvDTE.CodeNamespace.AddInterface(
			string Name, object Position, object Bases, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		CodeStruct EnvDTE.CodeNamespace.AddStruct(
			string Name, object Position, object Bases, object ImplementedInterfaces, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		CodeEnum EnvDTE.CodeNamespace.AddEnum(
			string Name, object Position, object Bases, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		CodeDelegate EnvDTE.CodeNamespace.AddDelegate(
			string Name, object Type, object Position, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		void EnvDTE.CodeNamespace.Remove(object Element)
		{
			throw new NotImplementedException();
		}

		object       EnvDTE.CodeNamespace.Parent  { get { throw new NotImplementedException(); } }
		CodeElements EnvDTE.CodeNamespace.Members { get { return LoadMembers(); } }

		CodeElements LoadMembers()
		{
			List<CodeElement> elements = new List<CodeElement>(_codeNamespace.Types.Count);

			foreach (CodeTypeDeclaration type in _codeNamespace.Types)
				elements.Add(CodeTypeElement.Create(DTE, ProjectItem, type, this));

			return new NemerleCodeElements(DTE, ProjectItem, this, elements.ToArray());
		}

		string EnvDTE.CodeNamespace.DocComment
		{
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}

		string EnvDTE.CodeNamespace.Comment
		{
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}

		#endregion
	}
}