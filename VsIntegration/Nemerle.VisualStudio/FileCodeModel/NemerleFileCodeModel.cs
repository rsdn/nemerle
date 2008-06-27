using System;
using System.CodeDom;
using System.Collections.Generic;
using System.IO;

using EnvDTE;

using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Package.Automation;

using Nemerle.Compiler.Utils;

using Nemerle.VisualStudio.Project;

using CodeNamespace = System.CodeDom.CodeNamespace;

namespace Nemerle.VisualStudio.FileCodeModel
{
	internal class NemerleFileCodeModel : EnvDTE.FileCodeModel
	{
		readonly FileNode  _node;
		readonly OAProject _project;
		NemerleOAFileItem  _projectItem;
		DTE                _dte;

		public NemerleFileCodeModel(DTE dte, NemerleOAFileItem projectItem, FileNode node, OAProject project)
		{
			_node        = node;
			_project     = project;
			_projectItem = projectItem;
			_dte         = dte;
		}

		public DTE          DTE          { get { return _dte; } }
		public ProjectItem  Parent       { get { return _projectItem; } }
		public string       Language     { get { return NemerleConstants.LanguageServiceGuidString; } }
		public CodeElements CodeElements { get { return LoadCodeElements(); } }

		CodeElements LoadCodeElements()
		{
			NemerleCodeDomProvider provider = new NemerleCodeDomProvider();

			using (StreamReader reader = new StreamReader(_node.Url))
			{
				CodeCompileUnit   compileUnit = provider.Parse(reader);
				List<CodeElement> elements    = new List<CodeElement>(compileUnit.Namespaces.Count);

				foreach (CodeNamespace ns in compileUnit.Namespaces)
					if (ns.UserData["default"] == null)
						elements.Add(new CodeNamespaceElement(_dte, _projectItem, ns));
					else
						foreach (CodeTypeDeclaration codeType in ns.Types)
							elements.Add(CodeTypeElement.Create(_dte, _projectItem, codeType, null));

				return new NemerleCodeElements(_dte, _projectItem, null, elements.ToArray());
			}
		}

		public CodeElement CodeElementFromPoint(TextPoint Point, vsCMElement Scope)
		{
			throw new NotImplementedException();
		}

		public EnvDTE.CodeNamespace AddNamespace(string Name, object Position)
		{
			throw new NotImplementedException();
		}

		public CodeClass AddClass(
			string     Name,
			object     Position,
			object     Bases,
			object     ImplementedInterfaces,
			vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public CodeInterface AddInterface(string Name, object Position, object Bases, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public CodeFunction AddFunction(
			string       Name,
			vsCMFunction Kind,
			object       Type,
			object       Position,
			vsCMAccess   Access)
		{
			throw new NotImplementedException();
		}

		public CodeVariable AddVariable(string Name, object Type, object Position, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public CodeAttribute AddAttribute(string Name, string Value, object Position)
		{
			throw new NotImplementedException();
		}

		public CodeStruct AddStruct(
			string     Name,
			object     Position,
			object     Bases,
			object     ImplementedInterfaces,
			vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public CodeEnum AddEnum(string Name, object Position, object Bases, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public CodeDelegate AddDelegate(string Name, object Type, object Position, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public void Remove(object Element)
		{
			throw new NotImplementedException();
		}
	}
}