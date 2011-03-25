/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.IO;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

using EnvDTE;
using Microsoft.VisualStudio.TextManager.Interop;

using CDCodeNamespace = System.CodeDom.CodeNamespace;
using VSCodeNamespace = EnvDTE.CodeNamespace;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using Nemerle.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Designer.Interfaces;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.FileCodeModel
{

	/// <summary>
	/// Provides a FileCodeModel based upon the representation of the program obtained via CodeDom.
	/// 
	/// There are 3 ways a document can be edited that the code model needs to handle:
	///     1. The user edits the document inside of VS.  Here we don't need
	///        to update the code model until the next call back to manipulate it.
	///     2. A script runs which uses EditPoint's to update the text of the document.  
	///     3. The user uses the FileCodeModel to add members to the document.
	/// 
	/// </summary>
	internal abstract class FileCodeModelBase : SimpleCodeElement, EnvDTE.FileCodeModel
	{
		private ProjectItem _projectItem;
		private CodeCompileUnit _ccu;
		private CodeDomCodeNamespace vsTopNamespace; // top-level CodeModel namespace
		private CDCodeNamespace topNamespace;   // top level CodeDom namespace

		public FileCodeModelBase(ProjectItem projectItem)
			: base(projectItem.DTE, "")
		{
			_projectItem = projectItem;
		}

		protected CodeCompileUnit CompileUnit
		{
			get { return _ccu; }
			set { _ccu = value; }
		}

		protected abstract void Initialize();

		#region FileCodeModel Members

		/// <summary>
		/// Adds a class to the top-level (empty) namespace 
		/// </summary>
		/// <param name="Name">The name of the class to add</param>
		/// <param name="Position">The position where the class should be added (1 based)</param>
		/// <param name="Bases">The bases the class dervies from</param>
		/// <param name="ImplementedInterfaces">the interfaces the class implements</param>
		/// <param name="Access">The classes protection level</param>
		public CodeClass AddClass(string Name,
									object Position,
									object Bases,
									object ImplementedInterfaces,
									vsCMAccess Access)
		{
			Initialize();

			InitTopNamespace();

			CodeClass cs = vsTopNamespace.AddClass(Name, Position, Bases, ImplementedInterfaces, Access);

			CommitChanges();

			return cs;
		}

		/// <summary>
		/// Adds a function to the top-level namespace.  Currently adding functions to namespaces doesn't do anything.
		/// </summary>
		public CodeFunction AddFunction(string Name, vsCMFunction Kind, object Type, object Position, vsCMAccess Access)
		{
			Initialize();

			InitTopNamespace();

			CodeFunction cf = vsTopNamespace.AddFunction(Name, Kind, Type, Position, Access);

			CommitChanges();

			return cf;
		}

		/// <summary>
		/// Adds a namespace to the top level namespace.
		/// </summary>
		/// <param name="Name">Required. The name of the new namespace.</param>
		/// <param name="Position">Optional. Default = 0. The code element after which to add the new element. If the value is a CodeElement, then the new element is added immediately after it.</param>
		/// <returns></returns>
		public VSCodeNamespace AddNamespace(string Name, object Position)
		{
			Initialize();

			InitTopNamespace();

			CDCodeNamespace cn = new CDCodeNamespace(Name);
			EnsureNamespaceLinked(cn);

			VSCodeNamespace after = Position as VSCodeNamespace;
			if (after != null)
			{
				for (int i = 0; i < _ccu.Namespaces.Count; i++)
				{
					if (_ccu.Namespaces[i].UserData[CodeKey] == after)
					{
						_ccu.Namespaces.Insert(i + 1, cn);
					}
				}
			}
			else
			{
				int index = (int)Position - 1;
				_ccu.Namespaces.Insert(index, cn);
			}


			CommitChanges();

			return (VSCodeNamespace)cn.UserData[CodeKey];
		}

		/// <summary>
		/// Given a point and an element type to search for returns the element of that type at that point
		/// or null if no element is found.
		/// </summary>
		public CodeElement CodeElementFromPoint(TextPoint Point, vsCMElement Scope)
		{
			Initialize();

			CodeElement res;
			foreach (CDCodeNamespace cn in _ccu.Namespaces)
			{
				if (Scope == vsCMElement.vsCMElementNamespace)
				{
					if (IsInBlockRange(cn, Point))
					{
						EnsureNamespaceLinked(cn);
						return (CodeElement)cn.UserData[CodeKey];
					}
				}

				if (Scope == vsCMElement.vsCMElementImportStmt)
				{
					foreach (CodeNamespaceImport import in cn.Imports)
					{
						if (IsInRange(import, Point))
						{
							return (CodeElement)import.UserData[CodeKey];
						}
					}

					continue;
				}

				foreach (CodeTypeDeclaration ctd in cn.Types)
				{
					res = CheckAttributes(Point, Scope, ctd.CustomAttributes);
					if (res != null) return res;

					if ((ctd.IsClass && Scope == vsCMElement.vsCMElementClass) ||
						(ctd.IsEnum && Scope == vsCMElement.vsCMElementEnum) ||
						(ctd.IsInterface && Scope == vsCMElement.vsCMElementInterface) ||
						(ctd.IsStruct && Scope == vsCMElement.vsCMElementStruct))
					{

						if (IsInBlockRange(ctd, Point)) return (CodeElement)ctd.UserData[CodeKey];

						continue;
					}

					foreach (CodeTypeMember ctm in ctd.Members)
					{
						res = CodeElementFromMember(Point, Scope, ctm);
						if (res != null) return res;
					}
				}
			}
			return null;
		}

		/// <summary>
		/// Gets all the CodeElements that live in the namespace
		/// </summary>
		public CodeElements CodeElements
		{
			get
			{
				Initialize();

				CodeDomCodeElements res = new CodeDomCodeElements(DTE, this);
				foreach (CDCodeNamespace member in _ccu.Namespaces)
				{
					EnsureNamespaceLinked(member);

					if (member.UserData["default"] == null)
						res.Add((CodeElement)member.UserData[CodeKey]);
					else
						foreach (CodeElement codeType in ((CodeElement)member.UserData[CodeKey]).Children)
							res.Add(codeType);
				}
				return res;
			}
		}

		private void EnsureNamespaceLinked(CDCodeNamespace member)
		{
			if (member.UserData[CodeKey] == null)
			{
				CodeDomCodeNamespace cdcn = new CodeDomCodeNamespace(DTE, member.Name, this);
				cdcn.CodeObject = member;
				member.UserData[CodeKey] = cdcn;
			}
		}

		/// <summary>
		/// Removes an element from the namespace.
		/// </summary>
		/// <param name="Element"></param>
		public void Remove(object Element)
		{
			Initialize();

			int index = ((CodeDomCodeElements)CodeElements).IndexOf((SimpleCodeElement)Element);

			_ccu.Namespaces.RemoveAt(index);
		}

		public CodeDelegate AddDelegate(string Name, object Type, object Position, vsCMAccess Access)
		{
			Initialize();

			InitTopNamespace();

			CodeDelegate cd = vsTopNamespace.AddDelegate(Name, Type, Position, Access);

			CommitChanges();

			return cd;
		}

		public CodeEnum AddEnum(string Name, object Position, object Bases, vsCMAccess Access)
		{
			Initialize();

			InitTopNamespace();

			CodeEnum ce = vsTopNamespace.AddEnum(Name, Position, Bases, Access);

			CommitChanges();

			return ce;
		}

		public CodeInterface AddInterface(string Name, object Position, object Bases, vsCMAccess Access)
		{
			Initialize();

			InitTopNamespace();

			CodeInterface ci = vsTopNamespace.AddInterface(Name, Position, Bases, Access);

			CommitChanges();

			return ci;
		}

		public CodeStruct AddStruct(string Name, object Position, object Bases, object ImplementedInterfaces, vsCMAccess Access)
		{
			Initialize();

			InitTopNamespace();

			CodeStruct cs = vsTopNamespace.AddStruct(Name, Position, Bases, ImplementedInterfaces, Access);

			CommitChanges();

			return cs;
		}

		#endregion

		public CodeVariable AddVariable(string Name, object Type, object Position, vsCMAccess Access)
		{
			throw new NotImplementedException();
		}

		public CodeAttribute AddAttribute(string Name, string Value, object Position)
		{
			throw new NotImplementedException();
		}

		internal void CommitChanges()
		{
			// do nothing
		}

		#region Position Updater
		/// <summary>
		/// Walks the code dom tree updating positions of items based upon an 
		/// edit the user made directly to the text document
		/// </summary>
		[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
		internal void UpdatePositions(int fromLine, int lines, int chars)
		{
			if (_ccu == null) Initialize();

			foreach (CDCodeNamespace cn in _ccu.Namespaces)
			{
				foreach (CodeTypeDeclaration ctd in cn.Types)
				{
					AdjustTypeDeclaration(fromLine, lines, chars, ctd);
				}
			}
		}

		[SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily")]
		[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
		private static void AdjustTypeDeclaration(int fromLine, int lines, int chars, CodeTypeDeclaration ctd)
		{
			foreach (CodeTypeMember ctm in ctd.Members)
			{
				DoOneAdjust(fromLine, lines, chars, ctm);

				if (ctm is CodeTypeDeclaration)
				{
					AdjustTypeDeclaration(fromLine, lines, chars, ctm as CodeTypeDeclaration);
				}
				else if (ctm is CodeMemberField)
				{
					AdjustField(fromLine, lines, chars, ctm);
				}
				else if (ctm is CodeMemberMethod)
				{
					AdjustMethod(fromLine, lines, chars, ctm);
				}
				else if (ctm is CodeMemberProperty)
				{
					AdjustProperty(fromLine, lines, chars, ctm);
				}
				else if (ctm is CodeMemberEvent)
				{
					// already adjusted the event, no submembers to adjust
				}
			}
		}

		[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
		private static void AdjustField(int fromLine, int lines, int chars, CodeTypeMember cmm)
		{
			CodeMemberField field = cmm as CodeMemberField;

			DoOneAdjust(fromLine, lines, chars, field.InitExpression);
		}

		[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
		private static void AdjustProperty(int fromLine, int lines, int chars, CodeTypeMember ctm)
		{
			CodeMemberProperty cmp = ctm as CodeMemberProperty;

			AdjustParameters(fromLine, lines, chars, cmp.Parameters);

			if (cmp.HasGet)
			{
				AdjustStatements(fromLine, lines, chars, cmp.GetStatements);
			}

			if (cmp.HasSet)
			{
				AdjustStatements(fromLine, lines, chars, cmp.SetStatements);
			}
		}

		[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
		private static void AdjustMethod(int fromLine, int lines, int chars, CodeTypeMember ctm)
		{
			CodeMemberMethod cmm = ctm as CodeMemberMethod;
			AdjustParameters(fromLine, lines, chars, cmm.Parameters);

			foreach (CodeTypeParameter ctp in cmm.TypeParameters)
			{
				DoOneAdjust(fromLine, lines, chars, ctp);
			}

			AdjustStatements(fromLine, lines, chars, cmm.Statements);
		}

		[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
		private static void AdjustStatements(int fromLine, int lines, int chars, CodeStatementCollection statements)
		{
			foreach (CodeStatement cs in statements)
			{
				DoOneAdjust(fromLine, lines, chars, cs);
			}
		}

		[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
		private static void AdjustParameters(int fromLine, int lines, int chars, CodeParameterDeclarationExpressionCollection parameters)
		{
			foreach (CodeParameterDeclarationExpression param in parameters)
			{
				DoOneAdjust(fromLine, lines, chars, param);
			}
		}

		[SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "chars")]
		[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
		private static void DoOneAdjust(int fromLine, int lines, int chars, CodeObject co)
		{
			if (co.UserData["Line"] != null)
			{
				int curLine = (int)co.UserData["Line"];
				int curCol = (int)co.UserData["Column"];

				if (curLine == fromLine)
				{
				}
				else if ((lines > 0) == (curLine > fromLine))
				{
					// line needs to be adjusted
					co.UserData["Line"] = curLine + lines;
				}

				if (co.UserData["EndLine"] != null)
				{
					int endLine = (int)co.UserData["EndLine"];
					int endCol = (int)co.UserData["EndColumn"];

					if (curLine == fromLine)
					{
					}
					else if ((lines > 0) == (curLine > fromLine))
					{
						// line needs to be adjusted
						co.UserData["EndLine"] = curLine + lines;
					}
				}
			}
		}
		#endregion

		[SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
		[SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily")]
		private static CodeElement CodeElementFromMember(TextPoint Point, vsCMElement Scope, CodeTypeMember ctm)
		{
			CodeElement res = CheckAttributes(Point, Scope, ctm.CustomAttributes);
			if (res != null) return res;

			CodeMemberMethod method = ctm as CodeMemberMethod;
			if (method != null)
			{
				if (Scope == vsCMElement.vsCMElementFunction)
				{
					if (IsInBlockRange(method, Point)) return (CodeElement)method.UserData[CodeKey];
				}
				//!!! walk method
				if (Scope == vsCMElement.vsCMElementParameter || Scope == vsCMElement.vsCMElementAttribute)
				{
					foreach (CodeParameterDeclarationExpression param in method.Parameters)
					{
						if (IsInRange(param, Point)) return (CodeElement)method.UserData[CodeKey];

						res = CheckAttributes(Point, Scope, param.CustomAttributes);
						if (res != null) return res;
					}

					foreach (CodeTypeParameter ctp in method.TypeParameters)
					{
						if (IsInRange(ctp, Point)) return (CodeElement)method.UserData[CodeKey];

						res = CheckAttributes(Point, Scope, ctp.CustomAttributes);
						if (res != null) return res;
					}
				}

				res = CheckAttributes(Point, Scope, method.ReturnTypeCustomAttributes);
				if (res != null) return res;

				res = WalkStatements(Point, Scope, method.Statements);
				if (res != null) return res;
			}
			else if (ctm is CodeMemberEvent)
			{
				if (Scope == vsCMElement.vsCMElementEvent)
				{
					if (IsInRange(ctm, Point)) return (CodeElement)ctm.UserData[CodeKey];
				}
			}
			else if (ctm is CodeMemberProperty)
			{
				CodeMemberProperty cmp = ctm as CodeMemberProperty;

				foreach (CodeParameterDeclarationExpression param in cmp.Parameters)
				{
					if (IsInRange(param, Point)) return (CodeElement)method.UserData[CodeKey];

					res = CheckAttributes(Point, Scope, param.CustomAttributes);
					if (res != null) return res;
				}

				if (Scope == vsCMElement.vsCMElementProperty)
				{
					if (IsInBlockRange(ctm, Point)) return (CodeElement)ctm.UserData[CodeKey];
				}

				if (cmp.HasGet) WalkStatements(Point, Scope, cmp.GetStatements);
				if (cmp.HasSet) WalkStatements(Point, Scope, cmp.SetStatements);
			}
			else if (ctm is CodeMemberField)
			{
				if (Scope == vsCMElement.vsCMElementVariable)
				{
					if (IsInRange(ctm, Point)) return (CodeElement)ctm.UserData[CodeKey];
				}
			}
			return null;
		}

		private static CodeElement WalkStatements(TextPoint Point, vsCMElement Scope, CodeStatementCollection statements)
		{
			foreach (CodeStatement cs in statements)
			{
				if (Scope == vsCMElement.vsCMElementAssignmentStmt && cs is CodeAssignStatement)
				{
					if (IsInRange(cs, Point)) return (CodeElement)cs.UserData[CodeKey];
				}

				if (Scope == vsCMElement.vsCMElementLocalDeclStmt && cs is CodeVariableDeclarationStatement)
				{
					if (IsInRange(cs, Point)) return (CodeElement)cs.UserData[CodeKey];
				}

				CodeExpressionStatement ces = cs as CodeExpressionStatement;
				if (ces != null)
				{
					if (Scope == vsCMElement.vsCMElementFunctionInvokeStmt && ces.Expression is CodeMethodInvokeExpression)
					{
						if (IsInRange(cs, Point)) return (CodeElement)cs.UserData[CodeKey];
					}
					if (Scope == vsCMElement.vsCMElementPropertySetStmt && ces.Expression is CodePropertySetValueReferenceExpression)
					{
						if (IsInRange(cs, Point)) return (CodeElement)cs.UserData[CodeKey];
					}
				}

				if (Scope == vsCMElement.vsCMElementOther && IsInRange(cs, Point)) return (CodeElement)cs.UserData[CodeKey];
			}
			return null;
		}

		[SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "Point")]
		private static CodeElement CheckAttributes(TextPoint Point, vsCMElement Scope, CodeAttributeDeclarationCollection attributes)
		{
			if (Scope == vsCMElement.vsCMElementAttribute)
			{
				foreach (CodeAttributeDeclaration cad in attributes)
				{
					//!!! no user data on attributes!
					//if (IsInRange(cad, Point)) return (CodeElement)method.UserData[CodeKey];
				}
			}
			return null;
		}
		private static bool IsInBlockRange(CodeObject codeObject, TextPoint point)
		{
			Nullable<int> line = UserDataInt(codeObject, "Line");
			Nullable<int> endLine = UserDataInt(codeObject, "EndLine");

			// match in the middle of a block
			if (line <= point.Line && point.Line <= endLine)
			{
				return true;
			}
			return false;
		}
		private static bool IsInRange(CodeObject codeObject, TextPoint point)
		{
			Nullable<int> line = UserDataInt(codeObject, "Line");
			Nullable<int> endLine = UserDataInt(codeObject, "EndLine");

			// match in the middle of a block
			if (line < point.Line && point.Line < endLine)
			{
				return true;
			}

			if (line == point.Line || endLine == line)
			{
				// single line match, make sure the columns are right
				Nullable<int> col = UserDataInt(codeObject, "Column");
				Nullable<int> endCol = UserDataInt(codeObject, "EndColumn");

				if (line == point.Line &&
					col <= point.LineCharOffset &&
					point.LineCharOffset < endCol)
				{
					return true;
				}
				else if (endLine != line &&
					endLine == point.Line &&
					endCol <= point.LineCharOffset &&
					point.LineCharOffset < endCol)
				{
					return true;
				}
			}

			return false;
		}

		private static Nullable<int> UserDataInt(CodeObject codeObject, string name)
		{
			object val = codeObject.UserData[name];
			if (val == null) return null;

			return (int)val;
		}

		public override TextPoint GetEndPoint(vsCMPart Part)
		{
			return EndPoint;
		}

		public override TextPoint GetStartPoint(vsCMPart Part)
		{
			return StartPoint;
		}

		public override TextPoint EndPoint
		{
			get { return ((TextDocument)this.ProjectItem.Document.Object("TextDocument")).EndPoint; }
		}

		public override TextPoint StartPoint
		{
			get { return ((TextDocument)this.ProjectItem.Document.Object("TextDocument")).StartPoint; }
		}

		public override CodeElements Children
		{
			get { return CodeElements; }
		}

		public override CodeElements Collection
		{
			get { return null; }
		}

		public override string FullName
		{
			get { return DTE.FullName; }
		}

		public override vsCMElement Kind
		{
			get { return vsCMElement.vsCMElementModule; }
		}

		/// <summary>
		/// Ensures that we have a top-level namespace (cached in our topNamespace field)
		/// </summary>
		private void InitTopNamespace()
		{
			if (vsTopNamespace == null)
			{
				vsTopNamespace = new CodeDomCodeNamespace(DTE, String.Empty, this);

				foreach (CDCodeNamespace ns in _ccu.Namespaces)
				{
					if (String.IsNullOrEmpty(ns.Name))
					{
						topNamespace = ns;
						break;
					}
				}

				if (topNamespace == null)
				{
					topNamespace = new CDCodeNamespace(String.Empty);
					_ccu.Namespaces.Add(topNamespace);
				}

				vsTopNamespace.CodeObject = topNamespace;
				topNamespace.UserData[CodeKey] = vsTopNamespace;
			}
		}

		public override ProjectItem ProjectItem
		{
			get { return _projectItem; }
		}

		public ProjectItem Parent
		{
			get { return _projectItem; }
		}

		internal abstract void FlushChanges();
	}
}
