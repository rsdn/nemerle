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
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;
using EnvDTE;

using System.Diagnostics;

namespace Nemerle.VisualStudio.FileCodeModel
{
	[ComVisible(true)]
	public abstract class CodeDomCodeElement<CodeTypeType> : SimpleCodeElement, ICodeDomElement
		where CodeTypeType : CodeObject
	{
		private CodeTypeType codeObj;

		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "0#dte")]
		protected CodeDomCodeElement(DTE dte, string name)
			: base(dte, name)
		{
		}

		private TextDocument GetTextDocument()
		{
			TextDocument document = null;
			if (ProjectItem != null)
			{
				if (ProjectItem.Document == null)
				{
					ProjectItem.Open(Guid.Empty.ToString("B"));
				}

				document = (TextDocument)ProjectItem.Document.Object("TextDocument");
			}

			return document;
		}

		public override TextPoint EndPoint
		{
			get
			{
				return GetEndPoint(vsCMPart.vsCMPartWhole);
			}
		}

		public override TextPoint StartPoint
		{
			get
			{
				return GetStartPoint(vsCMPart.vsCMPartWhole);
			}
		}

		public override TextPoint GetEndPoint(vsCMPart part)
		{
			TextPoint point = null;
			var member = CodeObject.UserData["Member"] as Nemerle.Compiler.Parsetree.MemberBase;
			if (member != null)
			{
				if (part == vsCMPart.vsCMPartBody && member.BodyLocation != null)
					point = new CodeDomTextPoint(GetTextDocument(), member.BodyLocation.EndColumn, member.BodyLocation.EndLine);
				else if (part == vsCMPart.vsCMPartNavigate && member.BodyLocation != null)
					point = GetNavigationPoint(member.BodyLocation);
				else if (member.Location != null)
					point = new CodeDomTextPoint(GetTextDocument(), member.Location.EndColumn, member.Location.EndLine);
			}

			return (point != null) ? point : new NullTextPoint();
		}

		public override TextPoint GetStartPoint(vsCMPart part)
		{
			TextPoint point = null;
			var member = CodeObject.UserData["Member"] as Nemerle.Compiler.Parsetree.MemberBase;
			if (member != null)
			{
				if (part == vsCMPart.vsCMPartBody && member.BodyLocation != null)
					point = new CodeDomTextPoint(GetTextDocument(), member.BodyLocation.Column, member.BodyLocation.Line);
				else if (part == vsCMPart.vsCMPartNavigate && member.BodyLocation != null)
					point = GetNavigationPoint(member.BodyLocation);
				else if (member.Location != null)
					point = new CodeDomTextPoint(GetTextDocument(), member.Location.Column, member.Location.Line);
			}

			return (point != null) ? point : new NullTextPoint();
		}

		// The location in the source code to which the insertion point moves when you double-click an element in Designer or Class View.
		private TextPoint GetNavigationPoint(Nemerle.Compiler.Location bodyLocation)
		{
			TextPoint point = null;

			if (bodyLocation.Line < bodyLocation.EndLine) // тело метода состоит более чем из одной строки
			{
				var tmpPoint = new CodeDomTextPoint(GetTextDocument(), bodyLocation.Column, bodyLocation.Line);
				var editPoint = tmpPoint.CreateEditPoint();

				// предпоследняя строка тела метода
				var lines = editPoint.GetLines(bodyLocation.EndLine, bodyLocation.EndLine + 1);

				if (lines.Trim(' ', '\t').Length > 0) // если предпоследняя строка состоит не только из пробельных символов
				{
					if (bodyLocation.Line == bodyLocation.EndLine - 1)
						// тело метода состоит из двух строк
						// установим курсор на последней позиции предпоследней строки
						// void foo() 
						// {_
						// }
						point = new CodeDomTextPoint(GetTextDocument(), bodyLocation.Column, bodyLocation.EndLine - 1);
					else
						// тело метода состоит из более чем двух строк
						// установим курсор на перед первым непробельныи символом предпоследней строки
						// void foo() 
						// {
						//     _source 
						// }
						point = new CodeDomTextPoint(GetTextDocument(), lines.Length - lines.TrimStart(' ', '\t').Length, bodyLocation.EndLine - 1);
				}
				else
					// если предпоследняя строка - пустая (состоит тлько из пробельных симвлов)
					// установим курсор на последнюю позицию предпоследней строки
					// void foo() 
					// {
					//     _
					// }
					point = new CodeDomTextPoint(GetTextDocument(), lines.Length, bodyLocation.EndLine - 1);
			}
			else
				// тело метода стостоит только из одной строки
				// установим курсор на позицию за началом тела метода
				// void foo() 
				// {_    }
				point = new CodeDomTextPoint(GetTextDocument(), bodyLocation.Column, bodyLocation.Line);

			return point;
		}

		public CodeTypeType CodeObject
		{
			get
			{
				return codeObj;
			}
			set
			{
				codeObj = value;
			}
		}

		#region ICodeDomElement Members

		public object UntypedCodeObject
		{
			get { return codeObj; }
		}

		public abstract object ParentElement
		{
			[SuppressMessage("Microsoft.Security", "CA2119:SealMethodsThatSatisfyPrivateInterfaces")]
			get;
		}

		#endregion

		#region Common protected helpers

		[SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "collection")]
		protected CodeElements GetCustomAttributes(CodeAttributeDeclarationCollection collection)
		{
			CodeDomCodeElements res = new CodeDomCodeElements(DTE, this);
			//!!! not right
			return res;
		}

		[SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "value")]
		protected CodeAttribute AddCustomAttribute(CodeAttributeDeclarationCollection collection, string name, string value, object position)
		{
			CodeDomCodeAttribute cdca = new CodeDomCodeAttribute(DTE, this, name);
			collection.Insert(AttributePositionToIndex(collection, position), cdca.CodeObject);

			return cdca;
		}

		protected string GetComment(CodeCommentStatementCollection collection, bool docComment)
		{
			StringBuilder res = new StringBuilder();
			foreach (CodeComment comment in collection)
			{
				if (comment.DocComment == docComment)
				{
					res.AppendLine(comment.Text);
				}
			}

			return res.ToString();
		}

		protected void ReplaceComment(CodeCommentStatementCollection collection, string value, bool docComment)
		{
			int i = 0;
			while (i < collection.Count)
			{
				if (collection[i].Comment.DocComment != docComment)
				{
					i++;
				}
				else
				{
					collection.RemoveAt(i);
				}
			}

			string[] strings = value.Split('\n');
			for (i = 0; i < strings.Length; i++)
			{
				collection.Add(new CodeCommentStatement(new CodeComment(strings[i], docComment)));
			}
		}

		protected CodeParameter AddParameter(CodeParameterDeclarationExpressionCollection collection, string name, object type, object position)
		{
			CodeTypeRef typeRef = ObjectToTypeRef(type);
			CodeDomCodeParameter cdParam = new CodeDomCodeParameter(DTE, this, name, typeRef);

			collection.Insert(PositionToParameterIndex(collection, position), cdParam.CodeObject);
			return cdParam;
		}

		protected void RemoveParameter(CodeParameterDeclarationExpressionCollection collection, object element)
		{
			string strElement = element as string;
			int index = 0;
			foreach (CodeParameterDeclarationExpression param in collection)
			{
				if (strElement == null && CodeDomCodeParameter.GetCodeParameter(param) == element)
				{
					collection.RemoveAt(index);
					break;
				}
				else if (strElement != null && param.Name == strElement)
				{
					collection.RemoveAt(index);
					break;
				}

				index++;
			}
		}

		protected CodeElements GetParameters(CodeParameterDeclarationExpressionCollection collection)
		{
			CodeDomCodeElements res = new CodeDomCodeElements(DTE, this);
			foreach (CodeParameterDeclarationExpression param in collection)
			{
				if (param.UserData[CodeKey] == null)
				{
					param.UserData[CodeKey] = new CodeDomCodeParameter(this, param);
				}
				res.Add(CodeDomCodeParameter.GetCodeParameter(param));
			}
			return res;
		}

		[SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily")]
		protected void CommitChanges()
		{
			object curParent = ParentElement;
			while (!(curParent is EnvDTE.FileCodeModel))
			{
				curParent = ((ICodeDomElement)curParent).ParentElement;
				NemerleFileCodeModel fcm = curParent as NemerleFileCodeModel;
				if (fcm != null)
				{
					fcm.CommitChanges();
					break;
				}

				if (curParent == null)
				{
					Debug.Assert(false, "Not ICodeDomElement or CodeDomFileCodeModel in parent hierarchy");
					break;
				}
			}
		}
		#endregion

		#region Private helpers

		private static int PositionToParameterIndex(CodeParameterDeclarationExpressionCollection collection, object Position)
		{
			ICodeDomElement icde = Position as ICodeDomElement;
			if (icde != null)
			{
				return collection.IndexOf((CodeParameterDeclarationExpression)icde.UntypedCodeObject) + 1;
			}

			if (Position == System.Reflection.Missing.Value)
			{
				return collection.Count;
			}

			int pos = (int)Position;
			if (pos == -1)
			{
				return collection.Count;
			}
			return pos - 1;
		}


		private static int AttributePositionToIndex(CodeAttributeDeclarationCollection collection, object Position)
		{
			ICodeDomElement icde = Position as ICodeDomElement;
			if (icde != null)
			{
				return collection.IndexOf((CodeAttributeDeclaration)icde.UntypedCodeObject) + 1;
			}

			if (Position == System.Reflection.Missing.Value)
			{
				return collection.Count;
			}

			int pos = (int)Position;
			if (pos == -1)
			{
				return collection.Count;
			}
			return pos - 1;
		}

		#endregion

	}
}
