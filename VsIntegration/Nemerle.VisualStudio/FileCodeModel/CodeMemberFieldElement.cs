using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using EnvDTE;

using CodeNamespace = EnvDTE.CodeNamespace;

namespace Nemerle.VisualStudio.FileCodeModel
{
	internal class CodeMemberFieldElement : CodeTypeMemberElement, CodeVariable
	{
		public CodeMemberFieldElement(
			DTE                 dte,
			ProjectItem         projectItem,
			CodeMemberField codeType,
			CodeElementBase     parent)
			: base(dte, projectItem, codeType, parent)
		{
		}

		public override vsCMElement Kind
		{
			get { return vsCMElement.vsCMElementVariable; }
		}

		#region CodeVariable Members

		public CodeAttribute AddAttribute(string Name, string Value, object Position)
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

		public object InitExpression
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

		public bool IsConstant
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

		public bool IsShared
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

		public override string get_Prototype(int Flags)
		{
			vsCMPrototype flags = (vsCMPrototype)Flags;			

			if((flags & vsCMPrototype.vsCMPrototypeType) != 0)
			{
				CodeMemberField cmf = member as CodeMemberField;

				if(cmf != null)
					return cmf.Name + " : " + cmf.Type.BaseType;
			}

			return "";
		}

		#endregion
	}
}
