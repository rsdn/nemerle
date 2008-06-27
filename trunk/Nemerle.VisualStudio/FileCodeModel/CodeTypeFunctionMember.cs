using System;
using System.CodeDom;

using EnvDTE;

using Microsoft.VisualStudio;
using System.Text;

namespace Nemerle.VisualStudio.FileCodeModel
{
	internal class CodeTypeFunctionMember : CodeTypeMemberElement, CodeFunction, NativeMethods.IMethodXML
	{
		public CodeTypeFunctionMember(
			DTE             dte,
			ProjectItem     projectItem, 
			CodeTypeMember  member,
			CodeElementBase parent)
			: base(dte, projectItem, member, parent)
		{
		}

		public override vsCMElement Kind
		{
			get { return vsCMElement.vsCMElementFunction; }
		}

		#region CodeFunction
		
		public CodeParameter AddParameter(string Name, object Type, object Position)
		{
			throw new NotImplementedException();
		}

		public CodeAttribute AddAttribute(string Name, string Value, object Position)
		{
			throw new NotImplementedException();
		}

		public void RemoveParameter(object Element)
		{
			throw new NotImplementedException();
		}

		public override string get_Prototype(int Flags)
		{
			CodeMemberMethod cmm = member as CodeMemberMethod;

			if(cmm == null)
				return cmm.Name;

			vsCMPrototype flags = (vsCMPrototype)Flags;
			
			StringBuilder sb = new StringBuilder(Name);

			if((flags & vsCMPrototype.vsCMPrototypeParamTypes) != 0)
			{
				sb.Append(" (");

				bool first = true;

				foreach(CodeParameterDeclarationExpression cpde in cmm.Parameters)
				{
					first = false;
					sb.Append(cpde.Type.BaseType);
					sb.Append(", ");
				}

				if(!first)
				{
					sb.Length = sb.Length - 2;
				}

				sb.Append(")");
			}

			if((flags & vsCMPrototype.vsCMPrototypeType) != 0)
			{
				sb.Append(" : ");
				sb.Append(cmm.ReturnType.BaseType);
			}

			return sb.ToString();
		}		

		public vsCMFunction FunctionKind
		{
			get { throw new NotImplementedException(); }
		}

		public CodeTypeRef Type
		{
			get 
			{			
				return VoidCodeTypeRef.Value; 			
			}
			set { throw new NotImplementedException(); }
		}

		public CodeElements Parameters
		{
			get { return new NemerleCodeElements(DTE, ProjectItem, this); }
		}
		
		bool CodeFunction.IsOverloaded
		{
			get { return (member.Attributes & MemberAttributes.Override) == MemberAttributes.Override; }
		}
		
		bool CodeFunction.IsShared
		{
			get { return (member.Attributes & MemberAttributes.Static) == MemberAttributes.Static; }
			set { throw new NotImplementedException(); }
		}
		
		bool CodeFunction.MustImplement
		{
			get { return (member.Attributes & MemberAttributes.Static) == MemberAttributes.Static; }
			set { throw new NotImplementedException(); }
		}
		
		CodeElements CodeFunction.Overloads
		{
			get { throw new NotImplementedException(); }
		}
		
		CodeElements CodeFunction.Attributes
		{
			get { throw new NotImplementedException(); }
		}
		
		string CodeFunction.DocComment
		{
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}
		
		string CodeFunction.Comment
		{
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}
		
		bool CodeFunction.CanOverride
		{
			get { return (member.Attributes & MemberAttributes.VTableMask) == MemberAttributes.VTableMask; }
			set { throw new NotImplementedException(); }
		}

		#endregion

		#region IMethodXML
		
		void NativeMethods.IMethodXML.GetXML(ref string xml)
		{
			xml = string.Empty;
		}

		int NativeMethods.IMethodXML.SetXML(string xml)
		{
			return NativeMethods.S_FALSE;
		}

		int NativeMethods.IMethodXML.GetBodyPoint(out object bodyPoint)
		{
			bodyPoint = null;
			return NativeMethods.S_FALSE;
		}
		
		#endregion
	}
}