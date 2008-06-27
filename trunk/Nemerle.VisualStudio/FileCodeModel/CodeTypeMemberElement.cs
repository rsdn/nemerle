using System;
using System.CodeDom;

using EnvDTE;
using System.Text;

namespace Nemerle.VisualStudio.FileCodeModel
{
	internal abstract class CodeTypeMemberElement : CodeElementBase
	{
		protected readonly CodeTypeMember member;

		public CodeTypeMemberElement(DTE dte, ProjectItem projectItem, CodeTypeMember member, CodeElementBase parent)
			: base(dte, projectItem, parent)
		{
			this.member = member;
		}

		public static CodeTypeMemberElement Create(
			DTE dte,
			ProjectItem projectItem,
			CodeTypeMember member,
			CodeElementBase parent)
		{
			CodeMemberField cmf = member as CodeMemberField;

			if(cmf != null)
				return new CodeMemberFieldElement(dte, projectItem, cmf, parent);

			return new CodeTypeFunctionMember(dte, projectItem, member, parent);
		}

		public override string Name
		{
			get { return member.Name; }
			set { member.Name = value; }
		}

		public vsCMAccess Access
		{
			get { return CodeDomUtils.GetCMAccess(member.Attributes & MemberAttributes.AccessMask); }
			set { throw new NotImplementedException(); }
		}

		public virtual string get_Prototype(int Flags)
		{
			return "";
		}

		public new object Parent { get { return base.Parent; } }
	}
}