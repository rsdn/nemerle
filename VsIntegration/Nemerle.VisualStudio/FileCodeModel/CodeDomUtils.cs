using System;
using System.CodeDom;

using EnvDTE;

namespace Nemerle.VisualStudio.FileCodeModel
{
	class CodeDomUtils
	{
		public static vsCMAccess GetCMAccess(MemberAttributes attributes)
		{
			switch (attributes)
			{
				case MemberAttributes.Public:           return vsCMAccess.vsCMAccessPublic;
				case MemberAttributes.Private:          return vsCMAccess.vsCMAccessPrivate;
				case MemberAttributes.Assembly:         return vsCMAccess.vsCMAccessProject;
				case MemberAttributes.Family:           return vsCMAccess.vsCMAccessProtected;
				case MemberAttributes.FamilyOrAssembly: return vsCMAccess.vsCMAccessProjectOrProtected;
			}

			return vsCMAccess.vsCMAccessPrivate;
		}
	}
}