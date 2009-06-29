using System;
using System.Text;
using System.Data;
using System.Data.Common;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VisualStudio.TeamSystem.Data.UnitTesting;
using Microsoft.VisualStudio.TeamSystem.Data.UnitTesting.Conditions;

namespace Nemerle.VisualStudio.LanguageService
{
	public enum ParseReason2
	{
		CheckRelocatedMember = 100,
		ProcessRegions,
		BuildTypeTree
	}
}
