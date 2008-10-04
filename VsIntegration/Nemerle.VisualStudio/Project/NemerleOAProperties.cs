using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Project;
using System.Runtime.InteropServices;

namespace Nemerle.VisualStudio.Project
{
	[CLSCompliant(false), ComVisible(true)]
	public class NemerleOAProperties : OAProperties
	{
		public NemerleOAProperties(NodeProperties target)
			: base(target)
		{
		}
	}
}
