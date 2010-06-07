using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Project;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace Nemerle.VisualStudio.Project.References
{
	[CLSCompliant(false), ComVisible(true)]
	public class NemerleReferenceNodeProperties : ReferenceNodeProperties
	{
		public NemerleReferenceNodeProperties(HierarchyNode node)
			: base(node)
		{
		}

		[LocDisplayName("HintPath")]
		[SRCategoryAttribute("Misc")]
		//[SRDescriptionAttribute("ssssssssssss")]
		[Browsable(true)]
		[AutomationBrowsable(true)]
		public string HintPath
		{
			get
			{
				var value = Node.ItemNode.Item.GetMetadata("HintPath");
				return value;
			}
		}
	}
}
