using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Project;
using System.Runtime.InteropServices;
using System.ComponentModel;

namespace Nemerle.VisualStudio.Project
{
	[CLSCompliant(false), ComVisible(true)]
	public class NemerleProjectReferencesProperties : ReferenceNodeProperties
	{
		string _className;

		#region ctors

		public NemerleProjectReferencesProperties(HierarchyNode node, string className)
			: base(node)
		{
			_className = className + "!";
		}

		#endregion

		#region overriden

		[LocDisplayName("HintPath")]
		[SRCategoryAttribute("Misc")]
		[SRDescriptionAttribute("ssssssssssss")]
		[Browsable(true)]
		[AutomationBrowsable(true)]
		public string HintPath
		{
			get
			{
				var value = Node.ItemNode.Item.GetMetadataValue("HintPath");
				return value;
			}
		}

		public override string FullPath
		{
			get
			{
				var path = ((NemerleProjectReferenceNode)Node).ReferencedProjectOutputPath;
				return path == null ? "<can't resolve path>" : path;
			}
		}

		public override string GetClassName()
		{
			return _className;
		}

		#endregion
	}
}
