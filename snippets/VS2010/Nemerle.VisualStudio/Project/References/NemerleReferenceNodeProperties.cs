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

		[SRCategoryAttribute(Microsoft.VisualStudio.Project.SR.Misc)]
		[LocDisplayName(Microsoft.VisualStudio.Project.SR.CopyToLocal)]
		[SRDescriptionAttribute(Microsoft.VisualStudio.Project.SR.CopyToLocalDescription)]
		public new bool CopyToLocal
		{
			get
			{
				string copyLocal = this.GetProperty(ProjectFileConstants.Private, "False");
				if (copyLocal == null || copyLocal.Length == 0)
					return false; // false by defaulte!
				return bool.Parse(copyLocal);
			}
			set
			{
				this.SetProperty(ProjectFileConstants.Private, value.ToString());
			}
		}

		[DisplayName("Hint Path")]
		[Description("Hint Path with all expended properties.")]
		[CategoryAttribute("Misc")]
		[Browsable(true)]
		[AutomationBrowsable(true)]
		public string HintPath
		{
			get
			{
				var data = Node.ItemNode.Item.GetMetadata("HintPath");
				return data == null ? "<No hint path available>" : data.EvaluatedValue;
			}
		}

		[DisplayName("Unevaluated Hint Path")]
		[Description("Hint Path without expending properties.")]
		[CategoryAttribute("Misc")]
		[Browsable(true)]
		[AutomationBrowsable(true)]
		public string UnevaluatedHintPath
		{
			get
			{
				var data = Node.ItemNode.Item.GetMetadata("HintPath");
				return data == null ? "<No hint path available>" : data.UnevaluatedValue;
			}
		}

		[DisplayName("Specific Version")]
		[Description("Indicates whether this reference is to a specific version of an assembly.")]
		[DefaultValue(false)]
		[Category("Misc")]
		public bool SpecificVersion
		{
			get
			{
				string property = GetProperty("SpecificVersion", null);
				var assm = (Microsoft.VisualStudio.Project.Automation.OAAssemblyReference)Node.Object;
				if (string.IsNullOrEmpty(property))
				{
					string property2 = GetProperty("Include", null);
					return property2.IndexOf("Version=", StringComparison.Ordinal) >= 0;
				}

				return bool.Parse(property);
			}

			set
			{
				var assemblyReferenceNode = (AssemblyReferenceNode)base.Node;

				if (value)
				{
					if (assemblyReferenceNode.ResolvedAssembly != null)
					{
						Node.ItemNode.Rename(assemblyReferenceNode.ResolvedAssembly.FullName);
						SetProperty("SpecificVersion", "True");
						return;
					}
				}
				else
				{
					if (assemblyReferenceNode.ResolvedAssembly != null)
					{
						Node.ItemNode.Rename(assemblyReferenceNode.ResolvedAssembly.Name);
						SetProperty("SpecificVersion", "False");
					}
				}
			}
		}
	}
}
