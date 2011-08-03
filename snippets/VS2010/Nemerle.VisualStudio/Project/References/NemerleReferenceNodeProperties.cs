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
                    return false;
                return bool.Parse(copyLocal);
            }
            set
            {
                this.SetProperty(ProjectFileConstants.Private, value.ToString());
            }
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
				var value = Node.ItemNode.Item.GetMetadataValue("HintPath");
				return value;
			}
		}

    [SRDescription("SpecificVersionDescription"), SRCategory("Misc"), LocDisplayName("SpecificVersion")]
    public bool SpecificVersion
    {
      get
      {
        string property = base.GetProperty("SpecificVersion", null);
        if (string.IsNullOrEmpty(property))
        {
          string property2 = base.GetProperty("Include", null);
          return property2.IndexOf("Version=", StringComparison.Ordinal) != -1;
        }
        return bool.Parse(property);
      }
      set
      {
       
        if (value)
        {
          AssemblyReferenceNode assemblyReferenceNode = (AssemblyReferenceNode)base.Node;
          if (assemblyReferenceNode.ResolvedAssembly != null)
          {
            base.Node.ItemNode.Rename(assemblyReferenceNode.ResolvedAssembly.FullName);
            base.SetProperty("SpecificVersion", "True");
            return;
          }
        }
        else
        {
          AssemblyReferenceNode assemblyReferenceNode = (AssemblyReferenceNode)base.Node;
          if (assemblyReferenceNode.ResolvedAssembly != null)
          {
            base.Node.ItemNode.Rename(assemblyReferenceNode.ResolvedAssembly.Name);
            base.SetProperty("SpecificVersion", "False");
          }
        }
      }
    }
	}
}
