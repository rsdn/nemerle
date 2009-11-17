using System;
using System.IO;
using System.ComponentModel;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project
{
	[ComVisible(true)]
	[CLSCompliant(false)]
	[Guid(NemerleConstants.FileNodePropertiesGuidString)]
	public class NemerleFileNodeProperties : SingleFileGeneratorNodeProperties
	{
		public NemerleFileNodeProperties(HierarchyNode node)
			: base(node)
		{
		}

    public new string Extension
    {
			get 
			{
				//throw new NotImplementedException();
				//return ".Designer.n";
				return Path.GetExtension(Node.Url);
			}
    }

		[Browsable(false)]
		public string Url
		{
			get { return "file:///" + Node.Url; }
		}

		[Browsable(false)]
		public string SubType
		{
			get { return ((NemerleFileNode)Node).SubType;  }
			set { ((NemerleFileNode)Node).SubType = value; }
		}

		[SRCategoryAttribute   (Microsoft.VisualStudio.Project.SR.Advanced)]
		[LocDisplayName        (Microsoft.VisualStudio.Project.SR.BuildAction)]
		[SRDescriptionAttribute(Microsoft.VisualStudio.Project.SR.BuildActionDescription)]
		public virtual NemerleBuildAction NemerleBuildAction
		{
			get
			{
				string value = Node.ItemNode.ItemName;

				if (string.IsNullOrEmpty(value))
					return NemerleBuildAction.None;

				return (NemerleBuildAction)Enum.Parse(typeof(NemerleBuildAction), value);
			}

			set
			{
				Node.ItemNode.ItemName = value.ToString();
			}
		}

		[Browsable(false)]
		public override BuildAction BuildAction
		{
			get
			{
				switch (NemerleBuildAction)
				{
					case NemerleBuildAction.ApplicationDefinition:
					case NemerleBuildAction.Page:
					case NemerleBuildAction.Resource:
						return BuildAction.Compile;

					default:
						return (BuildAction)Enum.Parse(typeof(BuildAction), NemerleBuildAction.ToString());
				}
			}

			set
			{
				this.NemerleBuildAction = (NemerleBuildAction)Enum.Parse(typeof(NemerleBuildAction), value.ToString());
			}
		}
	}
}