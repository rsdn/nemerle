using System;
using System.IO;
using System.ComponentModel;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Project;
using Nemerle.VisualStudio.Helpers;

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

		// Студия запрашивает значение параметр Url используя разный стиль наименования (URL или Url)
		[Browsable(false)]
		public string URL
		{
			get { return this.Url; }
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
				var oldValue = NemerleBuildAction;
				Node.ItemNode.ItemName = value.ToString();
				var project = ProjectInfo.FindProject(this.Node.ProjectMgr);
				if (project != null)
					project.FileBuildActionPropertyChanged(this.Node, oldValue, value);
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

		[SRCategoryAttribute(Microsoft.VisualStudio.Project.SR.Advanced)]
		[LocDisplayName("Copy to Output Directory")]
		[SRDescriptionAttribute("CopyToOutputDirectory...")]
		[DefaultValue(CopyToOutputDirectory.DoNotCopy)]
		public virtual CopyToOutputDirectory CopyToOutputDirectory
		{
			get
			{
				string value = this.Node.ItemNode.GetMetadata("CopyToOutputDirectory");

				if (StringUtils.IsNullOrWhiteSpace(value))
					return CopyToOutputDirectory.DoNotCopy;

				return (CopyToOutputDirectory)Enum.Parse(typeof(CopyToOutputDirectory), value);
			}
			set
			{
				if (value == CopyToOutputDirectory.DoNotCopy)
				{
					this.Node.ItemNode.SetMetadata("CopyToOutputDirectory", null);
					return;
				}

				this.Node.ItemNode.SetMetadata("CopyToOutputDirectory", value.ToString());
			}
		}
	}
}