using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.Project
{
	class NemerleOAReferenceFolderItem : OAReferenceFolderItem
	{
		#region ctors

		public NemerleOAReferenceFolderItem(OAProject project, ReferenceContainerNode node)
			: base(project, node)
		{
		}

		#endregion

		#region overridden methods

		/// <summary>
		/// Gets a GUID string indicating the kind or type of the object.
		/// </summary>
		public override string Kind
		{
			get { return Utils.GetTypeGuidAsString(Node); }
		}

		/// <summary>
		/// Returns the project items collection of all the references defined for this project.
		/// </summary>
		public override EnvDTE.ProjectItems ProjectItems
		{
			get
			{
				return new OANavigableProjectItems(Project, GetListOfProjectItems(), Node);
			}
		}

		private IList<EnvDTE.ProjectItem> GetListOfProjectItems()
		{
			var list = new List<EnvDTE.ProjectItem>();
			for (var child = this.Node.FirstChild; child != null; child = child.NextSibling)
			{
				var refNode = child as ReferenceNode;
				if (refNode != null)
					list.Add(new NemerleOAReferenceItem(this.Project, refNode));
			}

			return list;
		}

		#endregion
	}
}
