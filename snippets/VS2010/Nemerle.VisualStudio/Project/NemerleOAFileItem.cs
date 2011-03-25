using System;
using System.Runtime.InteropServices;

using EnvDTE;

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;

using Nemerle.VisualStudio.FileCodeModel;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Add support for automation on n files.
	/// </summary>
	[Guid(NemerleConstants.OAFileItemGuidString)]
	public class NemerleOAFileItem : OAFileItem
	{
		public NemerleOAFileItem(OAProject project, FileNode node)
			: base(project, node)
		{
		}

		public override EnvDTE.Properties Properties
		{
			get
			{
				if (Node.NodeProperties == null)
					return null;

				return new NemerleOAProperties(Node.NodeProperties);
			}
		}

		/// <summary>
		/// Gets the FileCodeModel object for the project item.
		/// </summary>
		public override EnvDTE.FileCodeModel FileCodeModel
		{
			get { return new NemerleFileCodeModel(this, Node); }
		}

		public override Window Open(string viewKind)
		{
			if (string.Compare(viewKind, Constants.vsViewKindPrimary) == 0)
			{
				// Get the subtype and decide the viewkind based on the result.
				//
				if (Node.HasDesigner)
					return base.Open(Constants.vsViewKindDesigner);
			}

			return base.Open(viewKind);
		}
	}

}
