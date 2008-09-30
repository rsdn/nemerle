using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;

namespace Nemerle.VisualStudio.Project
{
	class NemerleDependentFileNode : DependentFileNode
	{
		#region ctor

		/// <summary>
		/// Constructor for the NemerleDependentFileNode
		/// </summary>
		/// <param name="root">Root of the hierarchy</param>
		/// <param name="e">Associated project element</param>
		public NemerleDependentFileNode(ProjectNode root, ProjectElement e)
			: base(root, e)
		{
			HasParentNodeNameRelation = false;
		}

		#endregion

		// Called since the FileNode.ImageIndex returns -1 by default.
		//
		public override object GetIconHandle(bool open)
		{
			if (FileName.EndsWith(NemerleConstants.FileExtension, StringComparison.InvariantCultureIgnoreCase))
				return PackageUtilities.GetIntPointerFromImage(
					NemerleProjectNode.NemerleImageList.Images[(int)NemerleConstants.ImageListIndex.NemerleSource]);

			return base.GetIconHandle(open);
		}

		#region CodeDomProvider

		NemerleFileNodeCodeDomProvider _codeDomProvider;

		protected internal NemerleFileNodeCodeDomProvider CodeDomProvider
		{
			get
			{
				if (_codeDomProvider == null)
					_codeDomProvider = new NemerleFileNodeCodeDomProvider(this);

				return _codeDomProvider;
			}
		}

		#endregion
	}
}
