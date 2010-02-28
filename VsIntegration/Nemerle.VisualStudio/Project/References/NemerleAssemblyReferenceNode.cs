using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio;
using System;
using Microsoft.VisualStudio.Shell.Interop;
using System.IO;

namespace Nemerle.VisualStudio.Project
{
	class NemerleAssemblyReferenceNode : AssemblyReferenceNode
	{
		public NemerleAssemblyReferenceNode(ProjectNode root, ProjectElement e)
			: base(root, e)
		{
		}

		/// <summary>
		/// Constructor for the AssemblyReferenceNode
		/// </summary>
		public NemerleAssemblyReferenceNode(ProjectNode root, string assemblyPath)
			: base(root, assemblyPath)
		{
			// AssemblyReferenceNode is useless without 'resolvedAssemblyName' field set.
			// The only way to set the 'AssemblyReferenceNode.resolvedAssemblyName' field
			// is a call to ResolveReference(), wich is redundant, since the assemly
			// was loaded by its path. ;)
			//
			ResolveReference();
		}

		/// <summary>
		/// Get a guid property
		/// </summary>
		/// <param name="propid">property id for the guid property requested</param>
		/// <param name="guid">the requested guid</param>
		/// <returns>S_OK if succeded</returns>
		public override int GetGuidProperty(int propid, out Guid guid)
		{
			guid = Guid.Empty;
			if (propid == (int)__VSHPROPID.VSHPROPID_TypeGuid)
			{
				guid = this.ItemTypeGuid;
				return VSConstants.S_OK;
			}
      
			return base.GetGuidProperty(propid, out guid);
		}

    protected override void BindReferenceData()
    {
      base.BindReferenceData();

      // Делаем HintPath относительным путем... 
      // Это позволит переносить проекты с машины на машину без изменений.

      var fullFilePath = Path.GetFullPath(Url);
      var fullProjectPath = Path.GetFullPath(ProjectMgr.ProjectFolder);
      var relativePath = Utils.GetRelativePath(fullProjectPath, fullFilePath);

      // Set a default HintPath for msbuild to be able to resolve the reference.
      ItemNode.SetMetadata(ProjectFileConstants.HintPath, relativePath);
    }

		public override string ToString()
		{
			return GetType().Name + ": " + Caption + " (" + Url + ")";
		}
	}
}
