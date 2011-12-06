using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio;
using System;
using Microsoft.VisualStudio.Shell.Interop;
using System.IO;
using Nemerle.VisualStudio.Project.References;

namespace Nemerle.VisualStudio.Project
{
	class NemerleAssemblyReferenceNode : AssemblyReferenceNode
	{
    readonly NemerleProjectNode _projectNode;
		public NemerleAssemblyReferenceNode(ProjectNode root, ProjectElement e)
			: base(root, e)
		{
      _projectNode = root as NemerleProjectNode;
      
		}

		protected override NodeProperties CreatePropertiesObject()
		{
      
			return new NemerleReferenceNodeProperties(this, _projectNode.GetAutomationObject() as NemerleOAProject);
      
		}

		/// <summary>
		/// Constructor for the AssemblyReferenceNode
		/// </summary>
		public NemerleAssemblyReferenceNode(ProjectNode root, string assemblyPath)
			: base(root, assemblyPath)
		{
      
			_projectNode = root as NemerleProjectNode;
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

    public override void Remove(bool removeFromStorage)
    {
      base.Remove(removeFromStorage);
      (_projectNode.GetAutomationObject() as NemerleOAProject).PersistProjectFile();
    }
		protected override void BindReferenceData()
		{
			base.BindReferenceData();

			string path = null;
			var fullFilePath = Path.GetFullPath(Url);
			// ������� ���������� �� �������� �� ���� ����� ��������� � ���������� ����� ��������� Nemerle...
			var dir = Path.GetFullPath(Path.GetDirectoryName(fullFilePath));
			var envVar = Environment.GetEnvironmentVariable("Nemerle");

			if (!string.IsNullOrEmpty(envVar))
			{
				envVar = Path.GetFullPath(envVar);
				if (string.Equals(dir, envVar, StringComparison.InvariantCultureIgnoreCase))
					path = Path.Combine("$(Nemerle)\\", Path.GetFileName(fullFilePath));
			}

			if (path == null)
			{
				// ������ HintPath ������������� �����... 
				// ��� �������� ���������� ������� � ������ �� ������ ��� ���������.

				var fullProjectPath = Path.GetFullPath(ProjectMgr.ProjectFolder);
				path = Utils.GetRelativePath(fullProjectPath, fullFilePath);
			}

			// Set a default HintPath for msbuild to be able to resolve the reference.
			ItemNode.SetMetadata(ProjectFileConstants.HintPath, path);
		}

		public override string ToString()
		{
			return GetType().Name + ": " + Caption + " (" + Url + ")";
		}
	}
}
