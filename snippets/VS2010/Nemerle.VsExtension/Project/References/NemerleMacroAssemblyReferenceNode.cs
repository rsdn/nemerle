using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio;
using System;
using Microsoft.VisualStudio.Shell.Interop;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;

namespace Nemerle.VisualStudio.Project
{
	class NemerleMacroAssemblyReferenceNode : NemerleAssemblyReferenceNode
	{
		public NemerleMacroAssemblyReferenceNode(ProjectNode root, ProjectElement e)
			: base(root, e)
		{
		}

		/// <summary>
		/// Constructor for the AssemblyReferenceNode
		/// </summary>
		public NemerleMacroAssemblyReferenceNode(ProjectNode root, string assemblyPath)
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
		/// Links a reference node to the project and hierarchy.
		/// </summary>
		protected override void BindReferenceData()
		{
			Debug.Assert(AssemblyName != null, "The AssemblyName field has not been initialized");

			// If the item has not been set correctly like in case of a new reference added it now.
			// The constructor for the AssemblyReference node will create a default project item. In that case the Item is null.
			// We need to specify here the correct project element. 
			if (ItemNode == null || ItemNode.Item == null)
				ItemNode = new ProjectElement(ProjectMgr, AssemblyName.FullName, NemerleConstants.MacroReference);

			// Set the basic information we know about
			ItemNode.SetMetadata(ProjectFileConstants.Name, AssemblyName.Name);
			ItemNode.SetMetadata(ProjectFileConstants.AssemblyName, Path.GetFileName(Url));

			var fullFilePath = Path.GetFullPath(Url);
			var fullProjectPath = Path.GetFullPath(ProjectMgr.ProjectFolder);
			var relativePath = Utils.GetRelativePath(fullProjectPath, fullFilePath);

			// Set a default HintPath for msbuild to be able to resolve the reference.
			ItemNode.SetMetadata(ProjectFileConstants.HintPath, relativePath);

			// ѕредотвращаем копирование макро-сборки в каталог собираемого проекта
			ItemNode.SetMetadata(ProjectFileConstants.Private, "False");
		}

		void VerifyMacroAssembly()
		{
			var path = ProjectInfo.GetAssemblyReferencesString(this);

			if (!File.Exists(path))
				throw new FileNotFoundException("Macro assembly not found", path);

			var assembly = Assembly.Load(File.ReadAllBytes(path));
			var macroDefs = assembly.GetCustomAttributes(false).Where(t => t.GetType().FullName == "Nemerle.Internal.ContainsMacroAttribute");
			if (macroDefs.Count() == 0)
			{
				var operDefs = assembly.GetCustomAttributes(false).Where(t => t.GetType().FullName == "Nemerle.Internal.OperatorAttribute");

				if (operDefs.Count() == 0)
					throw new InvalidOperationException("This assembly not contains macros.");
			}
		}

		/// <summary>
		/// Links a reference node to the project and hierarchy.
		/// </summary>
		public override void AddReference()
		{
			VerifyMacroAssembly();

			ReferenceContainerNode referencesFolder = (ReferenceContainerNode)((NemerleProjectNode)this.ProjectMgr).GetMacroReferenceContainer();
			Debug.Assert(referencesFolder != null, "Could not find the References node");

			CannotAddReferenceErrorMessage referenceErrorMessageHandler = null;

			if (!this.CanAddReference(out referenceErrorMessageHandler))
			{
				if (referenceErrorMessageHandler != null)
					referenceErrorMessageHandler.DynamicInvoke(new object[] { });

				return;
			}

			// Link the node to the project file.
			this.BindReferenceData();

			// At this point force the item to be refreshed
			this.ItemNode.RefreshProperties();

			referencesFolder.AddChild(this);

			return;
		}
	}
}
