using System;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project;
using System.IO;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio;
using System.Diagnostics;
using System.Reflection;
using System.Linq;

namespace Nemerle.VisualStudio.Project
{
	[CLSCompliant(false), ComVisible(true)]
	public class NemerleMacroProjectReferenceNode : NemerleProjectReferenceNode
	{
		#region ctors

		/// <summary>
		/// Constructor for the ReferenceNode. It is called when the project is reloaded, when the project element representing the refernce exists. 
		/// </summary>
		public NemerleMacroProjectReferenceNode(ProjectNode root, ProjectElement element)
			: base(root, element)
		{
		}

		public NemerleMacroProjectReferenceNode(ProjectNode root, string referencedProjectName, string projectPath, string projectReference)
			: base(root, referencedProjectName, projectPath, projectReference)
		{
		}

		#endregion

		protected override NodeProperties CreatePropertiesObject()
		{
			return new NemerleProjectReferencesProperties(this, "Macro Project Reference Properties");
		}

		/// <summary>
		/// Links a reference node to the project file.
		/// </summary>
		protected override void BindReferenceData()
		{
			base.BindReferenceData();
			this.ItemNode.ItemName = NemerleConstants.MacroProjectReference;
			// Предотвращаем копирование макро-сборки в каталог собираемого проекта
			ItemNode.SetMetadata(ProjectFileConstants.Private, "False");
		}

		void VerifyMacroAssembly()
		{
			var path = ProjectInfo.GetAssemblyReferencesString(this);

			if (!File.Exists(path))
			{
				//var message = "A macro project reference can't be verified as the project has not been compiled yet.";
				//OLEMSGICON icon = OLEMSGICON.OLEMSGICON_QUERY;
				//OLEMSGBUTTON buttons = OLEMSGBUTTON.OLEMSGBUTTON_YESNO;
				//OLEMSGDEFBUTTON defaultButton = OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_SECOND;
				//var res = VsShellUtilities.ShowMessageBox(ProjectMgr.Site,
				//  message, NemerleConstants.ProductName, icon, buttons, defaultButton);

				//if (res == VSConstants.S_OK)
				//  return;

				//throw new FileNotFoundException("Macro assembly not found", path);
				return;
			}

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
			if (this.ProjectMgr == null)
				return;

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

			this.ProjectMgr.AddBuildDependency(this.buildDependency);

			return;
		}

		public override object GetIconHandle(bool open)
		{
			//TODO: Shou special icon
			return base.GetIconHandle(open);
		}
	}
}
