using System;
using System.Runtime.InteropServices;
using System.Xml.Linq;
using Microsoft.VisualStudio.Project;

using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio;
using System.IO;
using Nemerle.VisualStudio.GUI;
using System.Windows.Forms;

namespace Nemerle.VisualStudio.Project
{
	[Guid(NemerleConstants.ProjectFactoryGuidString)]
	public class NemerleProjectFactory : ProjectFactory, IVsProjectUpgradeViaFactory
	{
		public NemerleProjectFactory(NemerlePackage package)
			: base(package)
		{
		}

		protected override ProjectNode CreateProject()
		{
			ProjectNode		 project			= new NemerleProjectNode(Package);
			IOleServiceProvider oleServiceProvider = Package.GetService<IOleServiceProvider>();

			project.SetSite(oleServiceProvider);

			return project;
		}

		public new NemerlePackage Package
		{
			get { return (NemerlePackage)base.Package; }
		}

		#region IVsProjectUpgradeViaFactory Members

		public int GetSccInfo(string bstrProjectFileName, out string pbstrSccProjectName, out string pbstrSccAuxPath, out string pbstrSccLocalPath, out string pbstrProvider)
		{
			throw new NotImplementedException();
		}

		const string ToolsVersion = "4.0";
		const string NemerlePath = @"$(ProgramFiles)\Nemerle\Net-4.0";
		const string TargetFrameworkVersion = "v4.0";

		public int UpgradeProject(string sourceProjectFilePath, uint fUpgradeFlag, string bstrCopyLocation, out string upgradedFullyQualifiedFileName, IVsUpgradeLogger pLogger, out int pUpgradeRequired, out Guid pguidNewProjectFactory)
		{
			upgradedFullyQualifiedFileName = null;
			pUpgradeRequired = 0;
			pguidNewProjectFactory = Guid.Empty;

			var destProjectFilePath = sourceProjectFilePath;
			var backupedProject = Path.Combine(bstrCopyLocation, Path.GetFileName(sourceProjectFilePath));
			var projectName = Path.GetFileName(sourceProjectFilePath);

			if (bstrCopyLocation != null && Directory.Exists(bstrCopyLocation))
			{
				var dlg = new PromptProjectRenameForm(Path.GetFileNameWithoutExtension(projectName));
				var result = dlg.ShowDialog();

				if (result == DialogResult.Yes)
				{
					destProjectFilePath = Path.Combine(Path.GetDirectoryName(sourceProjectFilePath), dlg.ProjectName + Path.GetExtension(projectName));
					pUpgradeRequired = 1;
					if (string.Equals(sourceProjectFilePath, destProjectFilePath, StringComparison.InvariantCultureIgnoreCase))
						throw new ApplicationException("Can't rename project to itself name.");

					upgradedFullyQualifiedFileName = destProjectFilePath;
					File.Copy(sourceProjectFilePath, destProjectFilePath);
					//File successfully backed up as
					pLogger.LogMessage(0, projectName, projectName, "The project file has been renamed to '"
						+ Path.GetFileName(destProjectFilePath) + "' ('" + destProjectFilePath 
						+ "'). The old project file remain unchanged.");
				}
				else
				{
					File.Copy(sourceProjectFilePath, backupedProject);
					pLogger.LogMessage(0, projectName, projectName, backupedProject);
				}
			}

			var projectData = new ProjectUpgradeHelper(sourceProjectFilePath);

			projectData.ToolsVersion.Value           = ToolsVersion;
			projectData.NemerleProperty.Value        = NemerlePath;
			projectData.TargetFrameworkVersion.Value = TargetFrameworkVersion;

			projectData.NemerleProperty.Document.Save(destProjectFilePath);

			pLogger.LogMessage((uint)__VSUL_ERRORLEVEL.VSUL_STATUSMSG, projectName, projectName, "Project converted successfully");
			
			return VSConstants.S_OK;
		}

		public int UpgradeProject_CheckOnly(string projectFileName, IVsUpgradeLogger pLogger, out int pUpgradeRequired, out Guid pguidNewProjectFactory, out uint pUpgradeProjectCapabilityFlags)
		{
			pUpgradeRequired = 1;
			pUpgradeProjectCapabilityFlags = (uint)(__VSPPROJECTUPGRADEVIAFACTORYFLAGS.PUVFF_BACKUPSUPPORTED | __VSPPROJECTUPGRADEVIAFACTORYFLAGS.PUVFF_COPYBACKUP);
			pguidNewProjectFactory = Guid.Empty;

			var projectData = new ProjectUpgradeHelper(projectFileName);

			if (projectData.ToolsVersion == null || float.Parse(projectData.ToolsVersion.Value) < 4.0)
				return VSConstants.S_OK;

			if (!Utils.Eq(projectData.NemerleProperty.Value, NemerlePath))
				return VSConstants.S_OK;

			if (!Utils.Eq(projectData.TargetFrameworkVersion.Value, TargetFrameworkVersion))
				return VSConstants.S_OK;

			pUpgradeRequired = 0;

			return VSConstants.S_OK;
		}

		#endregion
	}
}
