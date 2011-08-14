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
			pUpgradeRequired = 1;
			pguidNewProjectFactory = Guid.Empty;
			upgradedFullyQualifiedFileName = null;
			var projectName = Path.GetFileNameWithoutExtension(sourceProjectFilePath);

			try
			{
				var destProjectFilePath = sourceProjectFilePath;
				var backupedProject = string.IsNullOrEmpty(bstrCopyLocation)
					? null
					: Path.Combine(bstrCopyLocation, Path.GetFileName(sourceProjectFilePath));
				var projectFileName = Path.GetFileName(sourceProjectFilePath);

				BackupProjectForUpgrade(sourceProjectFilePath, pLogger, ref destProjectFilePath, backupedProject, projectName);

				upgradedFullyQualifiedFileName = destProjectFilePath;

				UpgradeProject(sourceProjectFilePath, destProjectFilePath);

				pLogger.LogMessage((uint)__VSUL_ERRORLEVEL.VSUL_INFORMATIONAL, projectName, sourceProjectFilePath, "Project converted successfully");
				// Tell to VS which the project converted successfull. It's a magic! :)
				pLogger.LogMessage((uint)__VSUL_ERRORLEVEL.VSUL_STATUSMSG, projectName, sourceProjectFilePath,
					// "Converted" should NOT be localized - it is referenced in the XSLT used to display the UpgradeReport
					"Converted");

				return VSConstants.S_OK;
			}
			catch (Exception ex)
			{
				pLogger.LogMessage((uint)__VSUL_ERRORLEVEL.VSUL_INFORMATIONAL, projectName, sourceProjectFilePath, "Error during project convertion: " + ex.Message);
				return VSConstants.E_FAIL;
			}
		}

		private static void UpgradeProject(string sourceProjectFilePath, string destProjectFilePath)
		{
			var projectData = new ProjectUpgradeHelper(sourceProjectFilePath);

			projectData.ToolsVersion.Value = ToolsVersion;
			projectData.NemerleProperty.Value = NemerlePath;
			projectData.TargetFrameworkVersion.Value = TargetFrameworkVersion;

			projectData.NemerleProperty.Document.Save(destProjectFilePath);
		}

		private static void BackupProjectForUpgrade(string sourceProjectFilePath, IVsUpgradeLogger pLogger, ref string destProjectFilePath, string backupedProject, string projectName)
		{
			var dlg = new PromptProjectRenameForm(projectName);
			var result = dlg.ShowDialog();

			if (result == DialogResult.Yes)
			{
				destProjectFilePath = Path.Combine(Path.GetDirectoryName(sourceProjectFilePath), dlg.ProjectName + Path.GetExtension(sourceProjectFilePath));
				if (string.Equals(sourceProjectFilePath, destProjectFilePath, StringComparison.InvariantCultureIgnoreCase))
					throw new ApplicationException("Can't rename project to itself name.");

				File.Copy(sourceProjectFilePath, destProjectFilePath);
				//File successfully backed up as
				pLogger.LogMessage(0, projectName, sourceProjectFilePath, "The project file has been renamed to '"
					+ Path.GetFileName(destProjectFilePath) + "' ('" + destProjectFilePath
					+ "'). The old project file remain unchanged.");
			}
			else if (!string.IsNullOrEmpty(backupedProject))
			{
				File.Copy(sourceProjectFilePath, backupedProject);
				pLogger.LogMessage(0, projectName, sourceProjectFilePath, "File successfully backed up as "
					+ backupedProject);
			}
		}

		public int UpgradeProject_CheckOnly(string projectFileName, IVsUpgradeLogger pLogger, out int pUpgradeRequired, out Guid pguidNewProjectFactory, out uint pUpgradeProjectCapabilityFlags)
		{
			pUpgradeRequired = 1;
			pUpgradeProjectCapabilityFlags = (uint)(__VSPPROJECTUPGRADEVIAFACTORYFLAGS.PUVFF_BACKUPSUPPORTED | __VSPPROJECTUPGRADEVIAFACTORYFLAGS.PUVFF_COPYBACKUP);
			pguidNewProjectFactory = Guid.Empty;

			var projectData = new ProjectUpgradeHelper(projectFileName);

			if (projectData.ToolsVersion == null || ParseVersion(projectData.ToolsVersion.Value).Major < 4)
				return VSConstants.S_OK;

			if (!Utils.Eq(projectData.NemerleProperty.Value, NemerlePath))
				return VSConstants.S_OK;

			if (!Utils.Eq(projectData.TargetFrameworkVersion.Value, TargetFrameworkVersion))
				return VSConstants.S_OK;

			pUpgradeRequired = 0;

			return VSConstants.S_OK;
		}

		private static Version ParseVersion(string value)
		{
			try { return Version.Parse(value); }
			catch (FormatException) { return new Version(0.0); }
		}

		#endregion
	}
}
