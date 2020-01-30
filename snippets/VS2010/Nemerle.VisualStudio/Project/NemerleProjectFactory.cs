using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using System.Xml.Linq;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;

using Nemerle.VisualStudio.GUI;

using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;
using System.Collections.Generic;

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
            ProjectNode         project            = new NemerleProjectNode(Package);
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

                UpgradeProject(sourceProjectFilePath, destProjectFilePath, pLogger, projectName);

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

        static bool IsNeedUpdateNemerleProperty(XElement nemerleProperty)
        {
            var value = nemerleProperty.Value;

            if (string.IsNullOrEmpty(value))
                return true;

            if (NetFrameworkProjectConstants.OldNemerlePropertyValues.Contains(value))
                return true;

            return false;
        }

        static bool IsNeedUpdateNemerleBinPathRootProperty(XElement nemerleProperty)
        {
            var value = nemerleProperty.Value;

            if (string.IsNullOrEmpty(value))
                return true;

            if (nemerleProperty.Attribute("Condition") == null)
                return true;

            return false;
        }

        private static void UpgradeProject(string sourceProjectFilePath, string destProjectFilePath, IVsUpgradeLogger pLogger, string projectName)
        {
            var projectData = new ProjectUpgradeHelper(sourceProjectFilePath);

            if (IsNeedUpdateNemerleBinPathRootProperty(projectData.NemerleBinPathRoot))
            {
                projectData.NemerleBinPathRoot.Value = NetFrameworkProjectConstants.NemerleBinPathRoot;
                projectData.NemerleBinPathRoot.SetAttributeValue("Condition", " '$(NemerleBinPathRoot)' == '' ");
            }
            else if (!Utils.Eq(projectData.NemerleBinPathRoot.Value, NetFrameworkProjectConstants.NemerleBinPathRoot))
                pLogger.LogMessage((uint)__VSUL_ERRORLEVEL.VSUL_WARNING, projectName, sourceProjectFilePath, "The NemerleBinPathRoot property changed by user. You must update it manually.");

            projectData.NemerleVersion.Value = NetFrameworkProjectConstants.NemerleVersion;

            if (IsNeedUpdateNemerleProperty(projectData.NemerleProperty))
                projectData.NemerleProperty.Value = NetFrameworkProjectConstants.NemerleProperty;
            else if (!Utils.Eq(projectData.NemerleProperty.Value, NetFrameworkProjectConstants.NemerleProperty))
                pLogger.LogMessage((uint)__VSUL_ERRORLEVEL.VSUL_WARNING, projectName, sourceProjectFilePath, "The Nemerle property changed by user. You must update it manually.");

            projectData.NemerleProperty.Document.Save(destProjectFilePath);
        }

        private static bool IsNeedUpdateNemerleVersion(ProjectUpgradeHelper projectData)
        {
            return !NetFrameworkProjectConstants.ValidNemerleVersions.Contains(projectData.NemerleVersion.Value);
        }

        private static void BackupProjectForUpgrade(string sourceProjectFilePath, IVsUpgradeLogger pLogger, ref string destProjectFilePath, string backupedProject, string projectName)
        {
            var dlg = new PromptProjectRenameForm(projectName);
            Form parent = null;
            foreach (Form item in Application.OpenForms)
                if (item.GetType().Name == "UpgradeWizard_Dialog")
                {
                    parent = item;
                    break;
                }

            var result = dlg.ShowDialog(parent);

            if (result == DialogResult.Yes)
            {
                destProjectFilePath = Path.Combine(Path.GetDirectoryName(sourceProjectFilePath), dlg.ProjectName + Path.GetExtension(sourceProjectFilePath));
                if (Utils.Eq(sourceProjectFilePath, destProjectFilePath))
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

        static bool NemerlePropertyHasCondition(XElement property)
        {
            var condition = property.Attribute("Condition");

            if (condition == null)
                return false;

            var rxCondition = new Regex(@"\s*'\$\(Nemerle\)'\s*==\s*''\s*");

            return rxCondition.IsMatch(condition.Value);
        }

        public int UpgradeProject_CheckOnly(string projectFileName, IVsUpgradeLogger pLogger, out int pUpgradeRequired, out Guid pguidNewProjectFactory, out uint pUpgradeProjectCapabilityFlags)
        {
            pUpgradeRequired = 1;
            pUpgradeProjectCapabilityFlags = (uint)(__VSPPROJECTUPGRADEVIAFACTORYFLAGS.PUVFF_BACKUPSUPPORTED | __VSPPROJECTUPGRADEVIAFACTORYFLAGS.PUVFF_COPYBACKUP);
            pguidNewProjectFactory = Guid.Empty;

            ProjectUpgradeHelper projectData;

            try
            {
                projectData = new ProjectUpgradeHelper(projectFileName);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Nemerle language");
                pUpgradeRequired = 0;
                return VSConstants.E_FAIL;
            }

            if (IsNeedUpdateNemerleProperty(projectData.NemerleProperty))
                return VSConstants.S_OK;

            if (IsNeedUpdateNemerleVersion(projectData))
                return VSConstants.S_OK;

            if (IsNeedUpdateNemerleBinPathRootProperty(projectData.NemerleBinPathRoot))
                return VSConstants.S_OK;

            pUpgradeRequired = 0;

            return VSConstants.S_OK;
        }

        private static Version ParseVersion(string value)
        {
            try { return Version.Parse(value); }
            catch (FormatException) { return new Version("0.0"); }
        }

        #endregion
    }
}
