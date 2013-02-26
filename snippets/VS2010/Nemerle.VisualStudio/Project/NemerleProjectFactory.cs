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

        static class Default
        {
            private const string FrameworkVersion45 = "4.5";
            private const string FrameworkVersion40 = "4.0";

            public const string ToolsVersion = "4.0";
            public const string NemerleVersion = "Net-" + ToolsVersion;

            public static readonly HashSet<string> ValidNemerleVersions = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase)
            {
                "Net-" + FrameworkVersion40
            };
            public const string NemerleBinPathRoot = @"$(ProgramFiles)\Nemerle";
            public static readonly string[] OldNemerlePropertyValues = new[] { @"$(ProgramFiles)\Nemerle", @"$(ProgramFiles)\Nemerle\Net-3.5", @"$(ProgramFiles)\Nemerle\Net-4.0" };
            public const string NemerleProperty = @"$(NemerleBinPathRoot)\$(NemerleVersion)";
#if VS2012
            public const string TargetFrameworkVersion = "v" + FrameworkVersion45;
#else
            public const string TargetFrameworkVersion = "v" + FrameworkVersion40;
#endif
            public static readonly HashSet<string> ValidTargetFrameworkVersions = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase)
            {
#if VS2012
                "v" + FrameworkVersion45,
#endif
                "v" + FrameworkVersion40
            };
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

            foreach (var oldNemerlePath in Default.OldNemerlePropertyValues)
                if (Utils.Eq(value, oldNemerlePath))
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

            projectData.ToolsVersion.Value = Default.ToolsVersion;

            if (IsNeedUpdateNemerleBinPathRootProperty(projectData.NemerleBinPathRoot))
            {
                projectData.NemerleBinPathRoot.Value = Default.NemerleBinPathRoot;
                projectData.NemerleBinPathRoot.SetAttributeValue("Condition", " '$(NemerleBinPathRoot)' == '' ");
            }
            else if (!Utils.Eq(projectData.NemerleBinPathRoot.Value, Default.NemerleBinPathRoot))
                pLogger.LogMessage((uint)__VSUL_ERRORLEVEL.VSUL_WARNING, projectName, sourceProjectFilePath, "The NemerleBinPathRoot property changed by user. You must update it manually.");

            projectData.NemerleVersion.Value = Default.NemerleVersion;

            if (IsNeedUpdateNemerleProperty(projectData.NemerleProperty))
                projectData.NemerleProperty.Value = Default.NemerleProperty;
            else if (!Utils.Eq(projectData.NemerleProperty.Value, Default.NemerleProperty))
                pLogger.LogMessage((uint)__VSUL_ERRORLEVEL.VSUL_WARNING, projectName, sourceProjectFilePath, "The Nemerle property changed by user. You must update it manually.");

            projectData.TargetFrameworkVersion.Value = Default.TargetFrameworkVersion;

            projectData.NemerleProperty.Document.Save(destProjectFilePath);
        }

        private static bool IsNeedUpdateNemerleVersion(ProjectUpgradeHelper projectData)
        {
            return !Default.ValidNemerleVersions.Contains(projectData.NemerleVersion.Value);
        }

        private static bool IsNeedUpdateTargetFrameworkVersion(ProjectUpgradeHelper projectData)
        {
            return !Default.ValidTargetFrameworkVersions.Contains(projectData.TargetFrameworkVersion.Value);
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
                MessageBox.Show(ex.Message, "Nemerle laguage");
                pUpgradeRequired = 0;
                return VSConstants.E_FAIL;
            }

            var version = ParseVersion(projectData.ToolsVersion.Value);
            if (projectData.ToolsVersion == null || version.Major != 4 && version.Minor != 0)
                return VSConstants.S_OK;

            if (IsNeedUpdateNemerleProperty(projectData.NemerleProperty))
                return VSConstants.S_OK;

            if (IsNeedUpdateNemerleVersion(projectData))
                return VSConstants.S_OK;

            if (IsNeedUpdateNemerleBinPathRootProperty(projectData.NemerleBinPathRoot))
                return VSConstants.S_OK;

            if (IsNeedUpdateTargetFrameworkVersion(projectData))
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
