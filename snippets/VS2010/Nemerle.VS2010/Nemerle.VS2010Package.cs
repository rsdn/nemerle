using System;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.ComponentModel.Design;
using Microsoft.Win32;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Project;


namespace Nemerle.VS2010
{
    /// <summary>
    /// This is the class that implements the package exposed by this assembly.
    ///
    /// The minimum requirement for a class to be considered a valid package for Visual Studio
    /// is to implement the IVsPackage interface and register itself with the shell.
    /// This package uses the helper classes defined inside the Managed Package Framework (MPF)
    /// to do it: it derives from the Package class that provides the implementation of the 
    /// IVsPackage interface and uses the registration attributes defined in the framework to 
    /// register itself and its components with the shell.
    /// </summary>
    // This attribute tells the PkgDef creation utility (CreatePkgDef.exe) that this class is
    // a package.
    [PackageRegistration(UseManagedResourcesOnly = true)]
    // This attribute is used to register the informations needed to show the this package
    // in the Help/About dialog of Visual Studio.
    [ComVisible(true)]
    [Guid(NemerleConstants.PackageGuidString)]
    [WAProvideProjectFactory(typeof(WANemerleProjectFactory), NemerleConstants.LanguageName + " Web Application Project Templates", NemerleConstants.LanguageId, false, "Web", null)]
    [WAProvideProjectFactoryTemplateMapping("{" + NemerleConstants.ProjectFactoryGuidString + "}", typeof(WANemerleProjectFactory))]
    [WAProvideLanguageProperty(typeof(WANemerleProjectFactory), "CodeFileExtension", NemerleConstants.FileExtension)]
    [WAProvideLanguageProperty(typeof(WANemerleProjectFactory), "TemplateFolder", NemerleConstants.LanguageId)]
    [WAProvideLanguageProperty(typeof(WANemerleProjectFactory), "CodeBehindCodeGenerator", NemerleConstants.WACodeBehindCodeGeneratorGuidString)]
    //[WAProvideLanguageProperty(typeof(WANemerleProjectFactory), "CodeBehindEventBinding", typeof(NemerleWACodeBehindEventBinding))]

    [InstalledProductRegistration("#110", "#112", "1.0", IconResourceID = 400)]
    [ProvideProjectFactory(typeof(NemerleProjectFactory),
        NemerleConstants.LanguageName,
        NemerleConstants.LanguageName + " Project Files (*." +
            NemerleConstants.ProjectExtension + ");*." + NemerleConstants.ProjectExtension,
        NemerleConstants.ProjectExtension,
        NemerleConstants.ProjectExtension,
        // Set the projectsTemplatesDirectory to a non-existant path to prevent VS 
        // from including the working directory as a valid template path
        @".\NullPath",
        LanguageVsTemplate = NemerleConstants.LanguageName)]
   
    [ProvideObject(typeof(GeneralPropertyPage))]
    [DefaultRegistryRoot(NemerleConstants.VisualStudioRegistryRoot)]
   // [ProvideService(typeof(NemerleLanguageService), ServiceName = NemerleConstants.LanguageName)]
   
    public sealed class Nemerle_VS2010Package : ProjectPackage
    {
        /// <summary>
        /// Default constructor of the package.
        /// Inside this method you can place any initialization code that does not require 
        /// any Visual Studio service because at this point the package object is created but 
        /// not sited yet inside Visual Studio environment. The place to do all the other 
        /// initialization is the Initialize method.
        /// </summary>
        public Nemerle_VS2010Package()
        {
            Trace.WriteLine(string.Format(CultureInfo.CurrentCulture, "Entering constructor for: {0}", this.ToString()));
        }

      


        /////////////////////////////////////////////////////////////////////////////
        // Overriden Package Implementation
        #region Package Members

        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initilaization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override void Initialize()
        {
            Trace.WriteLine (string.Format(CultureInfo.CurrentCulture, "Entering Initialize() of: {0}", this.ToString()));
            base.Initialize();            
            this.RegisterProjectFactory(new NemerleProjectFactory(this));

        }
        #endregion
        public override string ProductUserContext
        {
            get { return "Nemerle"; }
        }

    }
}
