/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.IO;
using System.Reflection;
using System.Text;
using System.Xml;
using Microsoft.Build.BuildEngine;
using Microsoft.Build.Evaluation;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VsSDK.UnitTestLibrary;
using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;
using MSBuild = Microsoft.Build.BuildEngine;
using OleServiceProvider = Microsoft.VsSDK.UnitTestLibrary.OleServiceProvider;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject.UnitTests
{
    /// <summary>
    /// NestedProjectFactory fake object creates NestedProjectNode fake object
    /// </summary>
    public class NestedProjectFactoryFake : NestedProjectFactory
    {
        public NestedProjectFactoryFake(NestedProjectPackage package)
            : base(package)
        {
        }

        protected override ProjectNode CreateProject()
        {
            NesteProjectNodeFake project = new NesteProjectNodeFake();
            project.SetSite((IOleServiceProvider)((IServiceProvider)this.Package).GetService(typeof(IOleServiceProvider)));
            return project;
        }
    }

    /// <summary>
    /// This is a fake object for NestedPRojectNode so that we can skip certain method calls,
    /// e.g. ProcessRefrences involves a build and we would like to skip that
    /// </summary>
    public class NesteProjectNodeFake : NestedProjectNode
    {
        protected internal override void ProcessReferences()
        {
            return;
        }
    }

    [TestClass]
	public abstract class BaseTest
	{
		protected Microsoft.VsSDK.UnitTestLibrary.OleServiceProvider serviceProvider;
		protected TestContext testContextInstance;
        protected static string fullPathToClassTemplateFile = @"TemplateClass.cs";
        protected static string fullPathToProjectFile = @"SampleProject.csproj";
        protected static string fullPathToTargetFile = @"SampleClass.cs";

        protected GeneralPropertyPage generalPropertyPage;

        protected NestedProjectPackage projectPackage;
        protected NestedProjectFactoryFake projectFactory;
        protected NesteProjectNodeFake projectNode;

		public TestContext TestContext
		{
			get
			{
				return testContextInstance;
			}
			set
			{
				testContextInstance = value;
			}
		}

		[TestInitialize()]
		public virtual void Initialize()
		{
            this.MockServices();
            this.LoadProject();
            UIThread.IsUnitTest = true;
        }

        /// <summary>
        /// Runs after the test has run and to free resources obtained 
        /// by all the tests in the test class.
        /// </summary>
        // [TestCleanup()]
        public void Cleanup()
        {
            ((IVsPackage)projectPackage).SetSite(null);
            serviceProvider.Dispose();

            generalPropertyPage = null;
        }

		protected virtual void SetMsbuildEngine(ProjectFactory factory)
		{
            ProjectCollection.GlobalProjectCollection.UnloadAllProjects();

			FieldInfo buildEngine = typeof(ProjectFactory).GetField("buildEngine", BindingFlags.Instance | BindingFlags.NonPublic);
            buildEngine.SetValue(factory, ProjectCollection.GlobalProjectCollection);

            Microsoft.Build.Evaluation.Project msbuildproject = ProjectCollection.GlobalProjectCollection.LoadProject(fullPathToProjectFile);
			FieldInfo buildProject = typeof(ProjectFactory).GetField("buildProject", BindingFlags.Instance | BindingFlags.NonPublic);
			buildProject.SetValue(factory, msbuildproject);
		}

        protected virtual void MockServices()
        {
            serviceProvider = Microsoft.VsSDK.UnitTestLibrary.OleServiceProvider.CreateOleServiceProviderWithBasicServices();

            // Add solution Support
            BaseMock solution = MockServicesProvider.GetSolutionFactoryInstance();
            serviceProvider.AddService(typeof(IVsSolution), solution, false);

            //Add site support for ILocalRegistry
            BaseMock localRegistry = MockServicesProvider.GetLocalRegistryInstance();
            serviceProvider.AddService(typeof(SLocalRegistry), (ILocalRegistry)localRegistry, false);

            // Add site support for UI Shell
            BaseMock uiShell = MockServicesProvider.GetUiShellInstance0();
            serviceProvider.AddService(typeof(SVsUIShell), uiShell, false);
            serviceProvider.AddService(typeof(SVsUIShellOpenDocument), (IVsUIShellOpenDocument)uiShell, false);

            // Add site support for RegisterProjectTypes
            BaseMock mock = MockServicesProvider.GetRegisterProjectInstance();
            serviceProvider.AddService(typeof(SVsRegisterProjectTypes), mock, false);

            // Add site support for VsShell
            BaseMock vsShell = MockServicesProvider.GetVsShellInstance0();
            serviceProvider.AddService(typeof(SVsShell), vsShell, false);

            // Add site support for SolutionBuildManager service
            BaseMock solutionBuildManager = MockServicesProvider.GetSolutionBuildManagerInstance0();
            serviceProvider.AddService(typeof(SVsSolutionBuildManager), solutionBuildManager, false);


            // SVsFileChangeEx support
            BaseMock fileChangeEx = MockServicesProvider.GetIVsFileChangeEx();
            serviceProvider.AddService(typeof(SVsFileChangeEx), fileChangeEx, false);
        }
        
        protected virtual void LoadProject()
        {
            generalPropertyPage = new GeneralPropertyPage();

            // Prepare the package
            projectPackage = new NestedProjectPackage();
            ((IVsPackage)projectPackage).SetSite(serviceProvider);

            // prepare the factory
            projectFactory = new NestedProjectFactoryFake(projectPackage);
            this.SetMsbuildEngine(projectFactory);

            //Create the project object using the projectfactory and load the project
            int canCreate;
            if (VSConstants.S_OK == ((IVsProjectFactory)projectFactory).CanCreateProject(fullPathToProjectFile, 2, out canCreate))
            {
                MethodInfo preCreateForOuter = typeof(NestedProjectFactory).GetMethod("PreCreateForOuter", BindingFlags.Instance | BindingFlags.NonPublic);
                Assert.IsNotNull(preCreateForOuter, "failed to get the PreCreateForOuter method info object from NestedProjectFactory type");
                projectNode = (NesteProjectNodeFake)preCreateForOuter.Invoke(projectFactory, new object[] { IntPtr.Zero });
                Assert.IsNotNull(projectNode, "Failed to create the projectnode object");
                Guid iidProject = new Guid();
                int pfCanceled;
                projectNode.Load(fullPathToProjectFile, "", "", 2, ref iidProject, out pfCanceled);
            }

        }
	}
}
