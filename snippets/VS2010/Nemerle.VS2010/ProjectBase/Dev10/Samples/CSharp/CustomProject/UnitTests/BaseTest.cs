/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System.Reflection;
using Microsoft.Build.BuildEngine;
using Microsoft.Build.Evaluation;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VsSDK.UnitTestLibrary;
using System.Xml;
using System.IO;
using System.Text;

namespace Microsoft.VisualStudio.Project.Samples.CustomProject.UnitTests
{
	public abstract class BaseTest
	{
		protected Microsoft.VsSDK.UnitTestLibrary.OleServiceProvider serviceProvider;
		protected TestContext testContextInstance;
		protected static string projectFile = "MyCustomProject.myproj";

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
			serviceProvider = Microsoft.VsSDK.UnitTestLibrary.OleServiceProvider.CreateOleServiceProviderWithBasicServices();
			// Solution Support
			serviceProvider.AddService(typeof(SVsSolution), MockIVsSolution.GetInstance(), false);
			// Project Types Support
			serviceProvider.AddService(typeof(SVsRegisterProjectTypes), MockIVsRegisterProjectTypes.GetInstance(), false);
			// UIShell Support
			BaseMock uiShell = MockIVsUIShell.GetInstance();
			serviceProvider.AddService(typeof(SVsUIShell), (IVsUIShell)uiShell, false);
			serviceProvider.AddService(typeof(SVsUIShellOpenDocument), (IVsUIShellOpenDocument)uiShell, false);
			// Shell Support
			serviceProvider.AddService(typeof(SVsShell), MockIVsShell.GetInstance(), false);
			// Build Manager support
			serviceProvider.AddService(typeof(SVsSolutionBuildManager), MockIVsSolutionBuildManager.GetInstance(), false);
			// ILocalRegistry support
			serviceProvider.AddService(typeof(SLocalRegistry), (ILocalRegistry)MockILocalRegistry.GetInstance(), false);

            // SVsFileChangeEx support
            serviceProvider.AddService(typeof(SVsFileChangeEx), MockIVsFileChangeEx.GetInstance(), false);
        }

		protected virtual void SetMsbuildEngine(ProjectFactory factory)
		{
            ProjectCollection.GlobalProjectCollection.UnloadAllProjects();

			FieldInfo buildEngine = typeof(ProjectFactory).GetField("buildEngine", BindingFlags.Instance | BindingFlags.NonPublic);
            buildEngine.SetValue(factory, ProjectCollection.GlobalProjectCollection);
           
            Microsoft.Build.Evaluation.Project msbuildproject = ProjectCollection.GlobalProjectCollection.LoadProject(projectFile);
			FieldInfo buildProject = typeof(ProjectFactory).GetField("buildProject", BindingFlags.Instance | BindingFlags.NonPublic);
			buildProject.SetValue(factory, msbuildproject);
		}
	}
}
