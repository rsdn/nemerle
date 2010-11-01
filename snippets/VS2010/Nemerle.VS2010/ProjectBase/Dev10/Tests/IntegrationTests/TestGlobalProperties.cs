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
using EnvDTE;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VSSDK.Tools.VsIdeTesting;
using MSBuild = Microsoft.Build.Evaluation;
namespace Microsoft.VisualStudio.Project.IntegrationTests
{
	/// <summary>
	/// Tests global property support on MPF
	/// </summary>
	[TestClass]
	public class TestGlobalProperties : BaseTest
	{
		[TestMethod()]
		public void TestConfigChange()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				EnvDTE.Property property = dte.Solution.Properties.Item("ActiveConfig");

				// Now change the active config that should trigger a project config change event and the global property should be thus updated.
				property.Value = "Release|x86";

                MSBuild.Project buildProject = typeof(ProjectNode).GetProperty("BuildProject", BindingFlags.Instance | BindingFlags.NonPublic).GetValue(project, new object[] { }) as MSBuild.Project;
                string activeConfig = null;
                buildProject.GlobalProperties.TryGetValue(GlobalProperty.Configuration.ToString(), out activeConfig);

				Assert.AreEqual("Release", activeConfig);
			});
		}
	}
}
