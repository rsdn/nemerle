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
using Microsoft.Build.BuildEngine;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Samples.CustomProject;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Microsoft.VisualStudio.Project.Samples.CustomProject.UnitTests
{
	[TestClass]
	public class MyCustomFileNodeTest : BaseTest
	{
		private CustomProjectPackage customProjectPackage;
		private MyCustomProjectFactory customProjectFactory;
		private MyCustomProjectNode projectNode;

		[ClassInitialize]
		public static void TestClassInitialize(TestContext context)
		{
			projectFile = Path.Combine(context.TestDeploymentDir, projectFile);
		}

		[TestInitialize()]
		public override void Initialize()
		{
			base.Initialize();
			customProjectPackage = new CustomProjectPackage();
			((IVsPackage)customProjectPackage).SetSite(serviceProvider);

			customProjectFactory = new MyCustomProjectFactory(customProjectPackage);

			base.SetMsbuildEngine(customProjectFactory);

			int canCreate;

			if(VSConstants.S_OK == ((IVsProjectFactory)customProjectFactory).CanCreateProject(projectFile, 2, out canCreate))
			{
				PrivateType type = new PrivateType(typeof(MyCustomProjectFactory));
				PrivateObject obj = new PrivateObject(customProjectFactory, type);
				projectNode = (MyCustomProjectNode)obj.Invoke("PreCreateForOuter", new object[] { IntPtr.Zero });

				Guid iidProject = new Guid();
				int pfCanceled;
				projectNode.Load(projectFile, "", "", 2, ref iidProject, out pfCanceled);
			}
		}

        ////[TestMethod()]
        ////public void GetAutomationObjectTest()
        ////{
        ////    ProjectElement element =
        ////        projectNode.GetProjectElement(new BuildItem("Compile", "AssemblyInfo.cs"));

        ////    Assert.IsNotNull(projectNode.CreateFileNode(element).GetAutomationObject(), "AutomationObject is null");
        ////}
	}
}