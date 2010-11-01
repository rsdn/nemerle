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
using Microsoft.VisualStudio.Project.Samples.CustomProject;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Microsoft.VisualStudio.Project.Samples.CustomProject.UnitTests
{
	[TestClass]
	public class MyCustomProjectFactoryTest : BaseTest
	{
		private CustomProjectPackage customProjectPackage;

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
		}

		[TestMethod()]
		public void ConstructorTest()
		{
			MyCustomProjectFactory myCustomProjectFactory = new MyCustomProjectFactory(customProjectPackage);
			Assert.IsNotNull(myCustomProjectFactory, "Constructor failed");
		}

		[TestMethod()]
		public void CreateProjectTest()
		{
			int actual;
			int expected = 1;

			MyCustomProjectFactory customProjectFactory = new MyCustomProjectFactory(customProjectPackage);

			base.SetMsbuildEngine(customProjectFactory);

			((IVsProjectFactory)customProjectFactory).CanCreateProject(projectFile, 2, out actual);

			Assert.AreEqual(expected, actual, "Cannot create project");

			PrivateType type = new PrivateType(typeof(MyCustomProjectFactory));
			PrivateObject obj = new PrivateObject(customProjectFactory, type);

			MyCustomProjectNode projectNode = (MyCustomProjectNode)obj.Invoke("PreCreateForOuter", new object[] { IntPtr.Zero });

			Assert.IsNotNull(projectNode, "Cannot create project");
		}
	}
}