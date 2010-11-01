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
	public class MyCustomProjectNodeTest : BaseTest
	{
		private CustomProjectPackage customProjectPackage;
		private MyCustomProjectFactory customProjectFactory;
		private MyCustomProjectNode projectNode;
		private static string templateFile = @"Program.cs";
		private static string targetFile = @"Program1.cs";

		[ClassInitialize]
		public static void TestClassInitialize(TestContext context)
		{
			projectFile = Path.Combine(context.TestDeploymentDir, projectFile);
			templateFile = Path.Combine(context.TestDeploymentDir, templateFile);
			targetFile = Path.Combine(context.TestDeploymentDir, targetFile);
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

		[TestMethod()]
		public void ConstructorTest()
		{
			MyCustomProjectNode customProject = new MyCustomProjectNode(customProjectPackage);
			Assert.IsNotNull(customProject, "Constructor failed");
		}

		[TestMethod()]
		public void ProjectGuidTest()
		{
			Guid actual = typeof(MyCustomProjectFactory).GUID;
			Guid expected = projectNode.ProjectGuid;

			Assert.AreEqual(expected, actual, "ProjectGuid was not set correctly.");
		}

		[TestMethod()]
		public void GetAutomationObjectTest()
		{
			Assert.IsNotNull(projectNode.GetAutomationObject(), "AutomationObject is null");
		}

        ////[TestMethod()]
        ////public void CreateFileNodeTest()
        ////{
        ////    ProjectElement element =
        ////        projectNode.GetProjectElement(new BuildItem("Compile", "AssemblyInfo.cs"));

        ////    Assert.IsNotNull(projectNode.CreateFileNode(element), "FileNode is null");
        ////}

		[TestMethod()]
		public void AddFileFromTemplateTest()
		{
			projectNode.AddFileFromTemplate(templateFile, targetFile);

			string content = File.ReadAllText(targetFile);

			Assert.IsTrue(content.IndexOf('%') == -1, "Parameter replacement did not occurred");
		}

		[TestMethod()]
		public void GetConfigurationIndependentPropertyPagesTest()
		{
			Guid[] expected = new Guid[] { typeof(GeneralPropertyPage).GUID };
			Guid[] actual;

			PrivateType type = new PrivateType(typeof(MyCustomProjectNode));
			PrivateObject obj = new PrivateObject(projectNode, type);
			actual = (Guid[])obj.Invoke("GetConfigurationIndependentPropertyPages", new object[] { });

			CollectionAssert.AreEqual(expected, actual, "IndependentPropertyPages not correctly set");
		}

		[TestMethod()]
		public void GetPriorityProjectDesignerPagesTest()
		{
			Guid[] expected = new Guid[] { typeof(GeneralPropertyPage).GUID };
			Guid[] actual;

			PrivateType type = new PrivateType(typeof(MyCustomProjectNode));
			PrivateObject obj = new PrivateObject(projectNode, type);
			actual = (Guid[])obj.Invoke("GetPriorityProjectDesignerPages", new object[] { });

			CollectionAssert.AreEqual(expected, actual, "DesignerPages not correctly set");
		}
	}
}