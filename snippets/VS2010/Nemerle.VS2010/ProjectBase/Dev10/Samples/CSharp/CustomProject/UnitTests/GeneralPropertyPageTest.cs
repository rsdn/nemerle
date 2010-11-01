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
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Samples.CustomProject;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Runtime.Versioning;

namespace Microsoft.VisualStudio.Project.Samples.CustomProject.UnitTests
{
	[TestClass]
	public class GeneralPropertyPageTest : BaseTest
	{
		private CustomProjectPackage customProjectPackage;
		private GeneralPropertyPage generalPropertyPage;
		private MyCustomProjectFactory customProjectFactory;
		private MyCustomProjectNode projectNode;

		private const string expected = "test";

		[ClassInitialize]
		public static void TestClassInitialize(TestContext context)
		{
			projectFile = Path.Combine(context.TestDeploymentDir, projectFile);
		}

		[TestInitialize()]
		public override void Initialize()
		{
			base.Initialize();

			generalPropertyPage = new GeneralPropertyPage();

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
			GeneralPropertyPage page = generalPropertyPage;
			page.Name = expected;

			string actual = page.Name;
			Assert.AreEqual(expected, actual, "Name property value was not initialized by expected value in GeneralPropertyPage() constructor.");
		}

		[TestMethod()]
		public void ApplicationIconTest()
		{
			generalPropertyPage.ApplicationIcon = expected;

			string actual = generalPropertyPage.ApplicationIcon;

			Assert.AreEqual(expected, actual,
				"ApplicationIcon value was not initialized by expected value.");

			Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
				"IsDirty status was unexpected after changing of the property of the tested object.");
		}

		[TestMethod()]
		public void AssemblyNameTest()
		{
			generalPropertyPage.AssemblyName = expected;

			string actual = generalPropertyPage.AssemblyName;

			Assert.AreEqual(expected, actual,
				"ApplicationIcon value was not initialized by expected value.");

			Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
				"IsDirty status was unexpected after changing of the property of the tested object.");
		}

		[TestMethod()]
		public void DefaultNamespaceTest()
		{
			generalPropertyPage.DefaultNamespace = expected;

			string actual = generalPropertyPage.DefaultNamespace;

			Assert.AreEqual(expected, actual,
				"ApplicationIcon value was not initialized by expected value.");

			Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
				"IsDirty status was unexpected after changing of the property of the tested object.");
		}

		[TestMethod()]
		public void OutputFileDllTest()
		{
			generalPropertyPage.OutputType = OutputType.Library;

			OutputType expected = OutputType.Library;
			OutputType actual = generalPropertyPage.OutputType;

			Assert.AreEqual(expected, actual,
				"ApplicationIcon value was not initialized by expected value.");

			Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
				"IsDirty status was unexpected after changing of the property of the tested object.");
		}

		[TestMethod()]
		public void OutputFileExeTest()
		{
			generalPropertyPage.OutputType = OutputType.Exe;

			OutputType expected = OutputType.Exe;
			OutputType actual = generalPropertyPage.OutputType;

			Assert.AreEqual(expected, actual,
				"ApplicationIcon value was not initialized by expected value.");

			Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
				"IsDirty status was unexpected after changing of the property of the tested object.");
		}

		[TestMethod()]
		public void OutputFileWinExeTest()
		{
			generalPropertyPage.OutputType = OutputType.WinExe;

			OutputType expected = OutputType.WinExe;
			OutputType actual = generalPropertyPage.OutputType;

			Assert.AreEqual(expected, actual,
				"ApplicationIcon value was not initialized by expected value.");

			Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
				"IsDirty status was unexpected after changing of the property of the tested object.");
		}

		[TestMethod()]
		public void ProjectFileTest()
		{
			SetProjectConfig();

			string expected = Path.GetFileName(projectFile);
			string actual = generalPropertyPage.ProjectFile;

			Assert.AreEqual(expected, actual,
				"ProjectFile property value was initialized by unexpected path value.");
		}

		[TestMethod()]
		public void ProjectFolderTest()
		{
			SetProjectConfig();

			string expected = Path.GetDirectoryName(Path.GetDirectoryName(projectFile));
			string actual = generalPropertyPage.ProjectFolder;

			Assert.AreEqual(expected, actual,
				"ProjectFolder property value was initialized by unexpected path value.");
		}

		[TestMethod()]
		public void StartupObjectTest()
		{
			generalPropertyPage.StartupObject = expected;

			string actual = generalPropertyPage.StartupObject;

			Assert.AreEqual(expected, actual,
				"ApplicationIcon value was not initialized by expected value.");

			Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
				"IsDirty status was unexpected after changing of the property of the tested object.");
		}

		[TestMethod()]
		public void TargetFrameworkMonikerTest()
		{
			var fx4 = new FrameworkName(".NETFramework", new Version(4, 1));
			generalPropertyPage.TargetFrameworkMoniker = fx4;

			FrameworkName expected = fx4;
			FrameworkName actual = generalPropertyPage.TargetFrameworkMoniker;

			Assert.AreEqual(expected, actual,
				"TargetFrameworkMoniker value was not initialized to expected value.");

			Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
				"IsDirty status was unexpected after changing of the property of the tested object.");
		}

		private void SetProjectConfig()
		{
			object[] ppUnk = new object[2];
			ProjectConfig config = new ProjectConfig(projectNode, "manualSetConfig");
			ppUnk[0] = config;
			generalPropertyPage.SetObjects(1, ppUnk);
		}
	}
}