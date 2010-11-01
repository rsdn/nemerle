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
using System.Runtime.Versioning;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VsSDK.UnitTestLibrary;
using MSBuild = Microsoft.Build.BuildEngine;
using OleServiceProvider = Microsoft.VsSDK.UnitTestLibrary.OleServiceProvider;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject.UnitTests
{
	/// <summary>
	///This is a test class for VisualStudio.Project.Samples.NestedProject.GeneralPropertyPage and is intended
	///to contain all VisualStudio.Project.Samples.NestedProject.GeneralPropertyPage Unit Tests
	///</summary>
	[TestClass()]
	public class GeneralPropertyPageTest : BaseTest
	{
		#region Fields

		private string testString;
		VisualStudio_Project_Samples_GeneralPropertyPageAccessor gppAccessor;

		#endregion Fields


		#region Initialization && Cleanup

		/// <summary>
		/// Runs before the test to allocate and configure resources needed 
		/// by all tests in the test class.
		/// </summary>
		[TestInitialize()]
		public void GeneralPropertyPageTestInitialize()
		{
            base.Initialize();
			testString = "This is a test string";

			// Initialize GeneralPropertyPage instance
			gppAccessor = new VisualStudio_Project_Samples_GeneralPropertyPageAccessor(generalPropertyPage);
		}

		#endregion Initialization && Cleanup


		#region Test methods
		/// <summary>
		/// The test for ApplicationIcon.
		/// AppIcon must be internally assigned and isDirty flag switched on.
		///</summary>
		[TestMethod()]
		public void ApplicationIconTest()
		{
			GeneralPropertyPage target = generalPropertyPage;
			target.ApplicationIcon = testString;

			Assert.AreEqual(testString, gppAccessor.applicationIcon, target.ApplicationIcon,
				"ApplicationIcon value was not initialized by expected value.");
			Assert.IsTrue((VSConstants.S_OK == target.IsPageDirty()),
				"IsDirty status was unexpected after changing of the property of the tested object.");
		}
		/// <summary>
		/// The test for ApplyChanges() in scenario when ProjectMgr is uninitialized.
		///</summary>
		[TestMethod()]
		public void ApplyChangesNullableProjectMgrTest()
		{
			GeneralPropertyPage target = generalPropertyPage;
			// sets indirectly projectMgr to null
			target.SetObjects(0, null);
			int actual = gppAccessor.ApplyChanges();
			Assert.IsNull(target.ProjectMgr, "ProjectMgr instance was not initialized to null as it expected.");
			Assert.AreEqual(VSConstants.E_INVALIDARG, actual, "Method ApplyChanges() was returned unexpected value in case of uninitialized project instance.");
		}
		/// <summary>
		/// The test for AssemblyName property.
		///</summary>
		[TestMethod()]
		public void AssemblyNameTest()
		{
			GeneralPropertyPage target = generalPropertyPage;
			target.AssemblyName = testString;
			Assert.AreEqual(testString, gppAccessor.assemblyName, target.ApplicationIcon,
				"AssemblyName property value was not initialized by expected value.");
			Assert.IsTrue((VSConstants.S_OK == target.IsPageDirty()), "IsDirty status was unexpected after changing of the property of the tested object.");
		}
		/// <summary>
		/// The test for GeneralPropertyPage default constructor.
		///</summary>
		[TestMethod()]
		public void ConstructorTest()
		{
			GeneralPropertyPage target = generalPropertyPage;
			target.Name = testString;
			Assert.AreEqual(testString, target.Name, target.ApplicationIcon,
				"Name property value was not initialized by expected value in GeneralPropertyPage() constructor.");
		}
		/// <summary>
		/// The test for DefaultNamespace property.
		///</summary>
		[TestMethod()]
		public void DefaultNamespaceTest()
		{
			GeneralPropertyPage target = generalPropertyPage;
			target.DefaultNamespace = testString;
			Assert.AreEqual(testString, target.DefaultNamespace, "DefaultNamespace property value was not initialized by expected value;");
			Assert.IsTrue((VSConstants.S_OK == target.IsPageDirty()), "IsDirty status was unexpected after changing of the property of the tested object.");
		}
		/// <summary>
		/// The test for GetClassName()  method.
		///</summary>
		[TestMethod()]
		public void GetClassNameTest()
		{
			GeneralPropertyPage target = generalPropertyPage;
			string expectedClassName = "Microsoft.VisualStudio.Project.Samples.NestedProject.GeneralPropertyPage";
			string actualClassName = target.GetClassName();

			Assert.AreEqual(expectedClassName, actualClassName,
				"GetClassName() method was returned unexpected Type FullName value.");
		}
		/// <summary>
		/// The test for OutputFile in case of OutputType.Exe file type.
		///</summary>
		[TestMethod()]
		public void OutputFileWithExeTypeTest()
		{
			GeneralPropertyPage target = generalPropertyPage;
			gppAccessor.outputType = OutputType.Exe;
			string expectedValue = target.AssemblyName + ".exe";

			Assert.AreEqual(expectedValue, target.OutputFile,
				"OutputFile name was initialized by unexpected value for EXE OutputType.");
		}
		/// <summary>
		/// The test for OutputFile property in case of using of OutputType.WinExe file type.
		///</summary>
		[TestMethod()]
		public void OutputFileWithWinExeTypeTest()
		{
			GeneralPropertyPage target = generalPropertyPage;
			gppAccessor.outputType = OutputType.WinExe;
			string expectedValue = target.AssemblyName + ".exe";

			Assert.AreEqual(expectedValue, target.OutputFile,
				"OutputFile name was initialized by unexpected value for WINEXE OutputType.");
		}
		/// <summary>
		/// The test for OutputFile in case of using of OutputType.Library file type.
		///</summary>
		[TestMethod()]
		public void OutputFileWithLibraryTypeTest()
		{
			GeneralPropertyPage target = generalPropertyPage;
			gppAccessor.outputType = OutputType.Library;
			string expectedValue = target.AssemblyName + ".dll";

			Assert.AreEqual(expectedValue, target.OutputFile,
				"OutputFile name was initialized by unexpected value for Library OutputType.");
		}
		/// <summary>
		/// The test for OutputType property.
		///</summary>
		[TestMethod()]
		public void OutputTypeTest()
		{
			GeneralPropertyPage target = generalPropertyPage;
			OutputType expectedOutType = OutputType.Library;
			target.OutputType = expectedOutType;

			Assert.AreEqual(expectedOutType, target.OutputType,
				"OutputType property value was initialized by unexpected value.");
			Assert.IsTrue((VSConstants.S_OK == target.IsPageDirty()),
				"IsDirty status was unexpected after changing of the property of the tested object.");
		}
		/// <summary>
		/// The test for StartupObject property.
		///</summary>
		[TestMethod()]
		public void StartupObjectTest()
		{
			GeneralPropertyPage target = generalPropertyPage;
			target.StartupObject = testString;
			Assert.AreEqual(testString, gppAccessor.startupObject, target.StartupObject,
				"StartupObject property value was not initialized by expected value.");
			Assert.IsTrue((VSConstants.S_OK == target.IsPageDirty()),
				"IsDirty status was unexpected after changing of the property of the tested object.");
		}
		/// <summary>
		/// The test for TargetPlatform property.
		///</summary>
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

		/// <summary>
		/// The test for BindProperties() method.
		///</summary>
		[TestMethod()]
		public void BindPropertiesTest()
		{
			PrepareProjectConfig();
			gppAccessor.defaultNamespace = null;
			gppAccessor.startupObject = null;
			gppAccessor.applicationIcon = null;
			gppAccessor.assemblyName = null;
			gppAccessor.targetFrameworkMoniker = null;

			// NOTE: For the best test result we must tests all shown below fields:
			// For this we must Load project with specified property values.
			//gppAccessor.targetPlatform
			//gppAccessor.targetPlatformLocation
			//gppAccessor.defaultNamespace
			//gppAccessor.startupObject
			//gppAccessor.applicationIcon

			gppAccessor.BindProperties();
			Assert.IsNotNull(gppAccessor.assemblyName, "The AssemblyName was not properly initialized.");
		}
		/// <summary>
		/// The test for BindProperties() method in scenario when ProjectMgr is not initialized.
		///</summary>

		[TestMethod()]
		public void BindPropertiesWithNullableProjectMgrTest()
		{
			gppAccessor.BindProperties();

			Assert.IsNull(gppAccessor.assemblyName,
				"The AssemblyName was initialized in scenario when ProjectMgr is invalid.");
		}
		/// <summary>
		/// The test for ProjectFile property.
		///</summary>
		[TestMethod()]
		public void ProjectFileTest()
		{
			PrepareProjectConfig();
			GeneralPropertyPage target = generalPropertyPage;

			// Project File Name must be equivalent with name of the currently loaded project
			Assert.AreEqual(Path.GetFileName(fullPathToProjectFile), target.ProjectFile,
				"ProjectFile property value was initialized by unexpected path value.");
		}
		/// <summary>
		///The test for ProjectFolder property.
		///</summary>
		[TestMethod()]
		public void ProjectFolderTest()
		{
			PrepareProjectConfig();
			GeneralPropertyPage target = generalPropertyPage;

			string expectedProjectFolderPath = Path.GetDirectoryName(fullPathToProjectFile);
			expectedProjectFolderPath = Path.GetDirectoryName(expectedProjectFolderPath);

			// Project Folder path must be equivalent with path of the currently loaded project
			Assert.AreEqual(expectedProjectFolderPath, target.ProjectFolder,
				"ProjectFolder property value was initialized by unexpected path value.");

		}
		/// <summary>
		/// The test for ApplyChanges() in scenario when ProjectMgr is initialized.
		///</summary>
		[TestMethod()]
		public void ApplyChangesTest()
		{
			PrepareProjectConfig();
			int actual = gppAccessor.ApplyChanges();

			Assert.AreEqual(VSConstants.S_OK, actual,
				"Method ApplyChanges() was returned unexpected value in case of initialized project instance.");
		}
		#endregion Completed test methods

		#region Service functions
		/// <summary>
		/// Initialize ProjectConfig and internal projectMgr objects.
		/// </summary>
		/// <remarks>Service function. Before calling this function projectNode must be 
		/// initialized by valid project data.</remarks>
		protected void PrepareProjectConfig()
		{
			object[] ppUnk = new object[2];
			ProjectConfig pjc = new ProjectConfig(projectNode, "manualSetConfigArgument");
			ppUnk[0] = pjc;
			generalPropertyPage.SetObjects(1, ppUnk);
		}
		#endregion Service functions
	}
}

