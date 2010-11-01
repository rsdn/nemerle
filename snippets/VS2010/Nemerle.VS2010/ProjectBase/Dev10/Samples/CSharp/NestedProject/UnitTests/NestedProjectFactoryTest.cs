/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VsSDK.UnitTestLibrary;
using System.IO;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject.UnitTests
{
	/// <summary>
	///This is a test class for VisualStudio.Project.Samples.NestedProject.NestedProjectFactory and is intended
	///to contain all VisualStudio.Project.Samples.NestedProject.NestedProjectFactory Unit Tests
	///</summary>
	[TestClass()]
	public class NestedProjectFactoryTest : BaseTest
	{
        [ClassInitialize]
        public static void TestClassInitialize(TestContext context)
        {
            fullPathToClassTemplateFile = Path.Combine(context.TestDeploymentDir, fullPathToClassTemplateFile);
            fullPathToProjectFile = Path.Combine(context.TestDeploymentDir, fullPathToProjectFile);
            fullPathToTargetFile = Path.Combine(context.TestDeploymentDir, fullPathToTargetFile);
        }

		#region Test Methods
		/// <summary>
		///A test for CreateProject ()
		///</summary>
		[TestMethod()]
		public void CreateProjectTest()
		{
			NestedProjectFactory target = new NestedProjectFactory(projectPackage);

			VisualStudio_Project_Samples_NestedProjectFactoryAccessor accessor = new VisualStudio_Project_Samples_NestedProjectFactoryAccessor(target);

			ProjectNode actual = accessor.CreateProject();
			Assert.IsNotNull(actual, "CreateProject did not return the expected value.");
		}

		/// <summary>
		///A test for NestedProjectFactory (NestedProjectPackage)
		///</summary>
		[TestMethod()]
		public void ConstructorTest()
		{
			NestedProjectFactory target = new NestedProjectFactory(projectPackage);
			Assert.IsNotNull(target, "New instance if NestedProjectFactory  type was not correctly initialized.");
		}

		#endregion	
	}
}
