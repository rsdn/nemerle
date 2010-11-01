/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using Microsoft.VsSDK.UnitTestLibrary;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VisualStudio.Shell.Interop;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject.UnitTests
{
	/// <summary>
	///This is a test class for VisualStudio.Project.Samples.NestedProject.NestedProjectPackage and is intended
	///to contain all VisualStudio.Project.Samples.NestedProject.NestedProjectPackage Unit Tests
	///</summary>
	[TestClass()]
	public class NestedProjectPackageTest
	{
		#region Test Methods
		[TestMethod()]
		public void CreateInstanceTest()
		{
			NestedProjectPackage package = new NestedProjectPackage();
			Assert.IsNotNull(package, "Failed to initialize an instance of NestedProjectPackage type.");
		}	
		#endregion	
	}
}
