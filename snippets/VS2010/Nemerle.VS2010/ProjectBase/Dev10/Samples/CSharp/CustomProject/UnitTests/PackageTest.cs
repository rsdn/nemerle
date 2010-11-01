/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project.Samples.CustomProject;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Microsoft.VisualStudio.Project.Samples.CustomProject.UnitTests
{
	[TestClass]
	public class PackageTest
	{
		private Microsoft.VsSDK.UnitTestLibrary.OleServiceProvider serviceProvider;

		[TestInitialize()]
		public void Initialize()
		{
			serviceProvider = Microsoft.VsSDK.UnitTestLibrary.OleServiceProvider.CreateOleServiceProviderWithBasicServices();
			serviceProvider.AddService(typeof(SVsSolution), MockIVsSolution.GetInstance(), false);
			serviceProvider.AddService(typeof(SVsRegisterProjectTypes), MockIVsRegisterProjectTypes.GetInstance(), false);
		}

		[TestMethod()]
		public void ConstructorTest()
		{
			CustomProjectPackage package = new CustomProjectPackage();
			Assert.IsNotNull(package, "Constructor failed");
		}

		[TestMethod()]
		public void IsIVsPackage()
		{
			CustomProjectPackage package = new CustomProjectPackage();
			Assert.IsNotNull(package as IVsPackage, "The object does not implement IVsPackage");
		}

		[TestMethod()]
		public void InitializeTest()
		{
			IVsPackage package = new CustomProjectPackage();

			int expected = VSConstants.S_OK;
			int actual = package.SetSite(serviceProvider);

			Assert.AreEqual(expected, actual, "SetSite did not return VSConstants.S_OK");

			actual = package.SetSite(null);

			Assert.AreEqual(expected, actual, "SetSite(null) did not return VSConstants.S_OK");
		}
	}
}