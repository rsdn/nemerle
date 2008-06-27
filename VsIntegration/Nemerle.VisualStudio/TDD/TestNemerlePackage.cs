/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Text;
using System.Collections.Generic;
using System.Reflection;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Nemerle.VsIntegration.Project;
using Microsoft.VsSDK.UnitTestLibrary;

namespace NemerleProject.UnitTest
{
    /// <summary>
    /// Summary description for TestNemerlePackage
    /// </summary>
    [TestClass]
    public class TestNemerlePackage
    {
        public TestNemerlePackage()
        {
        }

        #region Additional test attributes
        //
        // You can use the following additional attributes as you write your tests:
        //
        // Use ClassInitialize to run code before running the first test in the class
        // [ClassInitialize()]
        // public static void MyClassInitialize(TestContext testContext) { }
        //
        // Use ClassCleanup to run code after all tests in a class have run
        // [ClassCleanup()]
        // public static void MyClassCleanup() { }
        //
        // Use TestInitialize to run code before running each test 
        // [TestInitialize()]
        // public void MyTestInitialize() { }
        //
        // Use TestCleanup to run code after each test has run
        // [TestCleanup()]
        // public void MyTestCleanup() { }
        //
        #endregion

        [TestMethod]
        public void CreateInstance()
        {
            NemerleProjectPackage package = new NemerleProjectPackage();
            Assert.IsNotNull(package);
        }

        [TestMethod]
        public void TestPackageSetSite()
        {
            // TODO Define new Mock object for SolutionService
            //IVsPackage package = new NemerleProjectPackage() as IVsPackage;
            
            //OleServiceProvider oleServiceProvider = OleServiceProvider.CreateOleServiceProviderWithBasicServices();
            
            //Assert.AreEqual<int>(VSConstants.S_OK, package.SetSite(oleServiceProvider));
        }

        [TestMethod]
        public void TestInitialize()
        {
            // TODO Define new Mock object for SolutionService
            //NemerleProjectPackage package = new NemerleProjectPackage();
            //Type t = package.GetType();
            //MethodInfo mi = typeof(NemerleProjectPackage).GetMethod("Initialize", BindingFlags.NonPublic | BindingFlags.Instance);
            //Assert.IsNotNull(mi);
            //mi.Invoke(package, new object[] { });

        }
    }
}
