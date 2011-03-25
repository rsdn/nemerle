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
using Nemerle.VsIntegration.Project;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace NemerleProject.UnitTest
{
    /// <summary>
    /// Summary description for TestProjectFactory
    /// </summary>
    [TestClass]
    public class TestProjectFactory
    {
        public TestProjectFactory()
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
            Assert.IsNotNull(GetNewFactory());
        }

        [TestMethod]
        public void TestCreateProject()
        {
            
        }

        #region helper methods
        private NemerleProjectFactory GetNewFactory()
        {
            NemerleProjectPackage package = new NemerleProjectPackage();
            NemerleProjectFactory factory = new NemerleProjectFactory(package);
            return factory;
        }
        #endregion
    }
}
