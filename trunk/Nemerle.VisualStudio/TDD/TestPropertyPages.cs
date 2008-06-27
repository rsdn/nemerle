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
using Microsoft.VisualStudio.Package;

namespace NemerleProject.UnitTest
{
    /// <summary>
    /// Summary description for TestPropertyPages
    /// </summary>
    [TestClass]
    public class TestPropertyPages
    {
        public TestPropertyPages()
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
            Assert.IsNotNull(new GeneralPropertyPage());            
        }

        [TestMethod]
        public void VerifyOutPutFileNameProperty()
        {
            GeneralPropertyPage page = new GeneralPropertyPage();

            // Set the assemblyname
            string defaultAssemblyName = "Test";
            FieldInfo assemblyName = typeof(GeneralPropertyPage).GetField("assemblyName", BindingFlags.NonPublic | BindingFlags.Instance);
            assemblyName.SetValue(page, defaultAssemblyName);
            Assert.AreEqual<string>(defaultAssemblyName, assemblyName.GetValue(page).ToString());

            // Test that Output file (DefaultAssemblyName and Library) provides expected result
            AssertOuputFile(page, defaultAssemblyName, OutputType.Library);

            // Test that Output file (DefaultAssemblyName and WinExe) provides expected result
            AssertOuputFile(page, defaultAssemblyName, OutputType.WinExe);

            // Test that Output file (DefaultAssemblyName and Exe) provides expected result
            AssertOuputFile(page, defaultAssemblyName, OutputType.Exe);
        }

        private static void AssertOuputFile(GeneralPropertyPage page, string expectedAssemblyName, OutputType outputType)
        {
            FieldInfo outputTypeInfo = typeof(GeneralPropertyPage).GetField("outputType", BindingFlags.NonPublic | BindingFlags.Instance);
            outputTypeInfo.SetValue(page, outputType);
            Assert.AreEqual<string>(outputType.ToString(), outputTypeInfo.GetValue(page).ToString());
            string expectedOutputFile = expectedAssemblyName + NemerleProjectNode.GetOuputExtension(outputType);
            Assert.AreEqual<string>(expectedOutputFile, page.OutputFile);
        }

    }
}
