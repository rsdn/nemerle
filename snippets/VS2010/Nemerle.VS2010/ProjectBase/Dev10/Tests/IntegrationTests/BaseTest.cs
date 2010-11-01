/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using EnvDTE;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VSSDK.Tools.VsIdeTesting;
using Microsoft.VisualStudio.Project.Automation;
using MSBuild = Microsoft.Build.Evaluation;

namespace Microsoft.VisualStudio.Project.IntegrationTests
{
    [TestClass]
    public abstract class BaseTest
    {
        private TestContext testContextInstance;

        protected delegate void ThreadInvoker();

        /// <summary>
        ///Gets or sets the test context which provides
        ///information about and functionality for the current test run.
        ///</summary>
        public TestContext TestContext
        {
            get
            {
                return testContextInstance;
            }
            set
            {
                testContextInstance = value;
            }
        }

        [TestInitialize()]
        public virtual void Initialize()
        {
            UIThreadInvoker.Initialize();
            //UIThread.InitUnitTestingMode();
        }

        [TestCleanup()]
        public void MyTestCleanup()
        {
            IVsSolution solutionService = VsIdeTestHostContext.ServiceProvider.GetService(typeof(IVsSolution)) as IVsSolution;
            if (solutionService != null)
            {
                object isOpen;
                solutionService.GetProperty((int)__VSPROPID.VSPROPID_IsSolutionOpen, out isOpen);
                if ((bool)isOpen)
                {
                    solutionService.CloseSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_ForceSave, null, 0);
                }
            }
        }
    }
}
