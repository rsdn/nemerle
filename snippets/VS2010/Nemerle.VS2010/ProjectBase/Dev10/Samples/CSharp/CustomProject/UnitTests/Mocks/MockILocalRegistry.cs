/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VsSDK.UnitTestLibrary;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio;

namespace Microsoft.VisualStudio.Project.Samples.CustomProject.UnitTests
{
	public static class MockILocalRegistry
	{
		private static GenericMockFactory factory;

		static MockILocalRegistry()
		{
			factory = new GenericMockFactory("MockILocalRegistry", new Type[] { typeof(ILocalRegistry), typeof(ILocalRegistry3) });
		}

		public static BaseMock GetInstance()
		{
			BaseMock mock = factory.GetInstance();

			string name = string.Format("{0}.{1}", typeof(IVsWindowFrame).FullName, "SetProperty");
			mock.AddMethodReturnValues(name, new object[] { VSConstants.S_OK });

			name = string.Format("{0}.{1}", typeof(ILocalRegistry3).FullName, "GetLocalRegistryRoot");
			mock.AddMethodCallback(name, new EventHandler<CallbackArgs>(GetLocalRegistryRoot));

			return mock;
		}

		private static void GetLocalRegistryRoot(object caller, CallbackArgs arguments)
		{
			arguments.SetParameter(0, @"SOFTWARE\Microsoft\VisualStudio\9.0");
			arguments.ReturnValue = VSConstants.S_OK;
		}
	}
}