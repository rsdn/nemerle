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
	public static class MockIVsShell
	{
		private static GenericMockFactory factory;

		static MockIVsShell()
		{
			factory = new GenericMockFactory("MockIVsShell", new Type[] { typeof(IVsShell) });
		}

		public static BaseMock GetInstance()
		{
			BaseMock mock = factory.GetInstance();

			string name = string.Format("{0}.{1}", typeof(IVsShell).FullName, "GetProperty");

			mock.AddMethodCallback(name, new EventHandler<CallbackArgs>(GetPropertyCallBack));

			return mock;
		}

		private static void GetPropertyCallBack(object caller, CallbackArgs arguments)
		{
			__VSSPROPID propertyID = (__VSSPROPID)arguments.GetParameter(0);

			switch(propertyID)
			{
				case __VSSPROPID.VSSPROPID_IsInCommandLineMode:
					arguments.SetParameter(1, true);
					break;
				default:
					break;
			}

			arguments.ReturnValue = VSConstants.S_OK;
		}
	}
}