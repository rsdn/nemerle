/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Reflection;
using System.Runtime;

namespace Microsoft.VisualStudio.Project.UnitTests
{
	internal static class Utilities
	{
		public delegate void ThrowingFunction();
		public static bool HasFunctionThrown<ExceptionType>(ThrowingFunction func)
			where ExceptionType : Exception
		{
			bool hasThrown = false;
			try
			{
				func();
			}
			catch(ExceptionType)
			{
				hasThrown = true;
			}
			catch(TargetInvocationException e)
			{
				ExceptionType inner = e.InnerException as ExceptionType;
				if(null != inner)
				{
					hasThrown = true;
				}
			}

			return hasThrown;
		}
	}
}
