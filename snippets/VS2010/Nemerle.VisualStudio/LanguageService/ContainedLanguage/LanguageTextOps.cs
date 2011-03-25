/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using Microsoft.VisualStudio.TextManager.Interop;
using VSConstants = Microsoft.VisualStudio.VSConstants;

namespace Nemerle.VisualStudio.LanguageService
{
	/// <summary>
	/// The implementation of this interface is needed only to work around a bug in the
	/// HTML editor. If it is not implemented, then GetPairExtent does not work.
	/// Note that all the methods return E_NOTIMPL because the actual implementation
	/// of the interface is not important in this context.
	/// </summary>
	public partial class NemerleLanguage : IVsLanguageTextOps
	{
		[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1033:InterfaceMethodsShouldBeCallableByChildTypes")]
		int IVsLanguageTextOps.Format(IVsTextLayer pTextLayer, TextSpan[] ptsSel)
		{
			return VSConstants.E_NOTIMPL;
		}

		[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1033:InterfaceMethodsShouldBeCallableByChildTypes")]
		int IVsLanguageTextOps.GetDataTip(IVsTextLayer pTextLayer, TextSpan[] ptsSel, TextSpan[] ptsTip, out string pbstrText)
		{
			pbstrText = string.Empty;
			return VSConstants.E_NOTIMPL;
		}

		[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1033:InterfaceMethodsShouldBeCallableByChildTypes")]
		int IVsLanguageTextOps.GetPairExtent(IVsTextLayer pTextLayer, TextAddress ta, TextSpan[] pts)
		{
			return VSConstants.E_NOTIMPL;
		}

		[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1033:InterfaceMethodsShouldBeCallableByChildTypes")]
		int IVsLanguageTextOps.GetWordExtent(IVsTextLayer pTextLayer, TextAddress ta, WORDEXTFLAGS flags, TextSpan[] pts)
		{
			return VSConstants.E_NOTIMPL;
		}
	}
}
