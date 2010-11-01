/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.IO;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VsSDK.UnitTestLibrary;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject.UnitTests
{
	static class MockServicesProvider
	{
		private static GenericMockFactory solutionBuildManager;
		private static GenericMockFactory solutionFactory;
		private static GenericMockFactory vsShellFactory;
		private static GenericMockFactory uiShellFactory;
		private static GenericMockFactory trackSelFactory;
		private static GenericMockFactory runningDocFactory;
		private static GenericMockFactory windowFrameFactory;
		private static GenericMockFactory qeqsFactory;
		private static GenericMockFactory localRegistryFactory;
		private static GenericMockFactory registerProjectFactory;
        private static GenericMockFactory fileChangeEx;

		#region SolutionBuildManager Getters
		/// <summary>
		/// Returns a SVsSolutionBuildManager that does not implement any methods
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetSolutionBuildManagerInstance()
		{
			if(solutionBuildManager == null)
			{
				solutionBuildManager = new GenericMockFactory("SolutionBuildManager", new Type[] { typeof(IVsSolutionBuildManager2), typeof(IVsSolutionBuildManager3) });
			}
			BaseMock buildManager = solutionBuildManager.GetInstance();
			return buildManager;
		}

		/// <summary>
		/// Returns a SVsSolutionBuildManager that implements IVsSolutionBuildManager2 and IVsSolutionBuildManager3
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetSolutionBuildManagerInstance0()
		{
			BaseMock buildManager = GetSolutionBuildManagerInstance();
			string name = string.Format("{0}.{1}", typeof(IVsSolutionBuildManager2).FullName, "AdviseUpdateSolutionEvents");
			buildManager.AddMethodCallback(name, new EventHandler<CallbackArgs>(AdviseUpdateSolutionEventsCallBack));
			name = string.Format("{0}.{1}", typeof(IVsSolutionBuildManager3).FullName, "AdviseUpdateSolutionEvents3");
			buildManager.AddMethodCallback(name, new EventHandler<CallbackArgs>(AdviseUpdateSolutionEvents3CallBack));
			return buildManager;
		}
		#endregion

		#region SolutionFactory Getters
		/// <summary>
		/// Returns an IVsUiShell that does not implement any methods
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetSolutionFactoryInstance()
		{
			if(solutionFactory == null)
			{
				solutionFactory = new GenericMockFactory("MockSolution", new Type[] { typeof(IVsSolution) });
			}
			BaseMock solution = solutionFactory.GetInstance();
			return solution;
		}
		#endregion

		#region VsShell Getters
		/// <summary>
		/// Returns an IVsShell that does not implement any methods
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetVsShellInstance()
		{
			if(vsShellFactory == null)
			{
				vsShellFactory = new GenericMockFactory("VsShell", new Type[] { typeof(IVsShell) });
			}
			BaseMock shell = vsShellFactory.GetInstance();
			return shell;
		}

		/// <summary>
		/// Returns an IVsShell that implements GetProperty
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetVsShellInstance0()
		{
			BaseMock vsShell = GetVsShellInstance();
			string name = string.Format("{0}.{1}", typeof(IVsShell).FullName, "GetProperty");
			vsShell.AddMethodCallback(name, new EventHandler<CallbackArgs>(GetPropertyCallBack));
			return vsShell;
		}

		#endregion

		#region UiShell Getters
		/// <summary>
		/// Returns an IVsUiShell that does not implement any methods
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetUiShellInstance()
		{
			if(uiShellFactory == null)
			{
				uiShellFactory = new GenericMockFactory("UiShell", new Type[] { typeof(IVsUIShell), typeof(IVsUIShellOpenDocument) });
			}
			BaseMock uiShell = uiShellFactory.GetInstance();
			return uiShell;
		}

		/// <summary>
		/// Get an IVsUiShell that implements SetWaitCursor, SaveDocDataToFile, ShowMessageBox
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetUiShellInstance0()
		{
			BaseMock uiShell = GetUiShellInstance();
			string name = string.Format("{0}.{1}", typeof(IVsUIShell).FullName, "SetWaitCursor");
			uiShell.AddMethodCallback(name, new EventHandler<CallbackArgs>(SetWaitCursorCallBack));
			name = string.Format("{0}.{1}", typeof(IVsUIShell).FullName, "SaveDocDataToFile");
			uiShell.AddMethodCallback(name, new EventHandler<CallbackArgs>(SaveDocDataToFileCallBack));
			name = string.Format("{0}.{1}", typeof(IVsUIShell).FullName, "ShowMessageBox");
			uiShell.AddMethodCallback(name, new EventHandler<CallbackArgs>(ShowMessageBoxCallBack));
			name = string.Format("{0}.{1}", typeof(IVsUIShellOpenDocument).FullName, "OpenCopyOfStandardEditor");
			uiShell.AddMethodCallback(name, new EventHandler<CallbackArgs>(OpenCopyOfStandardEditorCallBack));
			return uiShell;
		}
		#endregion

		#region TrackSelection Getters

		/// <summary>
		/// Get an ITrackSelection mock object which implements the OnSelectChange method
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetTrackSelectionInstance()
		{
			if(trackSelFactory == null)
			{
				trackSelFactory = new GenericMockFactory("MockTrackSelection", new Type[] { typeof(ITrackSelection) });
			}
			BaseMock trackSel = trackSelFactory.GetInstance();
			string name = string.Format("{0}.{1}", typeof(ITrackSelection).FullName, "OnSelectChange");
			trackSel.AddMethodCallback(name, new EventHandler<CallbackArgs>(OnSelectChangeCallBack));
			return trackSel;
		}
		#endregion

		#region RunningDocumentTable Getters

		/// <summary>
		/// Gets the IVsRunningDocumentTable mock object which implements FindAndLockIncrement, 
		/// NotifyDocumentChanged and UnlockDocument
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetRunningDocTableInstance()
		{
			if(null == runningDocFactory)
			{
				runningDocFactory = new GenericMockFactory("RunningDocumentTable", new Type[] { typeof(IVsRunningDocumentTable) });
			}
			BaseMock runningDoc = runningDocFactory.GetInstance();
			string name = string.Format("{0}.{1}", typeof(IVsRunningDocumentTable).FullName, "FindAndLockDocument");
			runningDoc.AddMethodCallback(name, new EventHandler<CallbackArgs>(FindAndLockDocumentCallBack));
			name = string.Format("{0}.{1}", typeof(IVsRunningDocumentTable).FullName, "NotifyDocumentChanged");
			runningDoc.AddMethodReturnValues(name, new object[] { VSConstants.S_OK });
			name = string.Format("{0}.{1}", typeof(IVsRunningDocumentTable).FullName, "UnlockDocument");
			runningDoc.AddMethodReturnValues(name, new object[] { VSConstants.S_OK });
			return runningDoc;
		}
		#endregion

		#region Window Frame Getters

		/// <summary>
		/// Gets an IVsWindowFrame mock object which implements the SetProperty method
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetWindowFrameInstance()
		{
			if(null == windowFrameFactory)
			{
				windowFrameFactory = new GenericMockFactory("WindowFrame", new Type[] { typeof(IVsWindowFrame) });
			}
			BaseMock windowFrame = windowFrameFactory.GetInstance();
			string name = string.Format("{0}.{1}", typeof(IVsWindowFrame).FullName, "SetProperty");
			windowFrame.AddMethodReturnValues(name, new object[] { VSConstants.S_OK });
			return windowFrame;
		}
		#endregion

		#region QueryEditQuerySave Getters

		/// <summary>
		/// Gets an IVsQueryEditQuerySave2 mock object which implements QuerySaveFile and QueryEditFiles methods
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetQueryEditQuerySaveInstance()
		{
			if(null == qeqsFactory)
			{
				qeqsFactory = new GenericMockFactory("QueryEditQuerySave", new Type[] { typeof(IVsQueryEditQuerySave2) });
			}

			BaseMock qeqs = qeqsFactory.GetInstance();

			string name = string.Format("{0}.{1}", typeof(IVsQueryEditQuerySave2).FullName, "QuerySaveFile");
			qeqs.AddMethodCallback(name, new EventHandler<CallbackArgs>(QuerySaveFileCallBack));
			name = string.Format("{0}.{1}", typeof(IVsQueryEditQuerySave2).FullName, "QueryEditFiles");
			qeqs.AddMethodCallback(name, new EventHandler<CallbackArgs>(QueryEditFilesCallBack));
			return qeqs;
		}
		#endregion

		#region LocalRegistry Getters

		/// <summary>
		/// Gets an IVsLocalRegistry mock object
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetLocalRegistryInstance()
		{
			if(null == localRegistryFactory)
			{
				localRegistryFactory = new GenericMockFactory("MockLocalRegistry", new Type[] { typeof(ILocalRegistry), typeof(ILocalRegistry3) });
			}
			BaseMock localRegistry = localRegistryFactory.GetInstance();

			string name = string.Format("{0}.{1}", typeof(IVsWindowFrame).FullName, "SetProperty");
			localRegistry.AddMethodReturnValues(name, new object[] { VSConstants.S_OK });

			name = string.Format("{0}.{1}", typeof(ILocalRegistry3).FullName, "GetLocalRegistryRoot");
			localRegistry.AddMethodCallback(name, new EventHandler<CallbackArgs>(GetLocalRegistryRoot));

			return localRegistry;
		}
		#endregion

		#region RegisterProjectservice Getters

		/// <summary>
		/// Gets an IVsRegisterProject service mock object
		/// </summary>
		/// <returns></returns>
		internal static BaseMock GetRegisterProjectInstance()
		{
			if(null == registerProjectFactory)
			{
				registerProjectFactory = new GenericMockFactory("MockRegisterProject", new Type[] { typeof(IVsRegisterProjectTypes) });
			}
			BaseMock mock = registerProjectFactory.GetInstance();

			return mock;
		}
		#endregion
        #region RegisterProjectservice Getters

        /// <summary>
        /// Gets an IVsFileChnageEx service mock object
        /// </summary>
        /// <returns></returns>
        internal static BaseMock GetIVsFileChangeEx()
        {
            if (null == fileChangeEx)
            {
                fileChangeEx = new GenericMockFactory("MockIVsFileChangeEx", new Type[] { typeof(IVsFileChangeEx) });
            }
            BaseMock mock = fileChangeEx.GetInstance();

            return mock;
        }
        #endregion

		#region Callbacks
		private static void AdviseUpdateSolutionEventsCallBack(object caller, CallbackArgs arguments)
		{
			arguments.ReturnValue = VSConstants.S_OK;
		}

		private static void AdviseUpdateSolutionEvents3CallBack(object caller, CallbackArgs arguments)
		{
			arguments.ReturnValue = VSConstants.S_OK;
		}

		private static void GetPropertyCallBack(object caller, CallbackArgs arguments)
		{
			__VSSPROPID propertyID = (__VSSPROPID)arguments.GetParameter(0);
			switch(propertyID)
			{
				case __VSSPROPID.VSSPROPID_IsInCommandLineMode:
					// fake that we are running in command line mode in order to load normally (security)
					arguments.SetParameter(1, true);
					break;
				default:
					break;
			}
			arguments.ReturnValue = VSConstants.S_OK;

		}

		private static void SetWaitCursorCallBack(object caller, CallbackArgs arguments)
		{
			arguments.ReturnValue = VSConstants.S_OK;
		}

		private static void SaveDocDataToFileCallBack(object caller, CallbackArgs arguments)
		{
			arguments.ReturnValue = VSConstants.S_OK;

			VSSAVEFLAGS dwSave = (VSSAVEFLAGS)arguments.GetParameter(0);
			IPersistFileFormat editorInstance = (IPersistFileFormat)arguments.GetParameter(1);
			string fileName = (string)arguments.GetParameter(2);

			//Call Save on the EditorInstance depending on the Save Flags
			switch(dwSave)
			{
				case VSSAVEFLAGS.VSSAVE_Save:
				case VSSAVEFLAGS.VSSAVE_SilentSave:
					editorInstance.Save(fileName, 0, 0);
					arguments.SetParameter(3, fileName);    // setting pbstrMkDocumentNew
					arguments.SetParameter(4, 0);           // setting pfSaveCanceled
					break;
				case VSSAVEFLAGS.VSSAVE_SaveAs:
					string newFileName = Environment.GetEnvironmentVariable("SystemDrive") +
						Path.DirectorySeparatorChar + "NewTempFile.rtf";
					editorInstance.Save(newFileName, 1, 0);     //Call Save with new file and remember=1
					arguments.SetParameter(3, newFileName);     // setting pbstrMkDocumentNew
					arguments.SetParameter(4, 0);               // setting pfSaveCanceled
					break;
				case VSSAVEFLAGS.VSSAVE_SaveCopyAs:
					newFileName = Environment.GetEnvironmentVariable("SystemDrive") +
						Path.DirectorySeparatorChar + "NewTempFile.rtf";
					editorInstance.Save(newFileName, 0, 0);     //Call Save with new file and remember=0
					arguments.SetParameter(3, newFileName);     // setting pbstrMkDocumentNew
					arguments.SetParameter(4, 0);               // setting pfSaveCanceled
					break;
			}
		}

		private static void OnSelectChangeCallBack(object caller, CallbackArgs arguments)
		{
			arguments.ReturnValue = VSConstants.S_OK;
		}

		private static void FindAndLockDocumentCallBack(object caller, CallbackArgs arguments)
		{
			arguments.ReturnValue = VSConstants.S_OK;

			arguments.SetParameter(2, null);        //setting IVsHierarchy
			arguments.SetParameter(3, (uint)1);           //Setting itemID
			arguments.SetParameter(4, IntPtr.Zero); //Setting DocData
			arguments.SetParameter(5, (uint)1);           //Setting docCookie
		}

		private static void QuerySaveFileCallBack(object caller, CallbackArgs arguments)
		{
			arguments.ReturnValue = VSConstants.S_OK;

			arguments.SetParameter(3, (uint)tagVSQuerySaveResult.QSR_SaveOK);   //set result
		}

		private static void QueryEditFilesCallBack(object caller, CallbackArgs arguments)
		{
			arguments.ReturnValue = VSConstants.S_OK;

			arguments.SetParameter(5, (uint)tagVSQueryEditResult.QER_EditOK);   //setting result
			arguments.SetParameter(6, (uint)0);                                 //setting outFlags
		}

		private static void ShowMessageBoxCallBack(object caller, CallbackArgs arguments)
		{
			arguments.ReturnValue = VSConstants.S_OK;

			arguments.SetParameter(10, (int)DialogResult.Yes);
		}

		private static void OpenCopyOfStandardEditorCallBack(object caller, CallbackArgs arguments)
		{
			arguments.ReturnValue = VSConstants.S_OK;
			arguments.SetParameter(2, (IVsWindowFrame)GetWindowFrameInstance());
		}

		private static void GetLocalRegistryRoot(object caller, CallbackArgs arguments)
		{
			// this is required for fetch MSBuildPath key value
			arguments.SetParameter(0, @"SOFTWARE\Microsoft\VisualStudio\9.0"); // pbstrRoot
			arguments.ReturnValue = VSConstants.S_OK;
		}
		#endregion
	}
}