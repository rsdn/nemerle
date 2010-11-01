/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using Microsoft.Build.Framework;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VsSDK.UnitTestLibrary;
using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;

namespace Microsoft.VisualStudio.Project.UnitTests
{
	internal static class MockFactories
	{
		private static GenericMockFactory hierarchyFactory;
		public static GenericMockFactory HierarchyFactory
		{
			get
			{
				if(null == hierarchyFactory)
				{
					hierarchyFactory = new GenericMockFactory("EmptyMockHierarchy", new Type[] { typeof(IVsHierarchy) });
				}
				return hierarchyFactory;
			}
		}

		public static IVsHierarchy HierarchyForLogger(IOleServiceProvider provider)
		{
			BaseMock mock = HierarchyFactory.GetInstance();
			mock.AddMethodReturnValues(
				string.Format("{0}.{1}", typeof(IVsHierarchy).FullName, "GetSite"),
				new object[] { 0, provider });

			return mock as IVsHierarchy;
		}

		private static GenericMockFactory outputWindowPaneFactory;
		public static GenericMockFactory OutputWindowPaneFactory
		{
			get
			{
				if(null == outputWindowPaneFactory)
				{
					outputWindowPaneFactory = new GenericMockFactory("EmptyMockIVsOutputWindowPane", new Type[] { typeof(IVsOutputWindowPane) });
				}
				return outputWindowPaneFactory;
			}
		}

		private static void OutputStringCallback(object sender, CallbackArgs args)
		{
			BaseMock mock = (BaseMock)sender;
			System.Text.StringBuilder builder = (System.Text.StringBuilder)mock["StringBuilder"];
			string text = (string)args.GetParameter(0);
			builder.Append(text);
			args.ReturnValue = 0;
		}
		private static void ClearOutputCallback(object sender, CallbackArgs args)
		{
			BaseMock mock = (BaseMock)sender;
			System.Text.StringBuilder builder = (System.Text.StringBuilder)mock["StringBuilder"];
			builder.Length = 0;
			args.ReturnValue = 0;
		}
		public static IVsOutputWindowPane OutputPaneWithStringFunctions()
		{
			BaseMock mock = OutputWindowPaneFactory.GetInstance();
			mock["StringBuilder"] = new System.Text.StringBuilder();
			mock.AddMethodCallback(
				string.Format("{0}.{1}", typeof(IVsOutputWindowPane).FullName, "OutputString"),
				new EventHandler<CallbackArgs>(OutputStringCallback));
			mock.AddMethodCallback(
				string.Format("{0}.{1}", typeof(IVsOutputWindowPane).FullName, "OutputStringThreadSafe"),
				new EventHandler<CallbackArgs>(OutputStringCallback));
			mock.AddMethodCallback(
				string.Format("{0}.{1}", typeof(IVsOutputWindowPane).FullName, "Clear"),
				new EventHandler<CallbackArgs>(ClearOutputCallback));

			return mock as IVsOutputWindowPane;
		}

		private static GenericMockFactory eventSourceFactory;
		public static GenericMockFactory MSBuildEventSourceFactory
		{
			get
			{
				if(null == eventSourceFactory)
				{
					eventSourceFactory = new GenericMockFactory("EmptyMockIEventSource", new Type[] { typeof(IEventSource) });
				}
				return eventSourceFactory;
			}
		}

		#region Default callback functions for the event source.
		private static void EventSourceAddMessageRaised(object sender, CallbackArgs args)
		{
			BaseMock mock = (BaseMock)sender;
			mock["MessageRaised"] = args.GetParameter(0);
		}
		private static void EventSourceAddBuildStarted(object sender, CallbackArgs args)
		{
			BaseMock mock = (BaseMock)sender;
			mock["BuildStarted"] = args.GetParameter(0);
		}
		private static void EventSourceAddBuildFinished(object sender, CallbackArgs args)
		{
			BaseMock mock = (BaseMock)sender;
			mock["BuildFinished"] = args.GetParameter(0);
		}
		private static void EventSourceAddTaskStarted(object sender, CallbackArgs args)
		{
			BaseMock mock = (BaseMock)sender;
			mock["TaskStarted"] = args.GetParameter(0);
		}
		private static void EventSourceAddTaskFinished(object sender, CallbackArgs args)
		{
			BaseMock mock = (BaseMock)sender;
			mock["TaskFinished"] = args.GetParameter(0);
		}
		#endregion

		public static BaseMock CreateMSBuildEventSource()
		{
			BaseMock mockSource = MSBuildEventSourceFactory.GetInstance();
			mockSource.AddMethodCallback(
				string.Format("{0}.{1}", typeof(IEventSource).FullName, "add_MessageRaised"),
				new EventHandler<CallbackArgs>(EventSourceAddMessageRaised));
			mockSource.AddMethodCallback(
				string.Format("{0}.{1}", typeof(IEventSource).FullName, "add_BuildFinished"),
				new EventHandler<CallbackArgs>(EventSourceAddBuildFinished));
			mockSource.AddMethodCallback(
				string.Format("{0}.{1}", typeof(IEventSource).FullName, "add_BuildStarted"),
				new EventHandler<CallbackArgs>(EventSourceAddBuildStarted));
			mockSource.AddMethodCallback(
				string.Format("{0}.{1}", typeof(IEventSource).FullName, "add_TaskStarted"),
				new EventHandler<CallbackArgs>(EventSourceAddTaskStarted));
			mockSource.AddMethodCallback(
				string.Format("{0}.{1}", typeof(IEventSource).FullName, "add_TaskFinished"),
				new EventHandler<CallbackArgs>(EventSourceAddTaskFinished));

			return mockSource;
		}
	}
}
