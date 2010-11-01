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
using System.Reflection;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VsSDK.UnitTestLibrary;
using Microsoft.Win32;

namespace Microsoft.VisualStudio.Project.UnitTests
{
	internal class IDELoggerProxy
	{
		// Static members.
		private static Type buildLoggerType;
		private static Type IDEBuildLoggerType
		{
			get
			{
				if(null == buildLoggerType)
				{
					Assembly asm = typeof(VisualStudio.Project.ProjectNode).Assembly;
					buildLoggerType = asm.GetType("Microsoft.VisualStudio.Project.IDEBuildLogger");
				}
				return buildLoggerType;
			}
		}

		private static ConstructorInfo loggerConstructor;
		private static ConstructorInfo LoggerConstructor
		{
			get
			{
				if(null == loggerConstructor)
				{
					Type[] constructorParams = new Type[] {
                        typeof(IVsOutputWindowPane),
                        typeof(TaskProvider),
                        typeof(IVsHierarchy)
                    };
					loggerConstructor = IDEBuildLoggerType.GetConstructor(constructorParams);
				}
				return loggerConstructor;
			}
		}

		private Logger loggerInstance;
		internal IDELoggerProxy(IVsOutputWindowPane pane, TaskProvider taskProvider, IVsHierarchy hierarchy)
		{
			loggerInstance = (Logger)LoggerConstructor.Invoke(new object[] { pane, taskProvider, hierarchy });
			Assert.IsNotNull(loggerInstance);
		}

		public void Initialize(IEventSource eventSource)
		{
			loggerInstance.Initialize(eventSource);
		}

        private static MethodInfo setVerbosityRoot;
		public void SetBuildVerbosityRegistryRoot(string root)
		{
			if(null == setVerbosityRoot)
			{
				setVerbosityRoot = IDEBuildLoggerType.GetMethod("set_BuildVerbosityRegistryRoot", BindingFlags.NonPublic | BindingFlags.Instance);
			}
			setVerbosityRoot.Invoke(loggerInstance, new object[] { root });
		}

		public LoggerVerbosity Verbosity
		{
			get { return loggerInstance.Verbosity; }
			set { loggerInstance.Verbosity = value; }
		}

        public void ClearVerbosityCache()
        {
            IDEBuildLoggerType.InvokeMember("ClearCachedVerbosity", BindingFlags.InvokeMethod | BindingFlags.NonPublic | BindingFlags.Instance, null, loggerInstance, null);
        }
	}

	[TestClass]
	public class BuildLoggerTest
	{
		private delegate void LoggerVerbositySetter(IDELoggerProxy logger, LoggerVerbosity verbosity);

		private class VerbosityImportancePair
		{
			private LoggerVerbosity verbosity;
			private MessageImportance importance;
			private bool shouldPrint;

			public VerbosityImportancePair(LoggerVerbosity v, MessageImportance i, bool print)
			{
				verbosity = v;
				importance = i;
				shouldPrint = print;
			}

			public MessageImportance Importance
			{
				get { return importance; }
			}
			public bool ShouldPrint
			{
				get { return shouldPrint; }
			}
			public LoggerVerbosity Verbosity
			{
				get { return verbosity; }
			}
		}

		private static VerbosityImportancePair[] expectedVerbosityImportancePair = new VerbosityImportancePair[] {
            // No message should be written if the verbosity is Quiet.
            new VerbosityImportancePair(LoggerVerbosity.Quiet, MessageImportance.Low, false),
            new VerbosityImportancePair(LoggerVerbosity.Quiet, MessageImportance.Normal, false),
            new VerbosityImportancePair(LoggerVerbosity.Quiet, MessageImportance.High, false),
            // Only high importance messages should be writte for minimal verbosity.
            new VerbosityImportancePair(LoggerVerbosity.Minimal, MessageImportance.Low, false),
            new VerbosityImportancePair(LoggerVerbosity.Minimal, MessageImportance.Normal, false),
            new VerbosityImportancePair(LoggerVerbosity.Minimal, MessageImportance.High, true),
            // Normal verbosity should cut only the low importance messages.
            new VerbosityImportancePair(LoggerVerbosity.Normal, MessageImportance.Low, false),
            new VerbosityImportancePair(LoggerVerbosity.Normal, MessageImportance.Normal, true),
            new VerbosityImportancePair(LoggerVerbosity.Normal, MessageImportance.High, true),
            // Detailed verbosity should be as the normal one.
            new VerbosityImportancePair(LoggerVerbosity.Detailed, MessageImportance.Low, false),
            new VerbosityImportancePair(LoggerVerbosity.Detailed, MessageImportance.Normal, true),
            new VerbosityImportancePair(LoggerVerbosity.Detailed, MessageImportance.High, true),
            // Diagnostic output should print everything
            new VerbosityImportancePair(LoggerVerbosity.Diagnostic, MessageImportance.Low, true),
            new VerbosityImportancePair(LoggerVerbosity.Diagnostic, MessageImportance.Normal, true),
            new VerbosityImportancePair(LoggerVerbosity.Diagnostic, MessageImportance.High, true),
        };

		private const string TestVerbosityRoot = @"Software\Microsoft\VSSDK\Tests\MPFProject";
		private const string LoggerVerbosityKey = "General";
		private const string LoggerVerbosityLabel = "MSBuildLoggerVerbosity";
		private static void DeleteVerbosityKey()
		{
			using(RegistryKey testRoot = Registry.CurrentUser.OpenSubKey(TestVerbosityRoot, RegistryKeyPermissionCheck.ReadWriteSubTree))
			{
				if(testRoot != null)
				{
					RegistryKey verbosityKey = testRoot.OpenSubKey(LoggerVerbosityKey);
					if(null != verbosityKey)
					{
						verbosityKey.Close();
						testRoot.DeleteSubKeyTree(LoggerVerbosityKey);
					}
				}
			}
		}

		private static bool HasConstructorThrown<ExceptionType>(IVsOutputWindowPane pane, TaskProvider taskProvider, IVsHierarchy hierarchy)
			where ExceptionType : Exception
		{
			bool hasThrown = false;
			try
			{
				IDELoggerProxy proxy = new IDELoggerProxy(pane, taskProvider, hierarchy);
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

		/// <summary>
		/// Verify that the constructor of the build logger validates the parameters.
		/// Note that the first parameter (the output window pane) can be null.
		/// </summary>
		[TestMethod]
		public void LoggerConstructorParameter()
		{
			using(OleServiceProvider provider = new OleServiceProvider())
			{
				Assert.IsTrue(HasConstructorThrown<ArgumentNullException>(null, null, null));
				using(TaskProvider task = new TaskProvider(null))
				{
					IVsHierarchy hierarchy = MockFactories.HierarchyForLogger(provider);
					IVsOutputWindowPane output = MockFactories.OutputWindowPaneFactory.GetInstance() as IVsOutputWindowPane;

					Assert.IsTrue(HasConstructorThrown<ArgumentNullException>(null, task, null));
					Assert.IsFalse(HasConstructorThrown<ArgumentNullException>(null, task, hierarchy));
					Assert.IsTrue(HasConstructorThrown<ArgumentNullException>(null, null, hierarchy));
					Assert.IsFalse(HasConstructorThrown<ArgumentNullException>(output, task, hierarchy));
					Assert.IsTrue(HasConstructorThrown<ArgumentNullException>(output, null, hierarchy));
					Assert.IsTrue(HasConstructorThrown<ArgumentNullException>(output, task, null));
				}
			}
		}

		/// <summary>
		/// Make sure that the Initialize method validates the input parameter.
		/// </summary>
		[TestMethod]
		[ExpectedException(typeof(ArgumentNullException))]
		public void LoggerInitializeBadParam()
		{
			using(OleServiceProvider provider = new OleServiceProvider())
			{
				using(TaskProvider task = new TaskProvider(new ServiceProvider(provider)))
				{
					IVsHierarchy hierarchy = MockFactories.HierarchyForLogger(provider);
					IDELoggerProxy logger = new IDELoggerProxy(null, task, hierarchy);
					logger.Initialize(null);
				}
			}
		}

		/// <summary>
		/// This is the common function that executes the verification that the build
		/// messages are correctly written or omitted from the output according with
		/// the verbosity level.
		/// It takes as argument a function that set the verbosity for the logger. This
		/// delegate is needed because there are at least two different ways to set the
		/// verbosity: one reading it from the registry, the other setting it directly
		/// on the logger.
		/// </summary>
		private static void VerifyLoggingByVerbosity(LoggerVerbositySetter verbositySetter)
		{
			using(OleServiceProvider provider = new OleServiceProvider())
			{
				using(TaskProvider task = new TaskProvider(new ServiceProvider(provider)))
				{
					IVsHierarchy hierarchy = MockFactories.HierarchyForLogger(provider);
					BaseMock mockOutput = MockFactories.OutputWindowPaneFactory.GetInstance();

					// Create the logger and make sure that it points to the verbosity
					// registry key that we have deleted at the beginning of the test.
					IDELoggerProxy logger = new IDELoggerProxy(mockOutput as IVsOutputWindowPane, task, hierarchy);
					logger.SetBuildVerbosityRegistryRoot(TestVerbosityRoot);

					// Create the IEventSource that will be used to send messages to the logger
					BaseMock mockSource = MockFactories.CreateMSBuildEventSource();

					// Now initialize the logger.
					logger.Initialize(mockSource as IEventSource);
					// Verify that the logger has installed an event handler for messages.
					Assert.IsNotNull(mockSource["MessageRaised"]);
					BuildMessageEventHandler messageHandler = (BuildMessageEventHandler)mockSource["MessageRaised"];

					foreach(VerbosityImportancePair expected in expectedVerbosityImportancePair)
					{
                        // Because the IDEBuildLogger caches its Verbosity setting after reading it 
                        // from the registry, we need to clear the cached value before calling the 
                        // verbositySetter, which may set verbosity by changing the registry value.
                        logger.ClearVerbosityCache();

                        // Set the verbosity of the logger.
						verbositySetter(logger, expected.Verbosity);
						// Create a message of the expected importance.
						BuildMessageEventArgs message = new BuildMessageEventArgs("message", "help", "sender", expected.Importance);

						// Reset the counter of the calls to the output window pane.
						mockOutput.ResetAllFunctionCalls();

						// Execute the message event handler
						messageHandler.Invoke(mockSource, message);
                        System.Windows.Forms.Application.DoEvents();

						// Verify that the output pane was used only if the expected result is that
						// something should be printed on the output.
						if(expected.ShouldPrint)
						{
							Assert.IsTrue(mockOutput.TotalCallsAllFunctions() > 0);
						}
						else
						{
							Assert.AreEqual<int>(mockOutput.TotalCallsAllFunctions(), 0);
						}
					}
				}
			}
		}


		private static void VerbositySetterOnLogger(IDELoggerProxy logger, LoggerVerbosity verbosity)
		{
			logger.Verbosity = verbosity;
		}
		/// <summary>
		/// The IDEBuildLogger class uses a registry key to find the verbosity level
		/// of the logging or, if the key is not present, uses any previous value set.
		/// In this test we verify that if the registry key is not present, the value
		/// set with the Verbosity property of the object is used correctly.
		/// </summary>
		[TestMethod]
		public void VerifyLoggingByVerbosity_NoRegKey()
		{
			// Make sure that the registry key is no present.
			DeleteVerbosityKey();
			VerifyLoggingByVerbosity(new LoggerVerbositySetter(VerbositySetterOnLogger));
		}


		private static void VerbositySetterOnRegistry(IDELoggerProxy logger, LoggerVerbosity verbosity)
		{
			string verbosityRegistryPath = string.Format("{0}\\{1}", TestVerbosityRoot, LoggerVerbosityKey);
			using(RegistryKey verbosityKey = Registry.CurrentUser.CreateSubKey(verbosityRegistryPath, RegistryKeyPermissionCheck.ReadWriteSubTree))
			{
				verbosityKey.SetValue(LoggerVerbosityLabel, (int)verbosity);
			}
		}
		/// <summary>
		/// This test verifies that verbosity is handled correctly when it the logger reads it
		/// from the registry.
		/// </summary>
		[TestMethod]
		public void VerifyLoggingByVerbosity_FromRegKey()
		{
			VerifyLoggingByVerbosity(new LoggerVerbositySetter(VerbositySetterOnRegistry));
		}

		[TestMethod]
		public void MessageFormattingTest()
		{
			// Make sure that the registry key is no present so that we can set the verbosity.
			DeleteVerbosityKey();

			using(OleServiceProvider provider = new OleServiceProvider())
			{
				using(TaskProvider task = new TaskProvider(new ServiceProvider(provider)))
				{
					IVsHierarchy hierarchy = MockFactories.HierarchyForLogger(provider);
					IVsOutputWindowPane output = MockFactories.OutputPaneWithStringFunctions();
					BaseMock mockOutput = (BaseMock)output;

					// Create the logger and make sure that it points to the verbosity
					IDELoggerProxy logger = new IDELoggerProxy(output, task, hierarchy);
					logger.SetBuildVerbosityRegistryRoot(TestVerbosityRoot);

					// Create the IEventSource that will be used to send messages to the logger
					BaseMock mockSource = MockFactories.CreateMSBuildEventSource();

					// Now initialize the logger.
					logger.Initialize(mockSource as IEventSource);
                    
					// Verify that the logger has installed an event handler for messages and
					// build events.
					Assert.IsNotNull(mockSource["MessageRaised"]);
					BuildMessageEventHandler messageHandler = (BuildMessageEventHandler)mockSource["MessageRaised"];
					Assert.IsNotNull(mockSource["BuildStarted"]);
					BuildStartedEventHandler buildStartedHandler = (BuildStartedEventHandler)mockSource["BuildStarted"];
					Assert.IsNotNull(mockSource["BuildFinished"]);
					BuildFinishedEventHandler buildFinishedHandler = (BuildFinishedEventHandler)mockSource["BuildFinished"];
					Assert.IsNotNull(mockSource["TaskStarted"]);
					TaskStartedEventHandler taskStartedHandler = (TaskStartedEventHandler)mockSource["TaskStarted"];
					Assert.IsNotNull(mockSource["TaskFinished"]);
					TaskFinishedEventHandler taskFinishedHandler = (TaskFinishedEventHandler)mockSource["TaskFinished"];

					// Set the verbosity to Diagnostic so that all the messages are written.
					logger.Verbosity = LoggerVerbosity.Diagnostic;

					// Create a message of the expected importance.
					BuildMessageEventArgs message = new BuildMessageEventArgs("message", "help", "sender", MessageImportance.Normal);
					messageHandler.Invoke(mockSource, message);

					// Add a second message with null text.
					message = new BuildMessageEventArgs(null, "help", "sender", MessageImportance.Normal);
					messageHandler.Invoke(mockSource, message);

					// Add another message with emty text.
					message = new BuildMessageEventArgs(string.Empty, "help", "sender", MessageImportance.Normal);
					messageHandler.Invoke(mockSource, message);
                    System.Windows.Forms.Application.DoEvents();

					string expected = "message" + Environment.NewLine;
					System.Text.StringBuilder builder = (System.Text.StringBuilder)mockOutput["StringBuilder"];
					Assert.AreEqual(expected, builder.ToString(), false);

					// Clean up the text.
					output.Clear();

					// Now verify the identation in case of start and end build events.
					string startText = "Test Started";
					string messageText = "build message";
					string endText = "Test Finished";
					BuildStartedEventArgs startBuildArgs = new BuildStartedEventArgs(startText, "help");
					buildStartedHandler.Invoke(mockSource, startBuildArgs);
					message = new BuildMessageEventArgs(messageText, "help", "sender", MessageImportance.Normal);
					messageHandler.Invoke(mockSource, message);
					BuildFinishedEventArgs finishBuildArgs = new BuildFinishedEventArgs(endText, "help", true);
					buildFinishedHandler.Invoke(mockSource, finishBuildArgs);
                    System.Windows.Forms.Application.DoEvents();

					// Get the text in the output pane.
					builder = (System.Text.StringBuilder)mockOutput["StringBuilder"];
					expected = string.Format("{0}{1}{2}{1}{1}{3}{1}", startText, Environment.NewLine, messageText, endText);
					Assert.AreEqual(expected, builder.ToString(), false);

					// Clear the output and test the identation in case of start and end task.
					output.Clear();
					TaskStartedEventArgs startTaskArgs = new TaskStartedEventArgs(startText, null, null, null, null);
					taskStartedHandler.Invoke(mockSource, startTaskArgs);
					message = new BuildMessageEventArgs(messageText, "help", "sender", MessageImportance.Normal);
					messageHandler.Invoke(mockSource, message);
					TaskFinishedEventArgs endTaskArgs = new TaskFinishedEventArgs(endText, null, null, null, null, true);
					taskFinishedHandler.Invoke(mockSource, endTaskArgs);
                    System.Windows.Forms.Application.DoEvents();

					// Verify the text in the output pane.
					expected = string.Format("{0}{1}\t{2}{1}{3}{1}", startText, Environment.NewLine, messageText, endText);
					builder = (System.Text.StringBuilder)mockOutput["StringBuilder"];
					Assert.AreEqual(expected, builder.ToString(), false);
				}
			}
		}

		private delegate void SendEventFunction();

		[TestMethod]
		public void EventsOutOfSequence()
		{
			// Make sure that the registry key is no present so that we can set the verbosity.
			DeleteVerbosityKey();
			using(OleServiceProvider provider = new OleServiceProvider())
			{
				using(TaskProvider task = new TaskProvider(new ServiceProvider(provider)))
				{
					IVsHierarchy hierarchy = MockFactories.HierarchyForLogger(provider);
					IVsOutputWindowPane output = MockFactories.OutputPaneWithStringFunctions();
					BaseMock mockOutput = (BaseMock)output;

					// Create the logger and make sure that it points to the verbosity
					IDELoggerProxy logger = new IDELoggerProxy(output, task, hierarchy);
					logger.SetBuildVerbosityRegistryRoot(TestVerbosityRoot);

					// Create the IEventSource that will be used to send messages to the logger
					BaseMock mockSource = MockFactories.CreateMSBuildEventSource();

					// Now initialize the logger.
					logger.Initialize(mockSource as IEventSource);

					// Verify that the logger has installed an event handler for messages and
					// build events.
					Assert.IsNotNull(mockSource["MessageRaised"]);
					BuildMessageEventHandler messageHandler = (BuildMessageEventHandler)mockSource["MessageRaised"];
					Assert.IsNotNull(mockSource["BuildStarted"]);
					BuildStartedEventHandler buildStartedHandler = (BuildStartedEventHandler)mockSource["BuildStarted"];
					Assert.IsNotNull(mockSource["BuildFinished"]);
					BuildFinishedEventHandler buildFinishedHandler = (BuildFinishedEventHandler)mockSource["BuildFinished"];
					Assert.IsNotNull(mockSource["TaskStarted"]);
					TaskStartedEventHandler taskStartedHandler = (TaskStartedEventHandler)mockSource["TaskStarted"];
					Assert.IsNotNull(mockSource["TaskFinished"]);
					TaskFinishedEventHandler taskFinishedHandler = (TaskFinishedEventHandler)mockSource["TaskFinished"];

					// Create the arguments for the events and the delegates.
					BuildMessageEventArgs messageArgs = new BuildMessageEventArgs("Message", "help", "sender", MessageImportance.Normal);
					SendEventFunction sendMessage = delegate { messageHandler.Invoke(mockSource, messageArgs); };
					//
					BuildStartedEventArgs startBuildArgs = new BuildStartedEventArgs("Start Build", "help");
					SendEventFunction sendStartBuild = delegate { buildStartedHandler.Invoke(mockSource, startBuildArgs); };
					//
					BuildFinishedEventArgs finishBuildArgs = new BuildFinishedEventArgs("End Build", "help", true);
					SendEventFunction sendEndBuild = delegate { buildFinishedHandler.Invoke(mockSource, finishBuildArgs); };
					//
					TaskStartedEventArgs startTaskArgs = new TaskStartedEventArgs("Start Task", null, null, null, null);
					SendEventFunction sendStartTask = delegate { taskStartedHandler.Invoke(mockSource, startTaskArgs); };
					//
					TaskFinishedEventArgs endTaskArgs = new TaskFinishedEventArgs("End Task", null, null, null, null, true);
					SendEventFunction sendEndTask = delegate { taskFinishedHandler.Invoke(mockSource, endTaskArgs); };

					// Set the verbosity to Diagnostic so that all the messages are written.
					logger.Verbosity = LoggerVerbosity.Diagnostic;

					List<SendEventFunction>[] events = new List<SendEventFunction>[] {
                        new List<SendEventFunction>(new SendEventFunction[]{ sendMessage, sendEndBuild, sendStartBuild }),
                        new List<SendEventFunction>(new SendEventFunction[]{ sendStartBuild, sendEndTask, sendEndBuild, sendStartTask }),
                        new List<SendEventFunction>(new SendEventFunction[]{ sendStartTask, sendStartBuild, sendEndTask, sendEndBuild }),
                        new List<SendEventFunction>(new SendEventFunction[]{ sendEndBuild, sendStartTask, sendEndTask, sendStartBuild }),
                        new List<SendEventFunction>(new SendEventFunction[]{ sendEndBuild, sendEndTask, sendStartTask, sendStartBuild }),
                        new List<SendEventFunction>(new SendEventFunction[]{ sendEndTask, sendEndBuild, sendStartTask, sendStartBuild }),
                    };

					// Call the functions. Note that here we don't check for the actual text printed on the output,
					// but we simply verify that the logger can handle the events without exceptions.
					foreach(List<SendEventFunction> sendFunctions in events)
					{
						output.Clear();
						foreach(SendEventFunction func in sendFunctions)
						{
							func();
						}
					}
				}
			}
		}
	}
}
