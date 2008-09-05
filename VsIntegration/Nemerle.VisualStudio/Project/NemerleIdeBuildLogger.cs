using System;
using System.Diagnostics;
using System.Globalization;
using System.CodeDom.Compiler;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;

using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.Win32;

using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// This class implements an MSBuild logger that output events to 
	/// VS outputwindow and tasklist.
	/// </summary>
	[ComVisible(true)]
	internal class NemerleIdeBuildLogger : Logger
	{
		#region fields
		// TODO: Remove these constants when we have a version that supports 
		//   getting the verbosity using automation.
		private string _buildVerbosityRegistryRoot;
		private const string BuildVerbosityRegistrySubKey = @"General";
		private const string BuildVerbosityRegistryKey = "MSBuildLoggerVerbosity";
		// TODO: Re-enable this constants when we have a version that 
		//   supports getting the verbosity using automation.
		//private const string EnvironmentCategory = "Environment";
		//private const string ProjectsAndSolutionSubCategory = "ProjectsAndSolution";
		//private const string BuildAndRunPage = "BuildAndRun";

		private int _currentIndent;
		private IVsOutputWindowPane _outputWindowPane;
		private string _errorString = Resources.Error;
		private string _warningString = Resources.Warning;
		private bool _isLogTaskDone;
		private TaskProvider _taskProvider;
		private IVsHierarchy _hierarchy;
		private IServiceProvider _serviceProvider;

		#endregion

		#region properties

		public string WarningString 
		{
			get { return _warningString; }
			set { _warningString = value; }
		}

		public string ErrorString
		{
			get { return _errorString; }
			set { _errorString = value; }
		}

		public bool IsLogTaskDone
		{
			get { return _isLogTaskDone; }
			set { _isLogTaskDone = value; }
		}

		/// <summary>
		/// When building from within VS, setting this will
		/// enable the logger to retrive the verbosity from
		/// the correct registry hive.
		/// </summary>
		internal string BuildVerbosityRegistryRoot
		{
			get { return _buildVerbosityRegistryRoot; }
			set { _buildVerbosityRegistryRoot = value; }
		}

		/// <summary>
		/// Set to null to avoid writing to the output window
		/// </summary>
		internal IVsOutputWindowPane OutputWindowPane
		{
			get { return _outputWindowPane; }
			set { _outputWindowPane = value; }
		}

		#endregion

		#region ctors

		/// <summary>
		/// Constructor.  Inititialize member data.
		/// </summary>
		public NemerleIdeBuildLogger(IVsOutputWindowPane output, TaskProvider taskProvider, IVsHierarchy hierarchy)
		{
			// HACK: We should use VS Automation for retrive root key!
			Regex rx = new Regex(@"/rootsuffix\s+(.+)");
			Match match = rx.Match(Environment.CommandLine);
			if (match.Success)
			{
				_buildVerbosityRegistryRoot = @"Software\Microsoft\VisualStudio\8.0"
					+ match.Groups[1].Value.Trim();
			}

			if (taskProvider == null)
				throw new ArgumentNullException("taskProvider");
			if (hierarchy == null)
				throw new ArgumentNullException("hierarchy");

			_taskProvider = taskProvider;
			_outputWindowPane = output;
			_hierarchy = hierarchy;
			IOleServiceProvider site;
			Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(hierarchy.GetSite(out site));
			_serviceProvider = new ServiceProvider(site);
		}

		#endregion

		#region overridden methods
		/// <summary>
		/// Overridden from the Logger class.
		/// </summary>
		public override void Initialize(IEventSource eventSource)
		{
			if (null == eventSource)
				throw new ArgumentNullException("eventSource");

			eventSource.BuildStarted += BuildStartedHandler;
			eventSource.BuildFinished += BuildFinishedHandler;
			eventSource.ProjectStarted += ProjectStartedHandler;
			eventSource.ProjectFinished += ProjectFinishedHandler;
			eventSource.TargetStarted += TargetStartedHandler;
			eventSource.TargetFinished += TargetFinishedHandler;
			eventSource.TaskStarted += TaskStartedHandler;
			eventSource.TaskFinished += TaskFinishedHandler;
			eventSource.CustomEventRaised += CustomHandler;
			eventSource.ErrorRaised += ErrorRaisedHandler;
			eventSource.WarningRaised += WarningHandler;
			eventSource.MessageRaised += MessageHandler;
		}

		#endregion

		#region event delegates

		/// <summary>
		/// This is the delegate for error events.
		/// </summary>
		private void ErrorRaisedHandler(object sender, BuildErrorEventArgs errorEvent)
		{
			AddToErrorList(errorEvent, errorEvent.Code, errorEvent.File,
				errorEvent.LineNumber, errorEvent.ColumnNumber);
		}

		/// <summary>
		/// This is the delegate for warning events.
		/// </summary>
		private void WarningHandler(object sender, BuildWarningEventArgs errorEvent)
		{
			AddToErrorList(errorEvent, errorEvent.Code, errorEvent.File, 
				errorEvent.LineNumber, errorEvent.ColumnNumber);
		}

		/// <summary>
		/// Add the error/warning to the error list and potentially to the output window.
		/// </summary>
		private void AddToErrorList(
			BuildEventArgs errorEvent,
			string errorCode,
			string file,
			int line,
			int column)
		{
      //var project = _hierarchy as IVsUIHierarchy;
      file = System.IO.Path.GetFullPath(file);
      System.Diagnostics.Trace.Assert(System.IO.File.Exists(file));

			TaskPriority priority = (errorEvent is BuildErrorEventArgs) 
				? TaskPriority.High : TaskPriority.Normal;

			if (OutputWindowPane != null)
			{
				// Format error and output it to the output window
				string message = FormatMessage(errorEvent.Message);
				CompilerError e = new CompilerError(file, line, column, errorCode, message);
				e.IsWarning = errorEvent is BuildWarningEventArgs;

				Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(
					OutputWindowPane.OutputStringThreadSafe(GetFormattedErrorMessage(e)));
			}

			// Add error to task list
			ErrorTask task = new ErrorTask();
			task.Document = file;
			task.Line = line - 1; // The task list does +1 before showing this number.
			task.Column = column;
			task.Text = errorEvent.Message;
			task.Priority = priority;
			task.Category = TaskCategory.BuildCompile;
			task.HierarchyItem = _hierarchy;
			task.Navigate += new EventHandler(NavigateTo);
			if (errorEvent is BuildWarningEventArgs)
				task.ErrorCategory = TaskErrorCategory.Warning;
			_taskProvider.Tasks.Add(task);
		}


		/// <summary>
		/// This is the delegate for Message event types
		/// </summary>		
		private void MessageHandler(object sender, BuildMessageEventArgs messageEvent)
		{
			if (LogAtImportance(messageEvent.Importance))
				LogEvent(sender, messageEvent);
		}

		private void NavigateTo(object sender, EventArgs arguments)
		{
			Microsoft.VisualStudio.Shell.Task task = 
				sender as Microsoft.VisualStudio.Shell.Task;

			if (task == null)
				throw new ArgumentException("sender");

			// Get the doc data for the task's document
			if (String.IsNullOrEmpty(task.Document))
				return;

			IVsUIShellOpenDocument openDoc = _serviceProvider.GetService(
				typeof(IVsUIShellOpenDocument)) as IVsUIShellOpenDocument;

			if (openDoc == null)
				return;

			IVsWindowFrame frame;
			IOleServiceProvider sp;
			IVsUIHierarchy hier;
			uint itemid;
			Guid logicalView = VSConstants.LOGVIEWID_Code;

			if (ErrorHandler.Failed(openDoc.OpenDocumentViaProject(
				task.Document, ref logicalView, out sp, out hier, out itemid, out frame))
				|| frame == null
			)
				return;

			object docData;
			frame.GetProperty((int)__VSFPROPID.VSFPROPID_DocData, out docData);

			// Get the VsTextBuffer
			VsTextBuffer buffer = docData as VsTextBuffer;
			if (buffer == null)
			{
				IVsTextBufferProvider bufferProvider = docData as IVsTextBufferProvider;
				if (bufferProvider != null)
				{
					IVsTextLines lines;
					ErrorHandler.ThrowOnFailure(bufferProvider.GetTextBuffer(out lines));
					buffer = lines as VsTextBuffer;
					Debug.Assert(buffer != null, "IVsTextLines does not implement IVsTextBuffer");

					if (buffer == null)
						return;
				}
			}

			// Finally, perform the navigation.
			IVsTextManager mgr = _serviceProvider.GetService(
				typeof(VsTextManagerClass)) as IVsTextManager;

			if (mgr == null)
				return;

			mgr.NavigateToLineAndColumn(buffer, ref logicalView, 
				task.Line, task.Column, task.Line, task.Column);
		}

		System.Diagnostics.Stopwatch _timer;

		/// <summary>
		/// This is the delegate for BuildStartedHandler events.
		/// </summary>
		private void BuildStartedHandler(object sender, BuildStartedEventArgs buildEvent)
		{
			if (LogAtImportance(MessageImportance.Low))
				LogEvent(sender, buildEvent);

			// Remove all errors and warnings since we are rebuilding
			_taskProvider.Tasks.Clear();

			_timer = System.Diagnostics.Stopwatch.StartNew();
		}

		/// <summary>
		/// This is the delegate for BuildFinishedHandler events.
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="buildEvent"></param>
		private void BuildFinishedHandler(object sender, BuildFinishedEventArgs buildEvent)
		{
			//LogEvent(sender, buildEvent);
			if (OutputWindowPane != null && !String.IsNullOrEmpty(buildEvent.Message))
			{
				StringBuilder msg = new StringBuilder(_currentIndent + buildEvent.Message.Length + 1);
				if (_currentIndent > 0)
					msg.Append('\t', _currentIndent);

				msg.Append(_buildTargetName ?? "Build");
				msg.Append(buildEvent.Succeeded ? " succeeded -- " : " failed -- ");

				if (!buildEvent.Succeeded)
					msg.Append(TaskCount(TaskErrorCategory.Error) + " errors, ");

				msg.AppendLine(TaskCount(TaskErrorCategory.Warning) + " warnings. Build took: "
					+ _timer.Elapsed + ".");

				OutputWindowPane.OutputStringThreadSafe(msg.ToString());
			}
		}

		public string BuildTargetName
		{
			get { return _buildTargetName; } set { _buildTargetName = value; }
		}
		string _buildTargetName;

		/// <summary>
		/// This is the delegate for ProjectStartedHandler events.
		/// </summary>
		private void ProjectStartedHandler(object sender, ProjectStartedEventArgs buildEvent)
		{
			if (LogAtImportance(MessageImportance.Low))
				LogEvent(sender, buildEvent);
		}

		/// <summary>
		/// This is the delegate for ProjectFinishedHandler events.
		/// </summary>
		private void ProjectFinishedHandler(object sender, ProjectFinishedEventArgs buildEvent)
		{
			if (LogAtImportance(buildEvent.Succeeded ? MessageImportance.Low : MessageImportance.High))
				LogEvent(sender, buildEvent);
		}

		/// <summary>
		/// This is the delegate for TargetStartedHandler events.
		/// </summary>
		private void TargetStartedHandler(object sender, TargetStartedEventArgs buildEvent)
		{
			if (LogAtImportance(MessageImportance.Normal))
				LogEvent(sender, buildEvent);

			++_currentIndent;
		}


		/// <summary>
		/// This is the delegate for TargetFinishedHandler events.
		/// </summary>
		private void TargetFinishedHandler(object sender, TargetFinishedEventArgs buildEvent)
		{
			--_currentIndent;
			if ((_isLogTaskDone) && LogAtImportance(buildEvent.Succeeded
				? MessageImportance.Low : MessageImportance.High)
			)
				LogEvent(sender, buildEvent);
		}


		/// <summary>
		/// This is the delegate for TaskStartedHandler events.
		/// </summary>
		private void TaskStartedHandler(object sender, TaskStartedEventArgs buildEvent)
		{
			if (LogAtImportance(MessageImportance.Normal))
				LogEvent(sender, buildEvent);

			++_currentIndent;
		}


		/// <summary>
		/// This is the delegate for TaskFinishedHandler events.
		/// </summary>
		private void TaskFinishedHandler(object sender, TaskFinishedEventArgs buildEvent)
		{
			--_currentIndent;
			if (_isLogTaskDone && LogAtImportance(buildEvent.Succeeded 
					? MessageImportance.Normal : MessageImportance.High))
			{
				LogEvent(sender, buildEvent);
			}
		}


		/// <summary>
		/// This is the delegate for CustomHandler events.
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="buildEvent"></param>
		private void CustomHandler(object sender, CustomBuildEventArgs buildEvent)
		{
			LogEvent(sender, buildEvent);
		}

		#endregion

		#region helpers
		/// <summary>
		/// This method takes a MessageImportance and returns true if messages
		/// at importance i should be loggeed.  Otherwise return false.
		/// </summary>
		private bool LogAtImportance(MessageImportance importance)
		{
			// If importance is too low for current settings, ignore the event
			bool logIt = false;

			SetVerbosity();

			switch (Verbosity)
			{
				case LoggerVerbosity.Quiet:
					logIt = false;
					break;
				case LoggerVerbosity.Minimal:
					logIt = (importance == MessageImportance.High);
					break;
				case LoggerVerbosity.Normal:
				// Falling through...
				case LoggerVerbosity.Detailed:
					logIt = (importance != MessageImportance.Low);
					break;
				case LoggerVerbosity.Diagnostic:
					logIt = true;
					break;
				default:
					Debug.Fail("Unknown Verbosity level. Ignoring will cause everything to be logged");
					break;
			}

			return logIt;
		}

		/// <summary>
		/// This is the method that does the main work of logging an event
		/// when one is sent to this logger.
		/// </summary>
		private void LogEvent(object sender, BuildEventArgs buildEvent)
		{
			// Fill in the Message text
			if (OutputWindowPane != null && !String.IsNullOrEmpty(buildEvent.Message))
			{
				StringBuilder msg = new StringBuilder(_currentIndent + buildEvent.Message.Length + 1);
				if (_currentIndent > 0)
					msg.Append('\t', _currentIndent);

				string txt = buildEvent.Message;

				msg.AppendLine(txt);

				OutputWindowPane.OutputStringThreadSafe(msg.ToString());
			}
		}

		int TaskCount(TaskErrorCategory type)
		{
			int count = 0;
			foreach (object task in _taskProvider.Tasks)
			{
				ErrorTask err = task as ErrorTask;

				if (err != null && err.ErrorCategory == type)
					count++;
			}

			return count;
		}

		/// <summary>
		/// This is called when the build complete.
		/// </summary>
		private void ShutdownLogger()
		{
		}


		/// <summary>
		/// Format error messages for the task list
		/// </summary>
		/// <param name="e"></param>
		/// <returns></returns>
		private string GetFormattedErrorMessage(CompilerError e)
		{
			if (e == null) return String.Empty;

			string errCode = e.IsWarning ? _warningString : _errorString;
			StringBuilder fileRef = new StringBuilder();

			if (!string.IsNullOrEmpty(e.FileName))
				fileRef.AppendFormat("{0}({1},{2}):", e.FileName, e.Line, e.Column);

			if (string.IsNullOrEmpty(e.ErrorNumber))
				fileRef.AppendFormat("{0}: {1}", errCode, e.ErrorText);
			else
				fileRef.AppendFormat(CultureInfo.CurrentUICulture, "{0} {1}: {2}",
					errCode, e.ErrorNumber, e.ErrorText);

			return fileRef.ToString();
		}

		/// <summary>
		/// Formats the message that is to be output.
		/// </summary>
		/// <param name="message">The message string.</param>
		/// <returns>The new message</returns>
		private string FormatMessage(string message)
		{
			return (message + Environment.NewLine).Replace('`', '\'');
		}

		/// <summary>
		/// Sets the verbosity level.
		/// </summary>
		private void SetVerbosity()
		{
			// TODO: This should be replaced when we have a version that supports automation.

			string xxx = Environment.CommandLine;

			string verbosityKey = String.Format(@"{0}\{1}", BuildVerbosityRegistryRoot, BuildVerbosityRegistrySubKey);
			using (RegistryKey subKey = Registry.CurrentUser.OpenSubKey(verbosityKey))
			{
				if (subKey != null)
				{
					object valueAsObject = subKey.GetValue(BuildVerbosityRegistryKey);
					if (valueAsObject != null)
						Verbosity = (LoggerVerbosity)((int)valueAsObject);
				}
			}

			// TODO: Continue this code to get the Verbosity 
			// when we have a version that supports automation to get the Verbosity.
			//EnvDTE.DTE dte = _serviceProvider.GetService(typeof(EnvDTE.DTE)) as EnvDTE.DTE;
			//EnvDTE.Properties properties = dte.get_Properties(EnvironmentCategory, 
			// ProjectsAndSolutionSubCategory);
		}

		#endregion
	}

}
