using System;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;

using Microsoft.Build.Framework;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System.Text;
using System.Globalization;
using System.IO;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// This class implements an MSBuild logger that output events to 
	/// VS outputwindow and tasklist.
	/// </summary>
	[ComVisible(true)]
	internal class NemerleIdeBuildLogger: IDEBuildLogger
	{
		private Stopwatch _timer;
		private readonly TaskProvider _taskProvider;
		private int _errorCount;
		private int _warningCount;

		public NemerleIdeBuildLogger(IVsOutputWindowPane output, TaskProvider taskProvider, IVsHierarchy hierarchy)
			: base(output, taskProvider, hierarchy)
		{
			_taskProvider = taskProvider;
		}

		enum MsgKind
		{
			Error,
			Warning,
			Message
		}

		/// <summary>Format error messages for the task list</summary>
		private string GetFormattedErrorMessage(
				string fileName,
				int line,
				int column,
				MsgKind kind,
				string errorNumber,
				string errorText)
		{
			string errorCode = "";

			switch (kind)
			{
				case MsgKind.Error:   errorCode = this.ErrorString;   break;
				case MsgKind.Warning: errorCode = this.WarningString; break;
				default:              errorCode = "";                 break;
			}
			
			var message = new StringBuilder();
			if (!string.IsNullOrEmpty(fileName))
				message.AppendFormat(CultureInfo.CurrentCulture, "{0}({1},{2}):", fileName, line, column);

			message.AppendFormat(CultureInfo.CurrentCulture, " {0} {1}: {2}", errorCode, errorNumber, errorText);
			message.AppendLine();

			return message.ToString();
		}

		protected override void WarningHandler(object sender, BuildWarningEventArgs warningEvent)
		{
			++_warningCount;
			QueueOutputText(GetFormattedErrorMessage(Path.GetFullPath(warningEvent.File), 
				warningEvent.LineNumber, warningEvent.ColumnNumber, MsgKind.Warning, warningEvent.Code, warningEvent.Message));
		}

		protected override void ErrorHandler(object sender, BuildErrorEventArgs errorEvent)
		{
			++_errorCount;
			QueueOutputText(GetFormattedErrorMessage(Path.GetFullPath(errorEvent.File),
				errorEvent.LineNumber, errorEvent.ColumnNumber, MsgKind.Error, errorEvent.Code, errorEvent.Message));
		}

		protected override void MessageHandler(object sender, BuildMessageEventArgs messageEvent)
		{
			if (messageEvent.Importance == MessageImportance.High && messageEvent is TaskCommandLineEventArgs)
			{
				QueueOutputEvent(MessageImportance.Normal, messageEvent);
				return;
			}
			base.MessageHandler(sender, messageEvent);
		}

		/// <summary>
		/// This is the delegate for BuildStartedHandler events.
		/// </summary>
		protected override void BuildStartedHandler(object sender, BuildStartedEventArgs buildEvent)
		{
			base.BuildStartedHandler(sender, buildEvent);

			_errorCount = 0;
			_warningCount = 0;
			_timer = Stopwatch.StartNew();
		}

		/// <summary>
		/// This is the delegate for BuildFinishedHandler events.
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="buildEvent"></param>
		protected override void BuildFinishedHandler(object sender, BuildFinishedEventArgs buildEvent)
		{
			if (_errorCount > 0 || _warningCount > 0)
			{
				var msgBuffer = new StringBuilder(Environment.NewLine);
				if (_errorCount > 0)
					msgBuffer.Append(_errorCount).Append(_errorCount == 1 ? " error, " : " errors, ");
				msgBuffer.Append(_warningCount).AppendLine(_warningCount == 1 ? " warning." : " warnings.");
				QueueOutputText(MessageImportance.High, msgBuffer.ToString());
			}

			base.BuildFinishedHandler(sender, buildEvent);

			QueueOutputText(MessageImportance.High, Environment.NewLine + "Time Elapsed " + _timer.Elapsed + Environment.NewLine);
		}
	}
}
