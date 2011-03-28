using System;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;

using Microsoft.Build.Framework;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

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

		public string BuildTargetName { get; set; }

		public NemerleIdeBuildLogger(IVsOutputWindowPane output, TaskProvider taskProvider, IVsHierarchy hierarchy)
			: base(output, taskProvider, hierarchy)
		{
			_taskProvider = taskProvider;
		}

		/// <summary>
		/// This is the delegate for BuildStartedHandler events.
		/// </summary>
		protected override void BuildStartedHandler(object sender, BuildStartedEventArgs buildEvent)
		{
			base.BuildStartedHandler(sender, buildEvent);

			_timer = Stopwatch.StartNew();
		}

		/// <summary>
		/// This is the delegate for BuildFinishedHandler events.
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="buildEvent"></param>
		protected override void BuildFinishedHandler(object sender, BuildFinishedEventArgs buildEvent)
		{
			base.BuildFinishedHandler(sender, buildEvent);

			var msg =
				(BuildTargetName ?? "Build") +
				(buildEvent.Succeeded ? " succeeded -- " : " failed -- ") +
				(!buildEvent.Succeeded ? TaskCount(TaskErrorCategory.Error) + " errors, " : "") +
				(TaskCount(TaskErrorCategory.Warning) + " warnings. ") +
				("Build took: " + _timer.Elapsed + ".") +
				Environment.NewLine;

			QueueOutputEvent(MessageImportance.High, new BuildFinishedEventArgs(msg, string.Empty, buildEvent.Succeeded));
		}

		int TaskCount(TaskErrorCategory type)
		{
			return
				_taskProvider.Tasks.OfType<ErrorTask>()
					.Count(task => task.ErrorCategory == type);
		}
	}
}
