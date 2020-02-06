using System;
using Microsoft.VisualStudio.Project;

using MSBuild = Microsoft.Build.BuildEngine;
//using ComInteropHelpers;
using Microsoft.VisualStudio.Shell.Interop;
using System.Runtime.InteropServices;
using System.IO;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio;
using System.Diagnostics;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Этот класс замазывает баги MS. В данном случае наследник ProjectConfig
	/// определен для того, чтобы изменить поведение метода GetConfigurationProperty().
	/// В оригинальной версии он возвращает "раскрытое" значение свойства 
	/// MSBuild в то время как для заполнения страницы свойств требуется исходное
	/// значение. Термин "раскрытое" означет, что если, например, свойство содержит
	/// значение "$(MSBuildProjectDirectory)\some.exe", то при открытии страницы свойсв
	/// в поле будет полный путь к some.exe(ссылка на свойство MSBuild будет удалена).
	/// </summary>
	public class NemerleProjectConfig : ProjectConfig
	{
		public NemerleProjectConfig(ProjectNode project, string canonicalName)
			: base(project, canonicalName)
		{
		}

		object _configurationProperties;

		public override object ConfigurationProperties
		{
			get
			{
				if (_configurationProperties == null)
					_configurationProperties = new NemerleProjectConfigProperties(this);

				return _configurationProperties;
			}
		}

		/// <summary>
		/// Called by the vs shell to start debugging (managed or unmanaged).
		/// Override this method to support other debug engines.
		/// </summary>
		/// <param name="grfLaunch">A flag that determines the conditions under which to start the debugger. For valid grfLaunch values, see __VSDBGLAUNCHFLAGS</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code</returns>
		public override int DebugLaunch(uint grfLaunch)
		{
			CCITracing.TraceCall();

			try
			{
				VsDebugTargetInfo info = new VsDebugTargetInfo();
				info.cbSize = (uint)Marshal.SizeOf(info);
				info.dlo = Microsoft.VisualStudio.Shell.Interop.DEBUG_LAUNCH_OPERATION.DLO_CreateProcess;

				// On first call, reset the cache, following calls will use the cached values
				string property = GetConfigurationProperty("StartProgram", true);

				if (string.IsNullOrEmpty(property))
					info.bstrExe = ProjectMgr.GetOutputAssembly(this.ConfigKey);
				else
					info.bstrExe = property;

				property = GetConfigurationProperty("WorkingDirectory", false);

				if (string.IsNullOrEmpty(property))
					info.bstrCurDir = Path.GetDirectoryName(info.bstrExe);
				else
				{
					if (Path.IsPathRooted(property))
						info.bstrCurDir = property;
					else
					{
						var path = Path.Combine(ProjectMgr.BaseURI.AbsoluteUrl, property);

						if (Directory.Exists(path))
							info.bstrCurDir = path;
						else
							info.bstrCurDir = property;
					}
				}

				property = GetConfigurationProperty("CmdArgs", false);

				if (!string.IsNullOrEmpty(property))
					info.bstrArg = property;

				property = GetConfigurationProperty("RemoteDebugMachine", false);

				if (property != null && property.Length > 0)
					info.bstrRemoteMachine = property;
				
				info.fSendStdoutToOutputWindow = 0;

				property = GetConfigurationProperty("EnableUnmanagedDebugging", false);
				
				if (property != null && string.Compare(property, "true", StringComparison.OrdinalIgnoreCase) == 0)
					//Set the unmanged debugger
					//TODO change to vsconstant when it is available in VsConstants (guidNativeOnlyEng was the old name, maybe it has got a new name)
					info.clsidCustom = new Guid("{3B476D35-A401-11D2-AAD4-00C04F990171}");
				else
					//Set the managed debugger
					info.clsidCustom = VSConstants.CLSID_ComPlusOnlyDebugEngine;

				info.grfLaunch = grfLaunch;
				LaunchDebugger(this.ProjectMgr.Site, info);
			}
			catch (Exception e)
			{
				var proj = ((NemerleProjectNode)ProjectMgr).ProjectInfo;
				proj.ShowMessage(e.Message, Nemerle.Completion2.MessageType.Error);

				return Marshal.GetHRForException(e);
			}

			return VSConstants.S_OK;
		}

		/// <devdoc>
		/// Launch the debugger.
		/// </devdoc>
		/// <param name="serviceProvider">The service provider.</param>
		/// <param name="info">A reference to a VsDebugTargetInfo object.</param>
		public static void LaunchDebugger(IServiceProvider serviceProvider, VsDebugTargetInfo info)
		{
			if (serviceProvider == null)
				throw new ArgumentException("serviceProvider");

			info.cbSize = (uint)Marshal.SizeOf(info);
			IntPtr ptr = Marshal.AllocCoTaskMem((int)info.cbSize);
			Marshal.StructureToPtr(info, ptr, false);

			try
			{
				IVsDebugger d = serviceProvider.GetService(typeof(IVsDebugger)) as IVsDebugger;

				if (d == null)
					throw new InvalidOperationException();

				ErrorHandler.ThrowOnFailure(d.LaunchDebugTargets(1, ptr));
			}
			finally
			{
				if (ptr != IntPtr.Zero)
				{
					Marshal.FreeCoTaskMem(ptr);
				}
			}
		}
	}
}
