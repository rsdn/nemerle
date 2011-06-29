using Microsoft.VisualStudio.Project;
using System;
using System.Runtime.InteropServices;

namespace Nemerle.VisualStudio.Project
{
	[ComVisible(true)]
	[Guid("FD22DD9F-592E-41b1-B6D6-135B78E10809")]
	[InterfaceType(ComInterfaceType.InterfaceIsIDispatch)]
	public interface IProjectConfigProperties
	{
		[DispId(1)]
		string OutputPath { get; set; }
	}

	[CLSCompliant(false), ComVisible(true)]
	[ClassInterface(ClassInterfaceType.None)]
	public class NemerleProjectConfigProperties : IProjectConfigProperties
	{
		#region fields
		private ProjectConfig _projectConfig;
		#endregion

		#region ctors
		public NemerleProjectConfigProperties(ProjectConfig projectConfig)
		{
			_projectConfig = projectConfig;
		}
		#endregion

		#region IProjectConfigProperties Members

		public string OutputPath
		{
			get
			{
				return _projectConfig.GetConfigurationProperty(
					BuildPropertyPageTag.OutputPath.ToString(), true);
			}
			set
			{
				_projectConfig.SetConfigurationProperty(
					BuildPropertyPageTag.OutputPath.ToString(), value);
			}
		}

		#endregion
	}
}
