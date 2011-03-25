using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project
{
	class NemerleProjectConfigProperties : ComInteropHelpers.IProjectConfigProperties
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
