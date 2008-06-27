using System;
using Microsoft.VisualStudio.Package;

using MSBuild = Microsoft.Build.BuildEngine;
using ComInteropHelpers;

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
		public NemerleProjectConfig(ProjectNode project, string configuration)
			: base(project, configuration)
		{
		}

		// Аналогичено поле в ProjectConfig объявлено скрытым, так что дублируем его.
		private MSBuild.BuildPropertyGroup _currentConfig;

		/// <summary>
		/// Get (not evaluated) value MsBuild property by name. 
		/// Use cached configuration if resetCache if false.
		/// </summary>
		/// <param name="propertyName">Name of MSBuild property to retrieve</param>
		/// <param name="resetCache">True if need reed fresh information from project file</param>
		/// <returns>String value of MSBuild property</returns>
		public override string GetConfigurationProperty(string propertyName, bool resetCache)
		{
			MSBuild.BuildProperty property = GetMsBuildProperty(propertyName, resetCache);
			if (property == null)
				return null;

			return property.FinalValue;
		}

		// Аналогиченая фукнция в ProjectConfig объявлена скрытой, так что дублируем ее.
		/// <summary>
		/// Get MsBuild property by name. Use cached configuration if resetCache if false.
		/// </summary>
		/// <param name="propertyName">Name of MSBuild property to retrieve</param>
		/// <param name="resetCache">True if need reed fresh information from project file</param>
		/// <returns>MSBuild property</returns>
 		public MSBuild.BuildProperty GetMsBuildProperty(string propertyName, bool resetCache)
		{
			if (resetCache || _currentConfig == null)
			{
				// Get properties for current configuration from project file and cache it
				ProjectMgr.SetConfiguration(this.ConfigName);
				_currentConfig = ProjectMgr.BuildProject.EvaluatedProperties;

				ProjectMgr.SetCurrentConfiguration();
			}

			if (_currentConfig == null)
				throw new Exception("Failed to retrive properties");

			// return property asked for
			return _currentConfig[propertyName];
		}

		// Переопределяется чтобы сбросить значение кэша конфигурации (_currentConfig).
		public override void SetConfigurationProperty(string propertyName, string propertyValue)
		{
			base.SetConfigurationProperty(propertyName, propertyValue);
			_currentConfig = null;
		}

		object _configurationProperties;

		public override object ConfigurationProperties
		{
			get
			{
				if (_configurationProperties == null)
					_configurationProperties = new ProjectConfigPropertiesComWrapper(
						new NemerleProjectConfigProperties(this));

				return _configurationProperties;
			}
		}
	}
}
