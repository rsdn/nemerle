/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using MSBuild = Microsoft.Build.Evaluation;
using Microsoft.Build.Construction;
using System.Linq;

/* This file provides a basefunctionallity for IVsCfgProvider2.
   Instead of using the IVsProjectCfgEventsHelper object we have our own little sink and call our own helper methods
   similiar to the interface. But there is no real benefit in inheriting from the interface in the first place. 
   Using the helper object seems to be:  
    a) undocumented
    b) not really wise in the managed world
*/
namespace Microsoft.VisualStudio.Project
{
	[CLSCompliant(false)]
	[ComVisible(true)]
	public class ConfigProvider : IVsCfgProvider2, IVsProjectCfgProvider, IVsExtensibleObject
	{
		#region fields
		internal const string configString = " '$(Configuration)' == '{0}' ";
		internal const string AnyCPUPlatform = "Any CPU";
		internal const string x86Platform = "x86";

		private ProjectNode project;
		private EventSinkCollection cfgEventSinks = new EventSinkCollection();
		private List<KeyValuePair<KeyValuePair<string, string>, string>> newCfgProps = new List<KeyValuePair<KeyValuePair<string, string>, string>>();
		private Dictionary<string, ProjectConfig> configurationsList = new Dictionary<string, ProjectConfig>();
		#endregion

		#region Properties
		/// <summary>
		/// The associated project.
		/// </summary>
		protected ProjectNode ProjectMgr
		{
			get
			{
				return this.project;
			}
		}
		/// <summary>
		/// If the project system wants to add custom properties to the property group then 
		/// they provide us with this data.
		/// Returns/sets the [(<propName, propCondition>) <propValue>] collection
		/// </summary>
		[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
		public virtual List<KeyValuePair<KeyValuePair<string, string>, string>> NewConfigProperties
		{
			get
			{
				return newCfgProps;
			}
			set
			{
				newCfgProps = value;
			}
		}

		#endregion

		#region ctors
		public ConfigProvider(ProjectNode manager)
		{
			this.project = manager;
		}
		#endregion

		#region methods
		/// <summary>
		/// Creates new Project Configuartion objects based on the configuration name.
		/// </summary>
		/// <param name="configName">The name of the configuration</param>
		/// <returns>An instance of a ProjectConfig object.</returns>
		protected ProjectConfig GetProjectConfiguration(string configName)
		{
			// if we already created it, return the cached one
			if (configurationsList.ContainsKey(configName))
			{
				return configurationsList[configName];
			}

			ProjectConfig requestedConfiguration = CreateProjectConfiguration(configName);
			configurationsList.Add(configName, requestedConfiguration);

			return requestedConfiguration;
		}

		protected virtual ProjectConfig CreateProjectConfiguration(string configName)
		{
			return new ProjectConfig(this.project, configName);
		}

		#endregion

		#region IVsProjectCfgProvider methods
		/// <summary>
		/// Provides access to the IVsProjectCfg interface implemented on a project's configuration object. 
		/// </summary>
		/// <param name="projectCfgCanonicalName">The canonical name of the configuration to access.</param>
		/// <param name="projectCfg">The IVsProjectCfg interface of the configuration identified by szProjectCfgCanonicalName.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code. </returns>
		public virtual int OpenProjectCfg(string projectCfgCanonicalName, out IVsProjectCfg projectCfg)
		{
			if (projectCfgCanonicalName == null)
			{
				throw new ArgumentNullException("projectCfgCanonicalName");
			}

			projectCfg = null;

			// Be robust in release
			if (projectCfgCanonicalName == null)
			{
				return VSConstants.E_INVALIDARG;
			}


			Debug.Assert(this.project != null && this.project.BuildProject != null);

			string[] configs = GetPropertiesConditionedOn(ProjectFileConstants.Configuration);

			foreach (string config in configs)
			{
				if (String.Compare(config, projectCfgCanonicalName, StringComparison.OrdinalIgnoreCase) == 0)
				{
					projectCfg = this.GetProjectConfiguration(config);
					if (projectCfg != null)
					{
						return VSConstants.S_OK;
					}
					else
					{
						return VSConstants.E_FAIL;
					}
				}
			}

			return VSConstants.E_INVALIDARG;
		}

		/// <summary>
		/// Checks whether or not this configuration provider uses independent configurations. 
		/// </summary>
		/// <param name="usesIndependentConfigurations">true if independent configurations are used, false if they are not used. By default returns true.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int get_UsesIndependentConfigurations(out int usesIndependentConfigurations)
		{
			usesIndependentConfigurations = 1;
			return VSConstants.S_OK;
		}
		#endregion

		#region IVsCfgProvider2 methods
		/// <summary>
		/// Copies an existing configuration name or creates a new one. 
		/// </summary>
		/// <param name="name">The name of the new configuration.</param>
		/// <param name="cloneName">the name of the configuration to copy, or a null reference, indicating that AddCfgsOfCfgName should create a new configuration.</param>
		/// <param name="fPrivate">Flag indicating whether or not the new configuration is private. If fPrivate is set to true, the configuration is private. If set to false, the configuration is public. This flag can be ignored.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code. </returns>
		public virtual int AddCfgsOfCfgName(string name, string cloneName, int fPrivate)
		{
			// We need to QE/QS the project file
			if (!this.ProjectMgr.QueryEditProjectFile(false))
				throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);

			ProjectMgr.BuildProject.ReevaluateIfNecessary();

			var list = new List<ProjectPropertyGroupElement>(this.project.BuildProject.Xml.PropertyGroups);
			var dictionary = new Dictionary<string, ProjectPropertyGroupElement>(StringComparer.Ordinal);

			if (cloneName != null)
			{
				foreach (ProjectPropertyGroupElement element in list)
				{
					if (!string.IsNullOrEmpty(element.Condition))
					{
						var cfgNameAndPlatform = ProjectConfig.ConfigAndPlatformOfCondition(element.Condition);
						//ConfigCanonicalName name2 = ConfigCanonicalName.OfCondition(element.Condition);
						var platformName = ProjectConfig.GetPlatformName(cfgNameAndPlatform);
						if ((string.Compare(ProjectConfig.GetConfigName(cfgNameAndPlatform), cloneName, StringComparison.OrdinalIgnoreCase) == 0) && !dictionary.ContainsKey(platformName))
							dictionary.Add(platformName, element);
					}
				}
			}
			string[] platformsFromProject = this.GetPlatformsFromProject();
			if (platformsFromProject.Length == 0)
				platformsFromProject = new [] { string.Empty };

			foreach (string latform in platformsFromProject)
			{
				if (dictionary.Count <= 0 || dictionary.ContainsKey(latform))
				{
					//ConfigCanonicalName name3 = new ConfigCanonicalName(name, latform);
					ProjectPropertyGroupElement newConfig = null;
					if (dictionary.ContainsKey(latform))
					{
						newConfig = this.project.ClonePropertyGroup(dictionary[latform]);
						foreach (ProjectPropertyElement element3 in newConfig.Properties)
						{
							if (element3.Name.Equals("OutputPath", StringComparison.OrdinalIgnoreCase))
							{
								element3.Parent.RemoveChild(element3);
							}
						}
					}
					else
					{
						var msbuildPlatform = ProjectConfig.ToMSBuildPlatform(latform);
						this.PopulateEmptyConfig(ref newConfig);
						if (!string.IsNullOrEmpty(msbuildPlatform))
							newConfig.AddProperty("PlatformTarget", msbuildPlatform);
					}

					this.AddOutputPath(newConfig, name);
					newConfig.Condition = ProjectConfig.MakeMSBuildCondition(name, latform);
				}
			}
			
			NotifyOnCfgNameAdded(name);
			return VSConstants.S_OK;
		}

		/// <summary>
		/// Copies an existing platform name or creates a new one. 
		/// </summary>
		/// <param name="platformName">The name of the new platform.</param>
		/// <param name="clonePlatformName">The name of the platform to copy, or a null reference, indicating that AddCfgsOfPlatformName should create a new platform.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int AddCfgsOfPlatformName(string platformName, string clonePlatformName)
		{
			var msbuildPlatform = ProjectConfig.ToMSBuildPlatform(platformName);
			clonePlatformName = ProjectConfig.ToMSBuildPlatform(clonePlatformName);

			if (!this.ProjectMgr.QueryEditProjectFile(false))
				throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED); //0x8004000c

			ProjectMgr.BuildProject.ReevaluateIfNecessary();
			var propertyGroups = new List<ProjectPropertyGroupElement>(this.project.BuildProject.Xml.PropertyGroups);
			var dictionary = new Dictionary<string, ProjectPropertyGroupElement>(StringComparer.Ordinal);

			if (clonePlatformName != null)
			{
				foreach (ProjectPropertyGroupElement propertyGroup in propertyGroups)
				{
					if (!string.IsNullOrEmpty(propertyGroup.Condition))
					{
						var cfgNameAndPlatform = ProjectConfig.ConfigAndPlatformOfCondition(propertyGroup.Condition);
						var cfgNme = ProjectConfig.GetConfigName(cfgNameAndPlatform);
						if (ProjectConfig.EqPlatform(cfgNameAndPlatform.Item2, clonePlatformName) && !dictionary.ContainsKey(cfgNme))
							dictionary.Add(cfgNme, propertyGroup);
					}
				}
			}

			string[] propertiesConditionedOn = this.GetPropertiesConditionedOn("Configuration");

			if (propertiesConditionedOn.Length == 0)
				return VSConstants.E_FAIL;

			foreach (string configName in propertiesConditionedOn)
			{
				if (dictionary.Count <= 0 || dictionary.ContainsKey(configName))
				{
					ProjectPropertyGroupElement newConfig = null;
					if (dictionary.ContainsKey(configName))
					{
						newConfig = this.project.ClonePropertyGroup(dictionary[configName]);

						foreach (ProjectPropertyElement property in newConfig.Properties)
							if (ProjectConfig.Eq(property.Name, "PlatformTarget") || ProjectConfig.Eq(property.Name, "Platform"))
								property.Parent.RemoveChild(property);
					}
					else
					{
						this.PopulateEmptyConfig(ref newConfig);
						this.AddOutputPath(newConfig, configName);
					}
					newConfig.AddProperty("PlatformTarget", msbuildPlatform);
					newConfig.AddProperty("Platform", msbuildPlatform);
					newConfig.Condition = ProjectConfig.MakeMSBuildCondition(configName, msbuildPlatform);
				}
			}

			NotifyOnPlatformNameAdded(platformName);

			return VSConstants.S_OK;
		}

		private void PopulateEmptyConfig(ref ProjectPropertyGroupElement newConfig)
		{
			newConfig = this.project.BuildProject.Xml.AddPropertyGroup();
			foreach (KeyValuePair<KeyValuePair<string, string>, string> pair in this.NewConfigProperties)
			{
				var key = pair.Key;
				var unevaluatedValue = pair.Value;
				var element = newConfig.AddProperty(key.Key, unevaluatedValue);

				if (!string.IsNullOrEmpty(key.Value))
					element.Condition = key.Value;
			}
		}

		private void AddOutputPath(ProjectPropertyGroupElement newConfig, string configName)
		{
			var outputBaseRelativePath = this.ProjectMgr.OutputBaseRelativePath;

			if (outputBaseRelativePath.EndsWith(Path.DirectorySeparatorChar.ToString(), StringComparison.Ordinal))
				outputBaseRelativePath = Path.GetDirectoryName(outputBaseRelativePath);

			newConfig.AddProperty("OutputPath", Path.Combine(outputBaseRelativePath, configName) + Path.DirectorySeparatorChar);
		}

		/// <summary>
		/// Deletes a specified configuration name. 
		/// </summary>
		/// <param name="name">The name of the configuration to be deleted.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code. </returns>
		public virtual int DeleteCfgsOfCfgName(string name)
		{
			// We need to QE/QS the project file
			if (!this.ProjectMgr.QueryEditProjectFile(false))
			{
				throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);
			}

			if (name == null)
			{
				Debug.Fail(String.Format(CultureInfo.CurrentCulture, "Name of the configuration should not be null if you want to delete it from project: {0}", this.project.BuildProject.FullPath));
				// The configuration " '$(Configuration)' ==  " does not exist, so technically the goal
				// is achieved so return S_OK
				return VSConstants.S_OK;
			}
			// Verify that this config exist
			string[] configs = GetPropertiesConditionedOn(ProjectFileConstants.Configuration);
			foreach (string config in configs)
			{
				if (String.Compare(config, name, StringComparison.OrdinalIgnoreCase) == 0)
				{
					// Create condition of config to remove
					string condition = String.Format(CultureInfo.InvariantCulture, configString, config);

					foreach (ProjectPropertyGroupElement element in this.project.BuildProject.Xml.PropertyGroups)
					{
						if (String.Equals(element.Condition, condition, StringComparison.OrdinalIgnoreCase))
						{
							element.Parent.RemoveChild(element);
						}
					}

					NotifyOnCfgNameDeleted(name);
				}
			}

			return VSConstants.S_OK;
		}

		/// <summary>
		/// Deletes a specified platform name. 
		/// </summary>
		/// <param name="platName">The platform name to delet.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int DeleteCfgsOfPlatformName(string platName)
		{
			if (!this.ProjectMgr.QueryEditProjectFile(false))
				throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);

			var platform = ProjectConfig.ToMSBuildPlatform(platName);

			if (platform != null)
			{
				ProjectMgr.BuildProject.ReevaluateIfNecessary();
				var inputElements = new List<ProjectPropertyGroupElement>(this.project.BuildProject.Xml.PropertyGroups);
				var elementsToRemove = new List<ProjectPropertyGroupElement>();

				foreach (ProjectPropertyGroupElement element in inputElements)
				{
					var cfgNameAndPlatform = ProjectConfig.ConfigAndPlatformOfCondition(element.Condition);

					if (ProjectConfig.EqPlatform(cfgNameAndPlatform.Item2, platform))
					{
						elementsToRemove.Add(element);
						this.configurationsList.Remove(ProjectConfig.MakeConfigKey(cfgNameAndPlatform.Item1, cfgNameAndPlatform.Item2));
					}
				}

				foreach (ProjectPropertyGroupElement element2 in elementsToRemove)
					element2.Parent.RemoveChild(element2);

				NotifyOnPlatformNameDeleted(platform);
			}

			return VSConstants.S_OK;
		}

		/// <summary>
		/// Returns the existing configurations stored in the project file.
		/// </summary>
		/// <param name="celt">Specifies the requested number of property names. If this number is unknown, celt can be zero.</param>
		/// <param name="names">On input, an allocated array to hold the number of configuration property names specified by celt. This parameter can also be a null reference if the celt parameter is zero. 
		/// On output, names contains configuration property names.</param>
		/// <param name="actual">The actual number of property names returned.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int GetCfgNames(uint celt, string[] names, uint[] actual)
		{
			// get's called twice, once for allocation, then for retrieval            
			int i = 0;

			string[] configList = GetPropertiesConditionedOn(ProjectFileConstants.Configuration);

			if (configList.Length == 0)
				configList = new string[] { "Debug" };

			if (names != null)
			{
				foreach (string config in configList)
				{
					names[i++] = config;
					if (i == celt)
						break;
				}
			}
			else
				i = configList.Length;

			if (actual != null)
			{
				actual[0] = (uint)i;
			}

			return VSConstants.S_OK;
		}

		/// <summary>
		/// Returns the configuration associated with a specified configuration or platform name. 
		/// </summary>
		/// <param name="name">The name of the configuration to be returned.</param>
		/// <param name="platName">The name of the platform for the configuration to be returned.</param>
		/// <param name="cfg">The implementation of the IVsCfg interface.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int GetCfgOfName(string name, string platName, out IVsCfg cfg)
		{
			cfg = this.GetProjectConfiguration(ProjectConfig.MakeConfigKey(name, platName));

			return VSConstants.S_OK;
		}

		/// <summary>
		/// Returns a specified configuration property. 
		/// </summary>
		/// <param name="propid">Specifies the property identifier for the property to return. For valid propid values, see __VSCFGPROPID.</param>
		/// <param name="var">The value of the property.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int GetCfgProviderProperty(int propid, out object var)
		{
			var = false;
			switch ((__VSCFGPROPID)propid)
			{
				case __VSCFGPROPID.VSCFGPROPID_SupportsCfgAdd:
				case __VSCFGPROPID.VSCFGPROPID_SupportsCfgDelete:
				case __VSCFGPROPID.VSCFGPROPID_SupportsCfgRename:
				case __VSCFGPROPID.VSCFGPROPID_SupportsPlatformAdd:
				case __VSCFGPROPID.VSCFGPROPID_SupportsPlatformDelete:
					var = true;
					break;
			}
			return VSConstants.S_OK;
		}

		/// <summary>
		/// Returns the per-configuration objects for this object. 
		/// </summary>
		/// <param name="celt">Number of configuration objects to be returned or zero, indicating a request for an unknown number of objects.</param>
		/// <param name="a">On input, pointer to an interface array or a null reference. On output, this parameter points to an array of IVsCfg interfaces belonging to the requested configuration objects.</param>
		/// <param name="actual">The number of configuration objects actually returned or a null reference, if this information is not necessary.</param>
		/// <param name="flags">Flags that specify settings for project configurations, or a null reference (Nothing in Visual Basic) if no additional flag settings are required. For valid prgrFlags values, see __VSCFGFLAGS.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int GetCfgs(uint celt, IVsCfg[] a, uint[] actual, uint[] flags)
		{
			if (celt == 1)
				Debug.WriteLine("GetCfgs(celt=1, ...)");

			if (flags != null)
				flags[0] = 0;

			int i = 0;
			string[] configList = GetPropertiesConditionedOn(ProjectFileConstants.Configuration);
			string[] platformList = GetPlatformsFromProject();

			if (configList.Length == 0)
				configList = new string[] { "Debug" };
			if (platformList.Length == 0)
				platformList = new string[] { "AnyCPU" };

			if (a != null)
			{
				foreach (string configName in configList)
				{
					foreach (string platform in platformList)
					{
						a[i] = this.GetProjectConfiguration(ProjectConfig.MakeConfigKey(configName, platform));

						i++;
						if (i == celt)
							break;
					}

					if (i == celt)
						break;
				}
			}
			else
				i = configList.Length * platformList.Length;

			if (actual != null)
				actual[0] = (uint)i;

			return VSConstants.S_OK;
		}

		/// <summary>
		/// Returns one or more platform names. 
		/// </summary>
		/// <param name="celt">Specifies the requested number of platform names. If this number is unknown, celt can be zero.</param>
		/// <param name="names">On input, an allocated array to hold the number of platform names specified by celt. This parameter can also be a null reference if the celt parameter is zero. On output, names contains platform names.</param>
		/// <param name="actual">The actual number of platform names returned.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int GetPlatformNames(uint celt, string[] names, uint[] actual)
		{
			string[] platforms = this.GetPlatformsFromProject();
			return GetPlatforms(celt, names, actual, platforms);
		}

		/// <summary>
		/// Returns the set of platforms that are installed on the user's machine. 
		/// </summary>
		/// <param name="celt">Specifies the requested number of supported platform names. If this number is unknown, celt can be zero.</param>
		/// <param name="names">On input, an allocated array to hold the number of names specified by celt. This parameter can also be a null reference (Nothing in Visual Basic)if the celt parameter is zero. On output, names contains the names of supported platforms</param>
		/// <param name="actual">The actual number of platform names returned.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int GetSupportedPlatformNames(uint celt, string[] names, uint[] actual)
		{
			string[] platforms = this.GetSupportedPlatformsFromProject();
			return GetPlatforms(celt, names, actual, platforms);
		}

		/// <summary>
		/// Assigns a new name to a configuration. 
		/// </summary>
		/// <param name="old">The old name of the target configuration.</param>
		/// <param name="newname">The new name of the target configuration.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int RenameCfgsOfCfgName(string old, string newname)
		{
			this.project.BuildProject.ReevaluateIfNecessary();
			foreach (ProjectPropertyGroupElement current in this.project.BuildProject.Xml.PropertyGroups)
			{
				if (string.IsNullOrEmpty(current.Condition))
					continue;

				var cfgNameAndPlatform = ProjectConfig.ConfigAndPlatformOfCondition(current.Condition);

				if (ProjectConfig.Eq(ProjectConfig.GetConfigName(cfgNameAndPlatform), old))
				{
					//ConfigCanonicalName key2 = new ConfigCanonicalName(newname, key.Platform);
					current.Condition = ProjectConfig.MakeMSBuildCondition(newname, cfgNameAndPlatform.Item2);

					var outputPath = current.Properties.Where(p => p.Name == "OutputPath").FirstOrDefault();

					if (outputPath != null && outputPath.Value != null)
					{
						string path = this.ProjectMgr.OutputBaseRelativePath;
						if (path.EndsWith(Path.DirectorySeparatorChar.ToString(), StringComparison.Ordinal))
							path = Path.GetDirectoryName(path);

						if (string.Equals(Path.Combine(path, old), outputPath.Value, StringComparison.OrdinalIgnoreCase))
							current.SetProperty("OutputPath", Path.Combine(path, newname));
					}

					var oldKey = ProjectConfig.MakeConfigKey(cfgNameAndPlatform);

					if (this.configurationsList.ContainsKey(oldKey))
					{
						ProjectConfig projectConfig = this.configurationsList[oldKey];
						this.configurationsList.Remove(oldKey);
						this.configurationsList.Add(ProjectConfig.MakeConfigKey(newname, cfgNameAndPlatform.Item2), projectConfig);
						projectConfig.ConfigurationName = newname;
					}
				}
			}

			this.NotifyOnCfgNameRenamed(old, newname);
			//// First create the condition that represent the configuration we want to rename
			//string condition = String.Format(CultureInfo.InvariantCulture, configString, old).Trim();

			//foreach (ProjectPropertyGroupElement config in this.project.BuildProject.Xml.PropertyGroups)
			//{
			//  // Only care about conditional property groups
			//  if (config.Condition == null || config.Condition.Length == 0)
			//    continue;

			//  // Skip if it isn't the group we want
			//  if (String.Compare(config.Condition.Trim(), condition, StringComparison.OrdinalIgnoreCase) != 0)
			//    continue;

			//  // Change the name 
			//  config.Condition = String.Format(CultureInfo.InvariantCulture, configString, newname);
			//  // Update the name in our config list
			//  if (configurationsList.ContainsKey(old))
			//  {
			//    ProjectConfig configuration = configurationsList[old];
			//    configurationsList.Remove(old);
			//    configurationsList.Add(newname, configuration);
			//    // notify the configuration of its new name
			//    configuration.ConfigurationName = newname;
			//  }

			//  NotifyOnCfgNameRenamed(old, newname);
			//}

			return VSConstants.S_OK;
		}

		/// <summary>
		/// Cancels a registration for configuration event notification. 
		/// </summary>
		/// <param name="cookie">The cookie used for registration.</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int UnadviseCfgProviderEvents(uint cookie)
		{
			this.cfgEventSinks.RemoveAt(cookie);
			return VSConstants.S_OK;
		}

		/// <summary>
		/// Registers the caller for configuration event notification. 
		/// </summary>
		/// <param name="sink">Reference to the IVsCfgProviderEvents interface to be called to provide notification of configuration events.</param>
		/// <param name="cookie">Reference to a token representing the completed registration</param>
		/// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
		public virtual int AdviseCfgProviderEvents(IVsCfgProviderEvents sink, out uint cookie)
		{
			cookie = this.cfgEventSinks.Add(sink);
			return VSConstants.S_OK;
		}
		#endregion

		#region IVsExtensibleObject Members

		/// <summary>
		/// Proved access to an IDispatchable object being a list of configuration properties
		/// </summary>
		/// <param name="configurationName">Combined Name and Platform for the configuration requested</param>
		/// <param name="configurationProperties">The IDispatchcable object</param>
		/// <returns>S_OK if successful</returns>
		public virtual int GetAutomationObject(string configurationName, out object configurationProperties)
		{
			//Init out param
			configurationProperties = null;

			string name, platform;
			if (!ProjectConfig.TrySplitConfigurationCanonicalName(configurationName, out name, out platform))
				return VSConstants.E_INVALIDARG;

			// Get the configuration
			IVsCfg cfg;
			ErrorHandler.ThrowOnFailure(this.GetCfgOfName(name, ProjectConfig.ToPlatformName(platform), out cfg));

			// Get the properties of the configuration
			configurationProperties = ((ProjectConfig)cfg).ConfigurationProperties;

			return VSConstants.S_OK;
		}
		#endregion

		#region helper methods
		/// <summary>
		/// Called when a new configuration name was added.
		/// </summary>
		/// <param name="name">The name of configuration just added.</param>
		private void NotifyOnCfgNameAdded(string name)
		{
			foreach (IVsCfgProviderEvents sink in this.cfgEventSinks)
			{
				ErrorHandler.ThrowOnFailure(sink.OnCfgNameAdded(name));
			}
		}

		/// <summary>
		/// Called when a config name was deleted.
		/// </summary>
		/// <param name="name">The name of the configuration.</param>
		private void NotifyOnCfgNameDeleted(string name)
		{
			foreach (IVsCfgProviderEvents sink in this.cfgEventSinks)
			{
				ErrorHandler.ThrowOnFailure(sink.OnCfgNameDeleted(name));
			}
		}

		/// <summary>
		/// Called when a config name was renamed
		/// </summary>
		/// <param name="oldName">Old configuration name</param>
		/// <param name="newName">New configuration name</param>
		private void NotifyOnCfgNameRenamed(string oldName, string newName)
		{
			foreach (IVsCfgProviderEvents sink in this.cfgEventSinks)
			{
				ErrorHandler.ThrowOnFailure(sink.OnCfgNameRenamed(oldName, newName));
			}
		}

		/// <summary>
		/// Called when a platform name was added
		/// </summary>
		/// <param name="platformName">The name of the platform.</param>
		[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
		private void NotifyOnPlatformNameAdded(string platformName)
		{
			foreach (IVsCfgProviderEvents sink in this.cfgEventSinks)
			{
				ErrorHandler.ThrowOnFailure(sink.OnPlatformNameAdded(platformName));
			}
		}

		/// <summary>
		/// Called when a platform name was deleted
		/// </summary>
		/// <param name="platformName">The name of the platform.</param>
		[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
		private void NotifyOnPlatformNameDeleted(string platformName)
		{
			foreach (IVsCfgProviderEvents sink in this.cfgEventSinks)
			{
				ErrorHandler.ThrowOnFailure(sink.OnPlatformNameDeleted(platformName));
			}
		}

		/// <summary>
		/// Gets all the platforms defined in the project
		/// </summary>
		/// <returns>An array of platform names.</returns>
		private string[] GetPlatformsFromProject()
		{
			string[] platforms = GetPropertiesConditionedOn(ProjectFileConstants.Platform);

			if (platforms == null || platforms.Length == 0)
				return new string[] { x86Platform, AnyCPUPlatform };

			return platforms.Select(ProjectConfig.ToPlatformName).Distinct().ToArray();
		}

		/// <summary>
		/// Return the supported platform names.
		/// </summary>
		/// <returns>An array of supported platform names.</returns>
		private string[] GetSupportedPlatformsFromProject()
		{
			string platforms = this.ProjectMgr.BuildProject.GetPropertyValue(ProjectFileConstants.AvailablePlatforms);

			if (platforms == null)
			{
				return new string[] { };
			}

			if (platforms.Contains(","))
			{
				return platforms.Split(',');
			}

			return new string[] { platforms };
		}

		/// <summary>
		/// Helper function to convert AnyCPU to Any CPU.
		/// </summary>
		/// <param name="oldName">The oldname.</param>
		/// <returns>The new name.</returns>
		private static string ConvertPlatformToVsProject(string oldPlatformName)
		{
			if (String.Compare(oldPlatformName, ProjectFileValues.AnyCPU, StringComparison.OrdinalIgnoreCase) == 0)
			{
				return AnyCPUPlatform;
			}

			return oldPlatformName;
		}

		/// <summary>
		/// Common method for handling platform names.
		/// </summary>
		/// <param name="celt">Specifies the requested number of platform names. If this number is unknown, celt can be zero.</param>
		/// <param name="names">On input, an allocated array to hold the number of platform names specified by celt. This parameter can also be null if the celt parameter is zero. On output, names contains platform names</param>
		/// <param name="actual">A count of the actual number of platform names returned.</param>
		/// <param name="platforms">An array of available platform names</param>
		/// <returns>A count of the actual number of platform names returned.</returns>
		/// <devremark>The platforms array is never null. It is assured by the callers.</devremark>
		private static int GetPlatforms(uint celt, string[] names, uint[] actual, string[] platforms)
		{
			Debug.Assert(platforms != null, "The plaforms array should never be null");
			if (names == null)
			{
				if (actual == null || actual.Length == 0)
				{
					throw new ArgumentException(SR.GetString(SR.InvalidParameter, CultureInfo.CurrentUICulture), "actual");
				}

				actual[0] = (uint)platforms.Length;
				return VSConstants.S_OK;
			}

			//Degenarate case
			if (celt == 0)
			{
				if (actual != null && actual.Length != 0)
				{
					actual[0] = (uint)platforms.Length;
				}

				return VSConstants.S_OK;
			}

			uint returned = 0;
			for (int i = 0; i < platforms.Length && names.Length > returned; i++)
			{
				names[returned] = platforms[i];
				returned++;
			}

			if (actual != null && actual.Length != 0)
			{
				actual[0] = returned;
			}

			if (celt > returned)
			{
				return VSConstants.S_FALSE;
			}

			return VSConstants.S_OK;
		}
		#endregion

		/// <summary>
		/// Get all the configurations in the project.
		/// </summary>
		private string[] GetPropertiesConditionedOn(string constant)
		{
			List<string> configurations = null;
			this.project.BuildProject.ReevaluateIfNecessary();
			this.project.BuildProject.ConditionedProperties.TryGetValue(constant, out configurations);

			return (configurations == null) ? new string[] { } : configurations.ToArray();
		}

	}
}