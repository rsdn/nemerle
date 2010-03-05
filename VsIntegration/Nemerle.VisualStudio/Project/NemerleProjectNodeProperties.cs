using System;
using System.ComponentModel;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project
{
	[ComVisible(true)]
	[CLSCompliant(false)]
	[Guid(NemerleConstants.ProjectNodePropertiesGuidString)]
	[ClassInterface(ClassInterfaceType.AutoDual)]
	public class NemerleProjectNodeProperties : ProjectNodeProperties
	{
		#region ctors

		public NemerleProjectNodeProperties(ProjectNode node) : base(node) {}

		#endregion

		#region Properties

		public const string MainFilePropertyName = "MainFile";
		public const string ManifestKeyFilePropertyName = "ManifestKeyFile";
		public const string ManifestCertificateThumbprintPropertyName = "ManifestCertificateThumbprint";
		public const string OutputPathPropertyName = "OutputPath";

		private NemerleProjectNode ThisProjectMgr
		{
			get { return (NemerleProjectNode)(Node.ProjectMgr); }
		}

		[Browsable(false)]
		// Обход странного поведения студии. В Microsoft.VisualStudio.Shell.Design.Serialization.ConfigurationHelperService()
		// производится безусловное считывание значения свойства Company и AssemblyVersion
		public string Company
		{
			get { return null; }
		}

		//[SRCategoryAttribute("Misc")]
		//[LocDisplayName("AssemblyVersion")]
		//[SRDescriptionAttribute("Тестируем разную хрень 2")]
		[Browsable(false)]
		public string AssemblyVersion
		{
			get { return null; }
		}

		[Browsable(false)]
		public string OutputFileName
		{
			get { return ThisProjectMgr.OutputFileName; }
		}

		[Browsable(false)]
		public string MainFile
		{
			get { return Node.ProjectMgr.GetProjectProperty(MainFilePropertyName, true); }
		}

		[Browsable(false)]
		public string AssemblyName
		{
			get { return GetPropertyValue(ProjectFileConstants.AssemblyName); }
			set { Node.ProjectMgr.SetProjectProperty(ProjectFileConstants.AssemblyName, value); }
		}

		protected string GetPropertyValue(string propertyName)
		{
			var prop = Node.ProjectMgr.BuildProject.EvaluatedProperties[propertyName];

			if (prop == null)
				return "";

			return prop.Value;
		}

		//TODO: Проверить, что эта фигня возвращает то что нужно! 
		// Возможно здесь нужно собирать путь по кускам.
		[Browsable(false)]
		public string OutputPath
		{
			get { return GetPropertyValue(OutputPathPropertyName); }
			set { Node.ProjectMgr.SetProjectProperty(OutputPathPropertyName, value); }
		}

		[Browsable(false)]
		public string OutputBaseRelativePath
		{
			get { return ThisProjectMgr.OutputBaseRelativePath; }
			set { ThisProjectMgr.OutputBaseRelativePath = value; }
		}

		[Browsable(false)]
		public string DefaultNamespace
		{
			get { return Node.ProjectMgr.GetProjectProperty(ProjectFileConstants.RootNamespace); }
			set { Node.ProjectMgr.SetProjectProperty(ProjectFileConstants.RootNamespace, value); }
		}

		[Browsable(false)]
		public string RootNamespace
		{
			get { return GetPropertyValue(ProjectFileConstants.RootNamespace); }
			set { Node.ProjectMgr.SetProjectProperty(ProjectFileConstants.RootNamespace, value); }
		}

		[Browsable(false)]
		public string OutputType
		{
			get { return GetPropertyValue(ProjectFileConstants.OutputType); }
			set { Node.ProjectMgr.SetProjectProperty(ProjectFileConstants.OutputType, value); }
		}

		[Browsable(false)]
		public string ManifestKeyFile
		{
			get { return GetPropertyValue(ManifestKeyFilePropertyName); }
			set { Node.ProjectMgr.SetProjectProperty(ManifestKeyFilePropertyName, value); }
		}

		[Browsable(false)]
		public string ManifestCertificateThumbprint
		{
			get { return GetPropertyValue(ManifestCertificateThumbprintPropertyName); }
			set { Node.ProjectMgr.SetProjectProperty(ManifestCertificateThumbprintPropertyName, value); }
		}

		[Browsable(false)]
		// see AddProperty.NemerleOAProperties()
		public int WebApplication_DebugStartAction
		{ get; set; }

		[Browsable(false)]
		// see AddProperty.NemerleOAProperties()
		public string WebApplication_StartPageUrl
		{ get; set; }

		#endregion
	}
}
