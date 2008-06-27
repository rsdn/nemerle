using System;
using System.ComponentModel;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Package;

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

		//TODO: ѕроверить, что эта фигн€ возвращает то что нужно! 
		// ¬озможно здесь нужно собирать путь по кускам.
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

		#endregion
	}
}