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
	public class NemerleProjectNodeProperties : ProjectNodeProperties, VSLangProj.ProjectProperties
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

		// Метод Microsoft.VisualStudio.Web.Application.WAProject.DoProjectLoadInitialization() прерывается по ошибке, если 
		// NemerleProjectNodeProperties не реализует интерфейс VSLangProj.ProjectProperties
		// От этого интерфейса нужно только одно свойства - ActiveConfigurationSettings
		//
		// Без успешной отработки DoProjectLoadInitialization не полностью работает поддержка веб-проектов,
		// например не подцепляются префиксы контролов, объявленные в web.config файле (секция <system.web><pages><controls>)
		#region ProjectProperties Members

		VSLangProj.ProjectConfigurationProperties VSLangProj.ProjectProperties.ActiveConfigurationSettings
		{
			get { return null; }
		}

		string VSLangProj.ProjectProperties.AbsoluteProjectDirectory
		{
			get { throw new NotImplementedException(); }
		}

		string VSLangProj.ProjectProperties.ActiveFileSharePath
		{
			get { throw new NotImplementedException(); }
		}

		VSLangProj.prjWebAccessMethod VSLangProj.ProjectProperties.ActiveWebAccessMethod
		{
			get { throw new NotImplementedException(); }
		}

		string VSLangProj.ProjectProperties.ApplicationIcon
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.AssemblyKeyContainerName
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.AssemblyName
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.AssemblyOriginatorKeyFile
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		VSLangProj.prjOriginatorKeyMode VSLangProj.ProjectProperties.AssemblyOriginatorKeyMode
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		VSLangProj.prjScriptLanguage VSLangProj.ProjectProperties.DefaultClientScript
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		VSLangProj.prjHTMLPageLayout VSLangProj.ProjectProperties.DefaultHTMLPageLayout
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.DefaultNamespace
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		VSLangProj.prjTargetSchema VSLangProj.ProjectProperties.DefaultTargetSchema
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		bool VSLangProj.ProjectProperties.DelaySign
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.ExtenderCATID
		{
			get { throw new NotImplementedException(); }
		}

		object VSLangProj.ProjectProperties.ExtenderNames
		{
			get { throw new NotImplementedException(); }
		}

		string VSLangProj.ProjectProperties.FileName
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.FileSharePath
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.FullPath
		{
			get { throw new NotImplementedException(); }
		}

		bool VSLangProj.ProjectProperties.LinkRepair
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.LocalPath
		{
			get { throw new NotImplementedException(); }
		}

		string VSLangProj.ProjectProperties.OfflineURL
		{
			get { throw new NotImplementedException(); }
		}

		VSLangProj.prjCompare VSLangProj.ProjectProperties.OptionCompare
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		VSLangProj.prjOptionExplicit VSLangProj.ProjectProperties.OptionExplicit
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		VSLangProj.prjOptionStrict VSLangProj.ProjectProperties.OptionStrict
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.OutputFileName
		{
			get { throw new NotImplementedException(); }
		}

		VSLangProj.prjOutputType VSLangProj.ProjectProperties.OutputType
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		VSLangProj.prjProjectType VSLangProj.ProjectProperties.ProjectType
		{
			get { throw new NotImplementedException(); }
		}

		string VSLangProj.ProjectProperties.ReferencePath
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.RootNamespace
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.ServerExtensionsVersion
		{
			get { throw new NotImplementedException(); }
		}

		string VSLangProj.ProjectProperties.StartupObject
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.URL
		{
			get { throw new NotImplementedException(); }
		}

		VSLangProj.prjWebAccessMethod VSLangProj.ProjectProperties.WebAccessMethod
		{
			get
			{
				throw new NotImplementedException();
			}
			set
			{
				throw new NotImplementedException();
			}
		}

		string VSLangProj.ProjectProperties.WebServer
		{
			get { throw new NotImplementedException(); }
		}

		string VSLangProj.ProjectProperties.WebServerVersion
		{
			get { throw new NotImplementedException(); }
		}

		string VSLangProj.ProjectProperties.__id
		{
			get { throw new NotImplementedException(); }
		}

		object VSLangProj.ProjectProperties.__project
		{
			get { throw new NotImplementedException(); }
		}

		object VSLangProj.ProjectProperties.get_Extender(string ExtenderName)
		{
			throw new NotImplementedException();
		}

		#endregion
	}
}
