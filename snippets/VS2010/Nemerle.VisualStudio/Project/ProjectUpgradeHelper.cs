using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;

namespace Nemerle.VisualStudio.Project
{
	class ProjectUpgradeHelper
	{
		public ProjectUpgradeHelper(string projectFilePath)
		{
			var doc = XDocument.Load(projectFilePath, LoadOptions.PreserveWhitespace | LoadOptions.SetBaseUri | LoadOptions.SetLineInfo);
			var project = doc.Root;

			if (project == null || project.Name.LocalName != "Project")
				throw new ApplicationException("The '" + projectFilePath + "' is not correct project file.");

			ToolsVersion           = project.Attribute("ToolsVersion");
			NemerleProperty        = FindPropertyElement(project, "Nemerle");
			TargetFrameworkVersion = FindPropertyElement(project, "TargetFrameworkVersion");

			if (ToolsVersion == null)
				project.Add(new XAttribute("ToolsVersion", ""));
		}

		private XElement FindPropertyElement(XElement project, string propertyNamre)
		{
			foreach (var nemerleProperty in project.Descendants())
				if (Utils.Eq(nemerleProperty.Name.LocalName, propertyNamre))
					return nemerleProperty;

			throw new ApplicationException("Incorrect format of project file. The project must contains '" 
				+ propertyNamre + "' property.");
		}

		public XAttribute ToolsVersion           { get; private set; }
		public XElement   NemerleProperty        { get; private set; }
		public XElement   TargetFrameworkVersion { get; private set; }
	}
}
