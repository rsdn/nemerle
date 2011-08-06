using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;

namespace Nemerle.VisualStudio.Project
{
	class ProjectUpgradeHelper
	{
		XNamespace vs = XNamespace.Get("http://schemas.microsoft.com/developer/msbuild/2003");

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
				project.Add(ToolsVersion = new XAttribute("ToolsVersion", "0.0"));
		}

		private XElement FindPropertyElement(XElement project, string propertyName)
		{
			foreach (var nemerleProperty in project.Descendants(vs + propertyName))
				return nemerleProperty;

			// Try to add property if it's not exists
			foreach (var propertyGroup in project.Elements(vs + "PropertyGroup").Where(g => !g.HasAttributes))
			{
				var newElem = new XElement(vs + propertyName, "");
				propertyGroup.Add(newElem);
				return newElem;
			}

			throw new ApplicationException("Incorrect format of project file. The project must contains '" + propertyName + "' property.");
		}

		public XAttribute ToolsVersion           { get; private set; }
		public XElement   NemerleProperty        { get; private set; }
		public XElement   TargetFrameworkVersion { get; private set; }
	}
}
