﻿using System;
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

			NemerleBinPathRoot     = FindPropertyElement(project, "NemerleBinPathRoot");
			NemerleVersion         = FindPropertyElement(project, "NemerleVersion");
			NemerleProperty        = FindPropertyElement(project, "Nemerle");
		}

		private XElement FindPropertyElement(XElement project, string propertyName)
		{
            var xmlns = project.Attribute("xmlns");
            var vs = xmlns == null ? XNamespace.Get("") : XNamespace.Get(xmlns.Value);

            foreach (var nemerleProperty in project.Descendants(vs + propertyName))
				return nemerleProperty;

			// Try to add property if it's not exists
			foreach (var propertyGroup in project.Elements(vs + "PropertyGroup").Where(g => !g.HasAttributes))
			{
				var text = propertyGroup.HasElements ? propertyGroup.FirstNode as XText : null;
				var newElem = new XElement(vs + propertyName, "");
				if (propertyName == "Nemerle")
					propertyGroup.Add(newElem);
				else
					propertyGroup.AddFirst(newElem);

				if (text != null)
					newElem.AddBeforeSelf(text.Value);
				else
					newElem.AddBeforeSelf("  " + Environment.NewLine);

				return newElem;
			}

			throw new ApplicationException("Incorrect format of project file. The project must contains '" + propertyName + "' property.");
		}

		public XElement   NemerleProperty        { get; private set; }
		public XElement   NemerleVersion         { get; private set; }
		public XElement   NemerleBinPathRoot     { get; private set; }
	}
}
