/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject
{
	/// <summary>
	/// This class extends the ContainerNode in order to represent our project 
	/// within the hierarchy.
	/// </summary>
	[GuidAttribute(GuidStrings.GuidNestedProjectNode)]
	public class NestedProjectNode : ProjectContainerNode
	{
		#region Constructors
		/// <summary>
		/// Explicitly defined default constructor.
		/// </summary>
		public NestedProjectNode()
		{
			this.SupportsProjectDesigner = true;
            this.CanProjectDeleteItems = true;

			// Add Category IDs mapping in order to support properties for project items
			AddCATIDMapping(typeof(FileNodeProperties), typeof(FileNodeProperties).GUID);
			AddCATIDMapping(typeof(ProjectNodeProperties), typeof(ProjectNodeProperties).GUID);
			AddCATIDMapping(typeof(FolderNodeProperties), typeof(FolderNodeProperties).GUID);
			AddCATIDMapping(typeof(ReferenceNodeProperties), typeof(ReferenceNodeProperties).GUID);
		}
		#endregion Constructors

		#region Properties
		/// <summary>
		/// Gets project's Guid value.
		/// </summary>
		public override Guid ProjectGuid
		{
			get
			{
				return typeof(NestedProjectFactory).GUID;
			}
		}
		/// <summary>
		/// Gets project's type as string value.
		/// </summary>
		public override string ProjectType
		{
			get
			{
				return this.GetType().Name;
			}
		}
		#endregion Properties

		#region Methods

		/// <summary>
		/// Generate new Guid value and update it with GeneralPropertyPage GUID.
		/// </summary>
		/// <returns>Returns the property pages that are independent of configuration.</returns>
		protected override Guid[] GetConfigurationIndependentPropertyPages()
		{
			Guid[] result = new Guid[1];
			result[0] = typeof(GeneralPropertyPage).GUID;
			return result;
		}

		/// <summary>
		/// Overriding to provide project general property page.
		/// </summary>
		/// <returns>Returns the GeneralPropertyPage GUID value.</returns>
		protected override Guid[] GetPriorityProjectDesignerPages()
		{
			Guid[] result = new Guid[1];
			result[0] = typeof(GeneralPropertyPage).GUID;
			return result;
		}

		/// <summary>
		/// Specify here a property page. 
		/// By returning no property page the configuration dependent properties will be neglected.
		/// </summary>
		/// <returns>Returns the configuration dependent property pages.</returns>
		protected override Guid[] GetConfigurationDependentPropertyPages()
		{
			Guid[] result = new Guid[1];
			result[0] = typeof(NestedProjectBuildPropertyPage).GUID;
			return result;
		}

		/// <summary>
		/// Overriding to provide customization of files on add files.
		/// This will replace tokens in the file with actual value (namespace, class name,...)
		/// </summary>
		/// <param name="source">Full path to template file.</param>
		/// <param name="target">Full path to destination file.</param>
		/// <exception cref="FileNotFoundException">Template file is not founded.</exception>
		public override void AddFileFromTemplate(string source, string target)
		{
			if(!File.Exists(source))
			{
				throw new FileNotFoundException(string.Format("Template file not found: {0}", source));
			}

			// The class name is based on the new file name
			string fileName = Path.GetFileNameWithoutExtension(target);
			string nameSpace = this.FileTemplateProcessor.GetFileNamespace(target, this);

			this.FileTemplateProcessor.Reset();
			this.FileTemplateProcessor.AddReplace("%className%", fileName);
			this.FileTemplateProcessor.AddReplace("%namespace%", nameSpace);

			try
			{
				this.FileTemplateProcessor.UntokenFile(source, target);

			}
			catch(Exception exceptionObj)
			{
				throw new FileLoadException(Resources.ResourceManager.GetString("MsgFailedToLoadTemplateFile"), target, exceptionObj);
			}
		}

		/// <summary>
		/// Creates the format list for the open file dialog.
		/// </summary>
		/// <param name="ppszFormatList">The format list to return.</param>
		/// <returns>S_OK if method is succeeded.</returns>
		public override int GetFormatList(out string ppszFormatList)
		{
			ppszFormatList = String.Format(CultureInfo.CurrentCulture, Resources.GetString(Resources.NestedProjectFileAssemblyFilter), "\0", "\0");
			return VSConstants.S_OK;
		}

		/// <summary>
		/// Adds support for project properties.
		/// </summary>
		/// <returns>Return the automation object associated to this project.</returns>
		public override object GetAutomationObject()
		{
			return new OANestedProject(this);
		}

		#endregion Methods
	}
}
