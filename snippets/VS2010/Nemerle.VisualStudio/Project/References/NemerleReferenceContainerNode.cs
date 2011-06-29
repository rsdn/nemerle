using System.IO;
using System.Reflection;

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using System;
using System.Diagnostics;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.Project
{
	class NemerleReferenceContainerNode : ReferenceContainerNode
	{
		#region ctor

		public NemerleReferenceContainerNode(ProjectNode root)
			: base(root)
		{
		}

		#endregion

		/// <summary>
		/// Returns an instance of the automation object for ReferenceContainerNode
		/// </summary>
		/// <returns>An intance of the Automation.OAReferenceFolderItem type if succeeeded</returns>
		public override object GetAutomationObject()
		{
			if (this.ProjectMgr == null || this.ProjectMgr.IsClosed)
				return null;

			return new NemerleOAReferenceFolderItem((OAProject)this.ProjectMgr.GetAutomationObject(), this);
		}

		protected override ReferenceNode CreateFileComponent(VSCOMPONENTSELECTORDATA selectorData, string wrapperTool = null)
		{
			try
			{
				var node = base.CreateFileComponent(selectorData, wrapperTool);
				return node;
			}
			catch (NullReferenceException e)
			{
				if (e.Message == "The InstalledFilePath is null")
					throw new ApplicationException("The file '"
						+ System.IO.Path.GetFileName(selectorData.bstrFile)
						+ "' not .Net assembly or correctly installed COM library!");

				throw;
			}
		}

		/// <summary>
		/// Creates a project reference node given an existing project element.
		/// </summary>
		protected override ProjectReferenceNode CreateProjectReferenceNode(ProjectElement element)
		{
			return new NemerleProjectReferenceNode(this.ProjectMgr, element);
		}

		/// <summary>
		/// Create a Project to Project reference given a VSCOMPONENTSELECTORDATA structure
		/// </summary>
		protected override ProjectReferenceNode CreateProjectReferenceNode(VSCOMPONENTSELECTORDATA selectorData)
		{
			return new NemerleProjectReferenceNode(this.ProjectMgr, selectorData.bstrTitle, selectorData.bstrFile, selectorData.bstrProjRef);
		}

		protected override AssemblyReferenceNode CreateAssemblyReferenceNode(string fileName)
		{
			NemerleAssemblyReferenceNode node = null;

			try
			{
				node = new NemerleAssemblyReferenceNode(ProjectMgr, fileName);
			}
			catch (ArgumentNullException e)
			{
				Trace.WriteLine("Exception : " + e.Message);
			}
			catch (FileNotFoundException e)
			{
				Trace.WriteLine("Exception : " + e.Message);
			}
			catch (BadImageFormatException e)
			{
				Trace.WriteLine("Exception : " + e.Message);
			}
			catch (FileLoadException e)
			{
				Trace.WriteLine("Exception : " + e.Message);
			}
			catch (System.Security.SecurityException e)
			{
				Trace.WriteLine("Exception : " + e.Message);
			}

			return node;
		}

		protected override AssemblyReferenceNode CreateAssemblyReferenceNode(ProjectElement element)
		{
			// VladD2:
			// ReferenceContainerNode does not support reference to files (instead of
			// get assembly name from file it tries to use file name as assembly name
			// (via System.Reflection.AssemblyName()).

			string item = element.Item.EvaluatedInclude; //FinalItemSpec
			NemerleAssemblyReferenceNode node = null;

			try
			{
				if (File.Exists(item))
					node = new NemerleAssemblyReferenceNode(ProjectMgr, item);
				else
					node = new NemerleAssemblyReferenceNode(ProjectMgr, element);
			}
			catch (ArgumentNullException e)
			{
				Trace.WriteLine("Exception : " + e.Message);
			}
			catch (FileNotFoundException e)
			{
				Trace.WriteLine("Exception : " + e.Message);
			}
			catch (BadImageFormatException e)
			{
				Trace.WriteLine("Exception : " + e.Message);
			}
			catch (FileLoadException e)
			{
				Trace.WriteLine("Exception : " + e.Message);
			}
			catch (System.Security.SecurityException e)
			{
				Trace.WriteLine("Exception : " + e.Message);
			}

			return node;
		}

		public override void AddChild(HierarchyNode node)
		{
			base.AddChild(node);

			NemerleProjectNode project = ProjectMgr as NemerleProjectNode;

			if (project != null && project.ProjectInfo.IsLoaded)
			{
				ReferenceNode referenceNode = (ReferenceNode)node;
				project.ProjectInfo.AddAssembly(referenceNode);
			}
		}

		public override void RemoveChild(HierarchyNode node)
		{
			base.RemoveChild(node);

			NemerleProjectNode project = ProjectMgr as NemerleProjectNode;

			if (project != null)
			{
				ReferenceNode referenceNode = (ReferenceNode)node;
				project.ProjectInfo.RemoveAssembly(referenceNode);
			}
		}
	}
}
