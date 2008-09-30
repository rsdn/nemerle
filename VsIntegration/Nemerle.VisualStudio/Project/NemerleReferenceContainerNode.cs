using System.IO;
using System.Reflection;

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using System;
using System.Diagnostics;
using Microsoft.VisualStudio.Shell;

namespace Nemerle.VisualStudio.Project
{
	class NemerleReferenceContainerNode : ReferenceContainerNode
	{
		#region ctor

		public NemerleReferenceContainerNode(ProjectNode root) : base(root)
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

		/// <summary>
		/// Creates a project reference node given an existing project element.
		/// </summary>
		protected override ProjectReferenceNode CreateProjectReferenceNode(ProjectElement element)
		{
			return new NemerleProjectReferenceNode(this.ProjectMgr, element);
		}

		protected override AssemblyReferenceNode CreateAssemblyReferenceNode(ProjectElement element)
		{
			// VladD2:
			// ReferenceContainerNode does not support reference to files (instead of
			// get assembly name from file it tries to use file name as assembly name
			// (via System.Reflection.AssemblyName()).

			string item = element.Item.FinalItemSpec;
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
			EventSinkCollection map = this.ProjectMgr.ItemIdMap;

			// make sure the node is in the map.
			Object nodeWithSameID = this.ProjectMgr.ItemIdMap[node.ID];

			base.AddChild(node);

			NemerleProjectNode project = ProjectMgr as NemerleProjectNode;

			if (project != null)
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
