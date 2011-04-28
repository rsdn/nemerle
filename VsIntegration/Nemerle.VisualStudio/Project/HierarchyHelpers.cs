using System;
using System.Collections.Generic;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;
using OleConstants = Microsoft.VisualStudio.OLE.Interop.Constants;

namespace Nemerle.VisualStudio.Project
{
	static class HierarchyHelpers
	{
		/// <summary>
		/// This is the node filter delegate.
		/// </summary>
		/// <param name="node">Node to be tested.</param>
		/// <param name="criteria">Filter criteria.</param>
		/// <returns>Returns if the node should be filtered or not.</returns>
		public delegate bool NodeFilter(HierarchyNode node, object criteria);

		/// <summary>
		/// Handles command status on source a node. Should be overridden by descendant nodes.
		/// </summary>
		/// <param name="node">A HierarchyNode that implements the IProjectSourceNode interface.</param>
		/// <param name="guidCmdGroup">A unique identifier of the command group. The pguidCmdGroup parameter can be NULL to specify the standard group.</param>
		/// <param name="cmd">The command to query status for.</param>
		/// <param name="result">An out parameter specifying the QueryStatusResult of the command.</param>
		/// <param name="returnCode">If the method succeeds, it returns S_OK. If it fails, it returns an error code.</param>
		/// <returns>Returns true if the status request is handled, false otherwise.</returns>
		internal static bool QueryStatusOnProjectSourceNode(HierarchyNode node, Guid guidCmdGroup, uint cmd, ref QueryStatusResult result, out int returnCode)
		{
			if (guidCmdGroup == VsMenus.guidStandardCommandSet2K)
			{
				IProjectSourceNode sourceNode = node as IProjectSourceNode;
				switch ((VsCommands2K)cmd)
				{
					case VsCommands2K.SHOWALLFILES:
						{
							NemerleProjectNode projectNode = node.ProjectMgr as NemerleProjectNode;
							result |= QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
							if (projectNode != null && projectNode.ShowAllFilesEnabled)
								result |= QueryStatusResult.LATCHED; // it should be displayed as pressed

							returnCode = VSConstants.S_OK;
							return true; // handled.
						}

					case VsCommands2K.INCLUDEINPROJECT:
						// if it is a non member item node, the we support "Include In Project" command
						if (sourceNode != null && sourceNode.IsNonMemberItem)
						{
							result |= QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
							returnCode = VSConstants.S_OK;
							return true; // handled.
						}

						break;

					case VsCommands2K.EXCLUDEFROMPROJECT:
						// if it is a non member item node, then we don't support "Exclude From Project" command
						if (sourceNode != null && sourceNode.IsNonMemberItem)
						{
							returnCode = (int)OleConstants.MSOCMDERR_E_NOTSUPPORTED;
							return true; // handled.
						}

						break;
				}
			}

			// just an arbitrary value, it doesn't matter if method hasn't handled the request
			returnCode = VSConstants.S_FALSE;

			// not handled
			return false;
		}

		/// <summary>
		/// Walks up in the hierarchy and ensures that all parent folder nodes of 'node' are included in the project.
		/// </summary>
		/// <param name="node">Start hierarchy node.</param>
		internal static void EnsureParentFolderIncluded(HierarchyNode node)
		{
			ErrorHelper.ThrowIsNull(node, "node");

			// use stack to make sure all parent folders are included in the project.
			Collections.Stack<NemerleFolderNode> stack = new Collections.Stack<NemerleFolderNode>();

			// Find out the parent folder nodes if any.
			NemerleFolderNode parentFolderNode = node.Parent as NemerleFolderNode;
			while (parentFolderNode != null && parentFolderNode.IsNonMemberItem)
			{
				stack.Push(parentFolderNode);
				parentFolderNode.CreateDirectory(); // ensure that the folder is there on file system
				parentFolderNode = parentFolderNode.Parent as NemerleFolderNode;
			}

			// include all parent folders in the project.
			while (stack.Count > 0)
			{
				NemerleFolderNode folderNode = stack.Pop();
				((IProjectSourceNode)folderNode).IncludeInProject(false);
			}
		}

		/// <summary>
		/// Finds child nodes uner the parent node and places them in the currentList.
		/// </summary>
		/// <param name="currentList">List to be populated with the nodes.</param>
		/// <param name="parent">Parent node under which the nodes should be searched.</param>
		/// <param name="filter">Filter to be used while selecting the node.</param>
		/// <param name="criteria">Criteria to be used by the filter.</param>
		internal static void FindNodes(
				IList<HierarchyNode> currentList, HierarchyNode parent, NodeFilter filter, object criteria)
		{
			ErrorHelper.ThrowIsNull(currentList, "currentList");
			ErrorHelper.ThrowIsNull(parent, "parent");
			ErrorHelper.ThrowIsNull(filter, "filter");

			for (HierarchyNode child = parent.FirstChild; child != null; child = child.NextSibling)
			{
				if (filter(child, criteria))
					currentList.Add(child);

				FindNodes(currentList, child, filter, criteria);
			}
		}

		internal static void RefreshPropertyBrowser(HierarchyNode node)
		{
			IVsUIShell shell = (IVsUIShell)node.ProjectMgr.GetService(typeof(IVsUIShell));
			ErrorHandler.ThrowOnFailure(shell.RefreshPropertyBrowser(0));
		}
	}
}
