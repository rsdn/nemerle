using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using Microsoft.Build.BuildEngine;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project
{
    /// <summary>
    /// Contains helper methods for including/excluding items in a <see cref="NemerleProjectNode"/>.
    /// </summary>
    internal static class NemerleProjectMembers
    {
        /// <summary>
        /// Adds non member items to the hierarchy.
        /// </summary>
        /// <param name="project">The project to modify.</param>
        [SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
        internal static void AddNonMemberItems(NemerleProjectNode project)
        {
            IList<string> files = new List<string>();
            IList<string> folders = new List<string>();

            // obtain the list of files and folders under the project folder.
            GetRelativeFileSystemEntries(project.ProjectFolder, null, files, folders);

            // exclude the items which are the part of the build.
            ExcludeProjectBuildItems(project, files, folders);

            AddNonMemberFolderItems(project, folders);
            AddNonMemberFileItems(project, files);
        }

        /// <summary>
        /// Removes non member item nodes from hierarchy.
        /// </summary>
        /// <param name="project">The project to modify.</param>
        [SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
        internal static void RemoveNonMemberItems(HierarchyNode project)
        {
            IList<HierarchyNode> nodeList = new List<HierarchyNode>();
            HierarchyHelpers.FindNodes(nodeList, project, IsNodeNonMemberItem, null);
            for (int index = nodeList.Count - 1; index >= 0; index--)
            {
                HierarchyNode parent = nodeList[index].Parent;
                nodeList[index].OnItemDeleted();
                parent.RemoveChild(nodeList[index]);
            }
        }

        /// <summary>
        /// This is the filter for non member items.
        /// </summary>
        /// <param name="node">Node to be filtered.</param>
        /// <param name="criteria">Filter criteria.</param>
        /// <returns>Returns if the node is a non member item node or not.</returns>
        [SuppressMessage("Microsoft.Usage", "CA1806:DoNotIgnoreMethodResults", MessageId = "System.Boolean.TryParse(System.String,System.Boolean@)")]
        private static bool IsNodeNonMemberItem(HierarchyNode node, object criteria)
        {
            bool isNonMemberItem = false;
            if (node != null)
            {
                object propObj = node.GetProperty((int)__VSHPROPID.VSHPROPID_IsNonMemberItem);
                if (propObj != null)
                    bool.TryParse(propObj.ToString(), out isNonMemberItem);
            }

            return isNonMemberItem;
        }

        /// <summary>
        /// Gets the file system entries of a folder and its all sub-folders with relative path.
        /// </summary>
        /// <param name="baseFolder">Base folder.</param>
        /// <param name="filter">Filter to be used. default is "*"</param>
        /// <param name="fileList">Files list containing the relative file paths.</param>
        /// <param name="folderList">Folders list containing the relative folder paths.</param>
        private static void GetRelativeFileSystemEntries(string baseFolder, string filter, ICollection<string> fileList, ICollection<string> folderList)
        {
            ErrorHelper.ThrowIsNull(baseFolder, "baseFolder");
            
            if (string.IsNullOrEmpty(filter))
                filter = "*";  // include all files and folders
            
            string[] fileEntries = Directory.GetFiles(baseFolder, filter, SearchOption.AllDirectories);
            foreach (string file in fileEntries)
            {
                FileInfo fileInfo = new FileInfo(file);
                if ((fileInfo.Attributes & FileAttributes.Hidden) == FileAttributes.Hidden)
                    continue;
                
                string fileRelativePath = Utils.GetRelativePath(baseFolder, file);
                if (!string.IsNullOrEmpty(fileRelativePath))
                    fileList.Add(fileRelativePath);
            }
        
            string[] folderEntries = Directory.GetDirectories(baseFolder, filter, SearchOption.AllDirectories);
            foreach (string folder in folderEntries)
            {
                DirectoryInfo folderInfo = new DirectoryInfo(folder);
                if ((folderInfo.Attributes & FileAttributes.Hidden) == FileAttributes.Hidden)
                    continue;

                string folderRelativePath = Utils.GetRelativePath(baseFolder, folder);
                if (!string.IsNullOrEmpty(folderRelativePath))
                    folderList.Add(folderRelativePath);
            }
        }

        /// <summary>
        /// Excludes the file and folder items from their corresponding maps if they are part of the build.
        /// </summary>
        /// <param name="project">The project to modify.</param>
        /// <param name="fileList">List containing relative files paths.</param>
        /// <param name="folderList">List containing relative folder paths.</param>
        private static void ExcludeProjectBuildItems(ProjectNode project, IList<string> fileList, IList<string> folderList)
        {
            BuildItemGroup projectItems = project.BuildProject.EvaluatedItems;

            if (projectItems == null)
                return; // do nothig, just ignore it.
                        
            if (fileList == null && folderList == null)
                throw new ArgumentNullException("folderList");
            
            // we need these maps becuase we need to have both lowercase and actual case path information.
            // we use lower case paths for case-insesitive search of file entries and actual paths for 
            // creating hierarchy node. if we don't do that, we will end up with duplicate nodes when the
            // case of path in .wixproj file doesn't match with the actual file path on the disk.
            IDictionary<string, string> folderMap = null;
            IDictionary<string, string> fileMap = null;

            if (folderList != null)
            {
                folderMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
                foreach (string folder in folderList)
                    folderMap.Add(folder, folder);
            }

            if (fileList != null)
            {
                fileMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
                foreach (string file in fileList)
                    fileMap.Add(file, file);
            }

            foreach (BuildItem buildItem in projectItems)
            {
                if (folderMap != null &&
                    folderMap.Count > 0 &&
                    String.Equals(buildItem.Name, ProjectFileConstants.Folder, StringComparison.OrdinalIgnoreCase))
                {
                    string relativePath = buildItem.FinalItemSpec;
                    if (Path.IsPathRooted(relativePath)) // if not the relative path, make it relative
                        relativePath = Utils.GetRelativePath(project.ProjectFolder, relativePath);
                    
                    if (folderMap.ContainsKey(relativePath))
                    {
                        folderList.Remove(folderMap[relativePath]); // remove it from the actual list.
                        folderMap.Remove(relativePath);
                    }
                }
                else if (fileMap != null &&
                    fileMap.Count > 0 &&
                    IsNemerleFileItem(buildItem))
                {
                    string relativePath = buildItem.FinalItemSpec;
                    if (Path.IsPathRooted(relativePath)) // if not the relative path, make it relative
                    {
                        relativePath = Utils.GetRelativePath(project.ProjectFolder, relativePath);
                    }

                    if (fileMap.ContainsKey(relativePath))
                    {
                        fileList.Remove(fileMap[relativePath]); // remove it from the actual list.
                        fileMap.Remove(relativePath);
                    }
                }
            }
        }

        /// <summary>
        /// Adds non member folder items to the hierarcy.
        /// </summary>
        /// <param name="project">The project to modify.</param>
        /// <param name="folderList">Folders list containing the folder names.</param>
        [SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
        private static void AddNonMemberFolderItems(ProjectNode project, IEnumerable<string> folderList)
        {
            ErrorHelper.ThrowIsNull(folderList, "folderList");
            
            foreach (string folderKey in folderList)
            {
                HierarchyNode parentNode = project;
                string[] folders = folderKey.Split(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
                NemerleFolderNode topFolderNode = null;
                foreach (string folder in folders)
                {
                    string childNodeId = Path.Combine(parentNode.VirtualNodeName, folder);
                    FileInfo folderInfo = new FileInfo(Path.Combine(project.ProjectFolder, childNodeId));
                    if ((folderInfo.Attributes & FileAttributes.Hidden) == FileAttributes.Hidden)
                        break;
                    
                    HierarchyNode childNode = parentNode.FindChild(childNodeId);
                    if (childNode == null)
                    {
                        if (topFolderNode == null)
                        {
                            topFolderNode = parentNode as NemerleFolderNode;
                            if (topFolderNode != null && (!topFolderNode.IsNonMemberItem) && topFolderNode.IsExpanded)
                                topFolderNode = null;
                        }

                        ProjectElement element = new ProjectElement(project, null, true);
                        childNode = project.CreateFolderNode(childNodeId, element);
                        parentNode.AddChild(childNode);
                    }

                    parentNode = childNode;
                }

                if (topFolderNode != null)
                    topFolderNode.CollapseFolder();
            }
        }

        /// <summary>
        /// Adds non member file items to the hierarcy.
        /// </summary>
        /// <param name="project">The project to modify.</param>
        /// <param name="fileList">Files containing the information about the non member file items.</param>
        [SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
        private static void AddNonMemberFileItems(ProjectNode project, IEnumerable<string> fileList)
        {
            ErrorHelper.ThrowIsNull(fileList, "fileList");
            
            foreach (string fileKey in fileList)
            {
                HierarchyNode parentNode = project;
                string[] pathItems = fileKey.Split(new[] { Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar });

                if (StringComparer.OrdinalIgnoreCase.Equals(fileKey, project.ProjectFile))
                    continue;
                
                NemerleFolderNode topFolderNode = null;
                foreach (string fileOrDir in pathItems)
                {
                    string childNodeId = Path.Combine(parentNode.VirtualNodeName, fileOrDir);
                    FileInfo fileOrDirInfo = new FileInfo(Path.Combine(project.ProjectFolder, childNodeId));
                    if ((fileOrDirInfo.Attributes & FileAttributes.Hidden) == FileAttributes.Hidden)
                        break;
                    
                    HierarchyNode childNode = parentNode.FindChild(childNodeId);
                    if (childNode == null)
                    {
                        if (topFolderNode == null)
                        {
                            topFolderNode = parentNode as NemerleFolderNode;
                            if (topFolderNode != null && (!topFolderNode.IsNonMemberItem) && topFolderNode.IsExpanded)
                                topFolderNode = null;
                        }

                        ProjectElement element = new ProjectElement(project, null, true);
                        element.Rename(childNodeId);
                        element.SetMetadata(ProjectFileConstants.Name, childNodeId);
                        childNode = project.CreateFileNode(element);
                        parentNode.AddChild(childNode);
                        break;
                    }

                    parentNode = childNode;
                }

                if (topFolderNode != null)
                    topFolderNode.CollapseFolder();
            }
        }

        /// <summary>
        /// Returns if the buildItem is a file item or not.
        /// </summary>
        /// <param name="buildItem">BuildItem to be checked.</param>
        /// <returns>Returns true if the buildItem is a file item, false otherwise.</returns>
        private static bool IsNemerleFileItem(BuildItem buildItem)
        {
            ErrorHelper.ThrowIsNull(buildItem, "buildItem");
            
            bool isNemerleFileItem = false;
            if (StringComparer.OrdinalIgnoreCase.Equals(buildItem.Name, ProjectFileConstants.Compile))
                isNemerleFileItem = true;
            else if (StringComparer.OrdinalIgnoreCase.Equals(buildItem.Name, ProjectFileConstants.EmbeddedResource))
                isNemerleFileItem = true;
            else if (StringComparer.OrdinalIgnoreCase.Equals(buildItem.Name, ProjectFileConstants.Content))
                isNemerleFileItem = true;
            else if (StringComparer.OrdinalIgnoreCase.Equals(buildItem.Name, "None"))
                isNemerleFileItem = true;
            
            return isNemerleFileItem;
        }
    }
}
