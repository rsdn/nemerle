using System.Diagnostics;
using EnvDTE;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;

namespace Nemerle.VisualStudio.Project
{
    public class NemerleOAFolderItem : OAFolderItem
    {
        public NemerleOAFolderItem(OAProject project, FolderNode node) : base(project, node)
        {}

        public override ProjectItems Collection
        {
            get
            {
                Debug.Assert(Node.Parent != null, string.Format("Failed to get the parent node of '{0}'", Node.Caption));
                return new OAProjectItems(Project, Node.Parent);
            }
        }

        public override ProjectItems ProjectItems
        {
            get
            {
                return new OAProjectItems(Project, Node);
            }
        }
    }
}