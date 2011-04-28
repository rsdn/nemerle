using System.IO;
using System.Reflection;

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using System;
using System.Diagnostics;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System.Security;

namespace Nemerle.VisualStudio.Project
{
	class NemerleMacroReferenceContainerNode : NemerleReferenceContainerNode
	{
		internal const string MacroReferencesNodeVirtualName = NemerleConstants.MacroReference;

		#region ctor

		public NemerleMacroReferenceContainerNode(ProjectNode root)
			: base(root)
		{
			this.VirtualNodeName = MacroReferencesNodeVirtualName;
		}

		#endregion

    public override object GetIconHandle(bool open)
    {
      var img = NemerleProjectNode.NemerleImageList.Images[NemerleConstants.ImageListIndex.NemerleMacroReferences];
      return PackageUtilities.GetIntPointerFromImage(img);
    }

    public override string Caption
    {
      get { return "Macro References"; }
    }

    private static string[] _supportedReferenceTypes = new[] { NemerleConstants.MacroReference, NemerleConstants.MacroProjectReference };

    protected override string[] SupportedReferenceTypes
    {
      get { return _supportedReferenceTypes; }
    }

    protected override ReferenceNode CreateReferenceNode(string referenceType, ProjectElement element)
    {
      switch (referenceType)
      {
        case NemerleConstants.MacroReference:        return this.CreateAssemblyReferenceNode(element);
        case NemerleConstants.MacroProjectReference: return this.CreateProjectReferenceNode(element);
        default:                                     return base.CreateReferenceNode(referenceType, element);
      }
    }

    /// <summary>
    /// Creates a project reference node given an existing project element.
    /// </summary>
    protected override ProjectReferenceNode CreateProjectReferenceNode(ProjectElement element)
    {
      return new NemerleMacroProjectReferenceNode(this.ProjectMgr, element);
    }
    /// <summary>
    /// Create a Project to Project reference given a VSCOMPONENTSELECTORDATA structure
    /// </summary>
    protected override ProjectReferenceNode CreateProjectReferenceNode(VSCOMPONENTSELECTORDATA selectorData)
    {
      return new NemerleMacroProjectReferenceNode(this.ProjectMgr, selectorData.bstrTitle, selectorData.bstrFile, selectorData.bstrProjRef);
    }

    protected override AssemblyReferenceNode CreateAssemblyReferenceNode(string fileName)
    {
      return new NemerleMacroAssemblyReferenceNode(ProjectMgr, fileName);
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
        node = File.Exists(item) ? new NemerleMacroAssemblyReferenceNode(ProjectMgr, item)
                                 : new NemerleMacroAssemblyReferenceNode(ProjectMgr, element);
      }
      catch (ArgumentNullException   e) { Trace.WriteLine("Exception : " + e.Message); }
      catch (FileNotFoundException   e) { Trace.WriteLine("Exception : " + e.Message); }
      catch (BadImageFormatException e) { Trace.WriteLine("Exception : " + e.Message); }
      catch (FileLoadException       e) { Trace.WriteLine("Exception : " + e.Message); }
      catch (SecurityException       e) { Trace.WriteLine("Exception : " + e.Message); }

      return node;
    }

    public override void AddChild(HierarchyNode node)
    {
      if (node == null)
        throw new ArgumentNullException("node");

      // make sure the node is in the map.
      Object nodeWithSameID = this.ProjectMgr.ItemIdMap[node.ID];

      if (!Object.ReferenceEquals(node, nodeWithSameID as HierarchyNode))
      {
        if (nodeWithSameID == null && node.ID <= this.ProjectMgr.ItemIdMap.Count)
          // reuse our hierarchy id if possible.
          this.ProjectMgr.ItemIdMap.SetAt(node.ID, this);
        else
          throw new InvalidOperationException();
      }

      HierarchyNode previous = null;
      
      for (HierarchyNode n = this.FirstChild; n != null; n = n.NextSibling)
      {
        if (this.ProjectMgr.CompareNodes(node, n) > 0)
          break;

        previous = n;
      }
      
      // insert "node" after "previous".
      if (previous != null)
      {
        node.NextSibling = previous.NextSibling;
        previous.NextSibling = node;

        if (previous == this.LastChild)
          this.LastChild = node;
      }
      else
      {
        if (this.LastChild == null)
          this.LastChild = node;

        node.NextSibling = this.FirstChild;
        this.FirstChild = node;
      }

      node.Parent = this;
      this.OnItemAdded(this, node);

      NemerleProjectNode project = ProjectMgr as NemerleProjectNode;

      if (project != null)
      {
        ReferenceNode referenceNode = (ReferenceNode)node;
        //TODO: Добавить в список макро-сборок
        project.ProjectInfo.AddMacroAssembly(referenceNode);
      }
    }

    public override void RemoveChild(HierarchyNode node)
    {
      NemerleProjectNode project = ProjectMgr as NemerleProjectNode;

      if (project != null)
      {
        ReferenceNode referenceNode = (ReferenceNode)node;
        //TODO: Удалить из списка макро-сборок
        project.ProjectInfo.RemoveMacroAssembly(referenceNode);
      }

      if (node == null)
        throw new ArgumentNullException("node");

      ProjectMgr.ItemIdMap.Remove(node);

      HierarchyNode last = null;

      for (HierarchyNode n = this.FirstChild; n != null; n = n.NextSibling)
      {
        if (n == node)
        {
          if (last != null)
            last.NextSibling = n.NextSibling;

          if (n == this.LastChild)
          {
            if (last == this.LastChild)
              this.LastChild = null;
            else
              this.LastChild = last;
          }

          if (n == this.FirstChild)
            this.FirstChild = n.NextSibling;

          return;
        }

        last = n;
      }

      throw new InvalidOperationException("Node not found");
    }
  }
}
