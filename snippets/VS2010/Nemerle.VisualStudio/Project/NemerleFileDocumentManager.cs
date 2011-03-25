using System;
using System.Collections.Generic;
using System.Text;

using Microsoft.VisualStudio.Project;

using Nemerle.VisualStudio.Project;

namespace Nemerle.VisualStudio.Package
{
	class NemerleFileDocumentManager : FileDocumentManager
	{
		public NemerleFileDocumentManager(FileNode node) : base(node)
		{
		}

		public override int Open(bool newFile, bool openWith, ref Guid logicalView, IntPtr docDataExisting, out Microsoft.VisualStudio.Shell.Interop.IVsWindowFrame windowFrame, WindowFrameShowAction windowFrameAction)
		{
			//TODO: It's not best way to find project. NemerleFileDocumentManager must contain reference to ProjectInfo.
			ProjectInfo projectInfo = ProjectInfo.FindProject(GetFullPathForDocument());
			
			if (projectInfo != null)
				projectInfo.IsDocumentOpening = true;

			try
			{
				return base.Open(newFile, openWith, ref logicalView, docDataExisting, out windowFrame, windowFrameAction);
			}
			finally
			{
				if (projectInfo != null)
					projectInfo.IsDocumentOpening = false;
			}
		}

		public override int Open(ref Guid logicalView, IntPtr docDataExisting, out Microsoft.VisualStudio.Shell.Interop.IVsWindowFrame windowFrame, WindowFrameShowAction windowFrameAction)
		{
			return base.Open(ref logicalView, docDataExisting, out windowFrame, windowFrameAction);
		}

		public override int OpenWithSpecific(uint editorFlags, ref Guid editorType, string physicalView, ref Guid logicalView, IntPtr docDataExisting, out Microsoft.VisualStudio.Shell.Interop.IVsWindowFrame windowFrame, WindowFrameShowAction windowFrameAction)
		{
			ProjectInfo projectInfo = ProjectInfo.FindProject(GetFullPathForDocument());
			
			if (projectInfo != null)
				projectInfo.IsDocumentOpening = true;

			try
			{
				return base.OpenWithSpecific(editorFlags, ref editorType, physicalView, ref logicalView, docDataExisting, out windowFrame, windowFrameAction);
			}
			finally
			{
				if (projectInfo != null)
					projectInfo.IsDocumentOpening = false;
			}
		}
	}
}
