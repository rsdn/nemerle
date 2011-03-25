using System;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.Project
{
	public class SelectionElementValueChangedListener : SelectionListener
	{
		#region ctors

		public SelectionElementValueChangedListener(
			ServiceProvider serviceProvider, ProjectNode proj) : base(serviceProvider)
		{
			projMgr = proj;
		}

		#endregion

		#region Fileds

		ProjectNode projMgr;

		#endregion

		#region Overridden Methods

		public override int OnElementValueChanged(uint elementid, object varValueOld, object varValueNew)
		{
			int hr = VSConstants.S_OK;

			if (elementid == VSConstants.DocumentFrame)
			{
				IVsWindowFrame pWindowFrame = varValueOld as IVsWindowFrame;

				if (pWindowFrame != null)
				{
					object document;

					// Get the name of the document associated with the old window frame.
					//
					hr = pWindowFrame.GetProperty((int)__VSFPROPID.VSFPROPID_pszMkDocument, out document);

					if (ErrorHandler.Succeeded(hr))
					{
						uint		 itemid;
						IVsHierarchy hier = projMgr;

						hr = hier.ParseCanonicalName((string)document, out itemid);

						NemerleFileNode node = projMgr.NodeFromItemId(itemid) as NemerleFileNode;
						
						if (null != node)
							node.RunGenerator();
					}
				}
			}

			return hr;
		}

		#endregion
	}
}