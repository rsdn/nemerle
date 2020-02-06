using System;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Shell.Flavor;
using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.WPFProviders
{
	[ComVisible(true)]
	[ClassInterface(ClassInterfaceType.None)]
	[Guid("8B0DD214-CD74-4997-825A-4A24D8148969")]
	public class NemerleWPFFlavor : FlavoredProjectBase
	{
		public NemerleWPFFlavor(IServiceProvider site)
		{
			this.serviceProvider = site;
		}

		protected override Guid GetGuidProperty(uint itemId, int propId)
		{
			if (propId == (int)__VSHPROPID2.VSHPROPID_AddItemTemplatesGuid)
				return typeof(NemerleWPFProjectFactory).GUID;

			return base.GetGuidProperty(itemId, propId);
		}

		protected override int GetProperty(uint itemId, int propId, out object property)
		{
			return base.GetProperty(itemId, propId, out property);
		}
	}
}
