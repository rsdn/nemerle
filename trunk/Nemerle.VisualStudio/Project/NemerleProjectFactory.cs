using System;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Package;

using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;

namespace Nemerle.VisualStudio.Project
{
	[Guid(NemerleConstants.ProjectFactoryGuidString)]
	public class NemerleProjectFactory : ProjectFactory
	{
		public NemerleProjectFactory(NemerlePackage package)
			: base(package)
		{
		}

		protected override ProjectNode CreateProject()
		{
			ProjectNode		 project			= new NemerleProjectNode(Package);
			IOleServiceProvider oleServiceProvider = Package.GetService<IOleServiceProvider>();

			project.SetSite(oleServiceProvider);

			return project;
		}

		public new NemerlePackage Package
		{
			get { return (NemerlePackage)base.Package; }
		}
	}
}
