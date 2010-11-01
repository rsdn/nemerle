using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project;
using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;



namespace Nemerle.VS2010
{
    [Guid(GuidList.guidNemerle_VS2010ProjectFactoryString)]
    class NemerleProjectFactory :ProjectFactory
    {
        protected override ProjectNode CreateProject()
        {
            var project = new NemerleProjectNode(this.package);

            project.SetSite((IOleServiceProvider)((IServiceProvider)this.package).GetService(typeof(IOleServiceProvider)));
            return project;

        }
        private Nemerle_VS2010Package package;

        public NemerleProjectFactory(Nemerle_VS2010Package package)
            : base(package)
        {
            this.package = package;
        }
        


    }
}
