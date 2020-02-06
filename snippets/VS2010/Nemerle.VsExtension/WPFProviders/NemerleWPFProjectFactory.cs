using System;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Shell.Flavor;

namespace Nemerle.VisualStudio.WPFProviders
{
	[Guid("B67E91ED-3EEB-409b-B372-B92405305B96")]
	public class NemerleWPFProjectFactory : FlavoredProjectFactoryBase
	{
		private IServiceProvider _site;

		public NemerleWPFProjectFactory(IServiceProvider site)
		{
			_site = site;
		}

		/// <summary>
		/// Create an instance of our project. The initialization will be done later
		/// when VS calls InitalizeForOuter on it.
		/// </summary>
		/// <param name="outerProjectIUnknown">This is only useful if someone else is subtyping us</param>
		/// <returns>An uninitialized instance of our project</returns>
		protected override object PreCreateForOuter(IntPtr outerProjectIUnknown)
		{
			return new NemerleWPFFlavor(_site);
		}
	}
}
