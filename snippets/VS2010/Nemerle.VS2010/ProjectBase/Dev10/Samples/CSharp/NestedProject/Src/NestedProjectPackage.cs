/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Project;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject
{

	/// <summary>
	/// This class implements the ProjectPackage that registers our package 
	/// within VS and handles the solution listeners.
	/// </summary>
	/// <remarks>
	/// <para>A Visual Studio component can be registered under different registry roots; for instance
	/// when you debug your package you want to register it in the experimental hive. This
	/// attribute specifies the registry root to use if no one is provided to regpkg.exe with
	/// the /root switch.</para>
	/// <para>A description of the different attributes used here is given below:</para>
	/// <para>DefaultRegistryRoot: This defines the default registry root for registering the package. 
	/// We are currently using the experimental hive.</para>
	/// <para>ProvideProjectFactory: Declares that a package provides a project factory.</para>
	/// <para>ProvideProjectItem: Declares that a package provides a project item.</para> 
	/// <para>ProvideObject: Declares that a package provides creatable objects of specified type.</para> 
	/// </remarks>
	[DefaultRegistryRoot(@"Software\Microsoft\VisualStudio\9.0Exp")]
    [ProvideProjectFactory(typeof(NestedProjectFactory), "MyNestedProject", "MyNestedProject Files (*.nestedproj);*.nestedproj", "nestedproj", "nestedproj", @"Templates\Projects\MyNestedProject")]
    [ProvideProjectItem(typeof(NestedProjectFactory), "Nested Project Items", @"Templates\ProjectItems\MyNestedProject", 500)]
	[ProvideObject(typeof(GeneralPropertyPage))]
	[ProvideObject(typeof(NestedProjectBuildPropertyPage))]
	[GuidAttribute(GuidStrings.GuidNestedProjectPackage)]
	public class NestedProjectPackage : ProjectPackage
	{
		#region Methods
		/// <summary>
		/// Perform base initialization and register project factory.
		/// </summary>
		protected override void Initialize()
		{
			base.Initialize();
			this.RegisterProjectFactory(new NestedProjectFactory(this));
		}

		public override string ProductUserContext
		{
			get { return "NestedProj"; }
		}

		#endregion Methods
	}
}