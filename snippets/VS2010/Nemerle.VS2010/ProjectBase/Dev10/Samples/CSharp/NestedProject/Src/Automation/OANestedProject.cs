/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using System.Runtime.InteropServices;
using System.Diagnostics.CodeAnalysis;
using System;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject
{
	/// <summary>
	/// This class provides automation support for ProjectNode.
	/// </summary>
	[SuppressMessage("Microsoft.Interoperability", "CA1405:ComVisibleTypeBaseTypesShouldBeComVisible")]
	[ComVisible(true), CLSCompliant(false)]
	public class OANestedProject : OAProject
	{
		#region Constructors
		/// <summary>
		/// Initializes new instance of OANestedProject object based on specified ProjectNode.
		/// </summary>
		/// <param name="project">The Project Node.</param>
		public OANestedProject(ProjectNode project)
			: base(project)
		{
		}
		#endregion

		#region Properties
		/// <summary>
		/// Gets nested project properties.
		/// </summary>
		public override EnvDTE.Properties Properties
		{
			get
			{
				return new OANestedProjectProperties(this.Project.NodeProperties);
			}
		}
		#endregion
	}
}