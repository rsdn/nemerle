/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject
{
	/// <summary>
	/// This class provides automation support for ProjectNode.
	/// </summary>
	[ComVisible(true)]
	public class OANestedProjectProperties : OAProperties
	{
		#region Constructors
		/// <summary>
		/// Initializes new instance of OANestedProjectProperties object based on specified ProjectNode.
		/// </summary>
		/// <param name="project">The Project Node.</param>
		internal OANestedProjectProperties(NodeProperties nodeProperties)
			: base(nodeProperties)
		{
			this.AddProperties();
		}
		#endregion

		#region Methods
		/// <summary>
		/// Initialize properties by names, which defined in property page tag.
		/// </summary>
		private void AddProperties()
		{
			foreach(string tag in Enum.GetNames(typeof(GeneralPropertyPageTag)))
			{
				this.Properties.Add(tag, new OANestedProjectProperty(this, tag));
			}
		}
		#endregion Methods
	}
}