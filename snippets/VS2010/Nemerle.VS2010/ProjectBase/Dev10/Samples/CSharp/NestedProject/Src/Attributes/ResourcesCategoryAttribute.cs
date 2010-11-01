/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.ComponentModel;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject
{
	/// <summary>
	/// Indicates the category to associate the associated property or event with, 
	/// when listing properties or events in a PropertyGrid control set to Categorized mode.
	/// </summary>
	[AttributeUsage(AttributeTargets.All)]
	internal sealed class ResourcesCategoryAttribute : CategoryAttribute
	{
		/// <summary>
		/// Explicit constructor.
		/// </summary>
		/// <param name="category">
		/// Specifies the name of the category in which to group the property 
		/// or event when displayed in a PropertyGrid control set to Categorized mode.
		/// </param>
		public ResourcesCategoryAttribute(string categoryName)
			: base(categoryName)
		{
		}
		/// <summary>
		/// Looks up the localized name of the specified category.
		/// </summary>
		/// <param name="value">The identifier for the category to look up.</param>
		/// <returns>The localized name of the category, or a null reference
		/// if a localized name does not exist.</returns>
		protected override string GetLocalizedString(string categoryName)
		{
			return Resources.GetString(categoryName);
		}
	}
}
