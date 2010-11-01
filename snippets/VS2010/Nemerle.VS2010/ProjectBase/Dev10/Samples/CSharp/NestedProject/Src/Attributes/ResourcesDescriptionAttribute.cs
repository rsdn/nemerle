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
	[AttributeUsage(AttributeTargets.All)]
	internal sealed class ResourcesDescriptionAttribute : DescriptionAttribute
	{
		private bool replaced;

		public ResourcesDescriptionAttribute(string description)
			: base(description)
		{
		}

		public override string Description
		{
			get
			{
				if(!replaced)
				{
					replaced = true;
					DescriptionValue = Resources.GetString(base.Description);
				}
				return base.Description;
			}
		}
	}
}