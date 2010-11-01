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
using Microsoft.VisualStudio.Project.Automation;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject
{
	/// <summary>
	/// This class provides automation support for ProjectNode.
	/// </summary>
	/// <remarks>This class has public scope in order for COM to recognize this class</remarks>
	[ComVisible(true)]
	public class OANestedProjectProperty : EnvDTE.Property
	{
		#region Fields
		private OAProperties parent;
		private string name = String.Empty;
		#endregion Fields

		#region Constructors
		/// <summary>
		/// Default public constructor for Com visibility.
		/// </summary>
		public OANestedProjectProperty()
		{
		}
		/// <summary>
		/// Initializes new instance of OANestedProjectProperty object based on specified 
		/// parent ProjectNode and Property name.
		/// </summary>
		/// <param name="parent">Parent properties collection.</param>
		/// <param name="name">Project property name.</param>
		internal OANestedProjectProperty(OANestedProjectProperties parent, string name)
		{
			this.parent = parent;
			this.name = name;
		}
		#endregion

		#region Properties
		/// <summary>
		/// Microsoft Internal Use Only.
		/// </summary>
		public object Application
		{
			get { return null; }
		}
		/// <summary>
		/// Gets the Collection containing the Property object supporting this property.
		/// </summary>
		public EnvDTE.Properties Collection
		{
			get
			{
				return this.parent;
			}
		}
		/// <summary>
		/// Gets the top-level extensibility object.
		/// </summary>
		public EnvDTE.DTE DTE
		{
			get
			{
				return this.parent.DTE;
			}
		}
		/// <summary>
		/// Returns one element of a list. 
		/// </summary>
		/// <param name="Index1">The index of the item to display.</param>
		/// <param name="Index2">The index of the item to display. Reserved for future use.</param>
		/// <param name="Index3">The index of the item to display. Reserved for future use.</param>
		/// <param name="Index4">The index of the item to display. Reserved for future use.</param>
		/// <returns>The value of a property</returns>
		// The message is suppressed to follow the csharp naming conventions instead of the base's naming convention that is using c++
		[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration")]
		public object get_IndexedValue(object Index1, object Index2, object Index3, object Index4)
		{
			return null;
		}

		/// <summary>
		/// Setter function to set properties values. 
		/// </summary>
		/// <param name="lppvReturn"></param>
		public void let_Value(object lppvReturn)
		{
			this.Value = lppvReturn;
		}

		/// <summary>
		/// Gets the name of the object.
		/// </summary>
		public string Name
		{
			get { return this.name; }
		}

		/// <summary>
		/// Gets the number of indices required to access the value.
		/// </summary>
		public short NumIndices
		{
			get { return 0; }
		}

		/// <summary>
		/// Sets or gets the object supporting the Property object.
		/// </summary>
		// The message is suppressed to follow the csharp naming conventions instead of the base's naming convention that is using c++
		[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration")]
		public object Object
		{
			get
			{
				return this.Value;
			}
			set
			{
				this.Value = value;
			}
		}

		/// <summary>
		/// Microsoft Internal Use Only.
		/// </summary>
		public EnvDTE.Properties Parent
		{
			get { return this.parent; }
		}

		/// <summary>
		/// Sets the value of the property at the specified index.
		/// </summary>
		/// <param name="Index1">The index of the item to set.</param>
		/// <param name="Index2">Reserved for future use.</param>
		/// <param name="Index3">Reserved for future use.</param>
		/// <param name="Index4">Reserved for future use.</param>
		/// <param name="value">The value to set.</param>
		public void set_IndexedValue(object Index1, object Index2, object Index3, object Index4, object Val)
		{
		}

		/// <summary>
		/// Gets or sets the value of the property returned by the Property object.
		/// </summary>
		public object Value
		{
			get
			{
				return this.parent.Target.Node.ProjectMgr.GetProjectProperty(this.name);
			}
			set
			{
				if(value == null)
				{
					throw new ArgumentNullException("value");
				}

				if(value is string)
				{
					this.parent.Target.Node.ProjectMgr.SetProjectProperty(this.name, value.ToString());
				}
				else
				{
					this.parent.Target.Node.ProjectMgr.SetProjectProperty(this.name, value.ToString());
				}
			}
		}
		#endregion Properties
	}
}
