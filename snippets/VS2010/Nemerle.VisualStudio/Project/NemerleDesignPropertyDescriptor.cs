using System;
using System.ComponentModel;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Subclasses <see cref="DesignPropertyDescriptor"/> to allow for non-bolded text in the property grid.
  /// </summary>
	public class NemerleDesignPropertyDescriptor : DesignPropertyDescriptor
	{
		#region Constructors

		/// <summary>
		/// Initializes a new instance of the <see cref="WixDesignPropertyDescriptor"/> class.
		/// </summary>
		/// <param name="propertyDescriptor">The <see cref="PropertyDescriptor"/> to wrap.</param>
		public NemerleDesignPropertyDescriptor(PropertyDescriptor propertyDescriptor)
			: base(propertyDescriptor)
		{
		}

		#endregion

		#region Methods

		/// <summary>
		/// By returning false here, we're always going to show the property as non-bolded.
		/// </summary>
		/// <param name="component">The component to check.</param>
		/// <returns>Always returns false.</returns>
		public override bool ShouldSerializeValue(object component)
		{
			return false;
		}

		#endregion
	}
}
