using System.ComponentModel;
using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Subclasses <see cref="DesignPropertyDescriptor"/> to allow for non-bolded text in the property grid.
	/// </summary>
	public class NemerleNonMemberDesignPropertyDescriptor : DesignPropertyDescriptor
	{
		#region ctors

		/// <summary>
		/// Initializes a new instance of the <see cref="NemerleNonMemberDesignPropertyDescriptor"/> class.
		/// </summary>
		/// <param name="propertyDescriptor">The <see cref="PropertyDescriptor"/> to wrap.</param>
		public NemerleNonMemberDesignPropertyDescriptor(PropertyDescriptor propertyDescriptor)
			: base(propertyDescriptor)
		{
		}

		#endregion

		#region Overridden Properties

		/// <summary>
		/// Properties of non-member nodes should be read-only.
		/// </summary>
		public override bool IsReadOnly
		{
			get
			{
				return true;
			}
		}

		#endregion

		#region Overridden Methods

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
