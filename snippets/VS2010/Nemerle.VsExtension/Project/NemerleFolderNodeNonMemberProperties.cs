using System;
using System.ComponentModel;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Represents folder node properties.
	/// </summary>
	/// <remarks>This class must be public and marked as ComVisible in order for the DispatchWrapper to work correctly.</remarks>
	[CLSCompliant(false)]
	[ComVisible(true)]
	[Guid("696121C2-ED25-477a-9257-7644DB136E83")]
	[SuppressMessage("Microsoft.Interoperability", "CA1409:ComVisibleTypesShouldBeCreatable")]
	public class NemerleFolderNodeNonMemberProperties : FolderNodeProperties
	{
		#region ctors

		/// <summary>
		/// Initializes a new instance of the <see cref="NemerleFolderNodeProperties"/> class.
		/// </summary>
		/// <param name="node">The node that contains the properties to expose via the Property Browser.</param>
		public NemerleFolderNodeNonMemberProperties(HierarchyNode node)
			: base(node)
		{
		}

		#endregion

		#region Overriden Methods

		/// <summary>
		/// Creates a custom property descriptor for the node properties, which affects the behavior
		/// of the property grid.
		/// </summary>
		/// <param name="propertyDescriptor">The <see cref="PropertyDescriptor"/> to wrap.</param>
		/// <returns>A custom <see cref="PropertyDescriptor"/> object.</returns>
		[SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#", Justification = "In the 2005 SDK, it's called p and in the 2008 SDK it's propertyDescriptor")]
		public override DesignPropertyDescriptor CreateDesignPropertyDescriptor(PropertyDescriptor propertyDescriptor)
		{
			return new NemerleNonMemberDesignPropertyDescriptor(propertyDescriptor);
		}

		#endregion
	}
}
