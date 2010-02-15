using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Project;
using System.Runtime.InteropServices;
using System.Collections;

namespace Nemerle.VisualStudio.Project
{
	[CLSCompliant(false), ComVisible(true)]
	public class NemerleOAProperties : OAProperties
	{
		public NemerleOAProperties(NodeProperties target)
			: base(target)
		{
		}

		/// <summary>
		/// Returns an indexed member of a Properties collection. 
		/// </summary>
		/// <param name="index">The index at which to return a mamber.</param>
		/// <returns>A Property object.</returns>
		[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily")]
		public override EnvDTE.Property Item(object index)
		{
			string indexAsString = index as string;
			if (indexAsString != null)
			{
				if (this.Properties.ContainsKey(indexAsString))
					return (EnvDTE.Property)this.Properties[indexAsString];
			}
			else if (index is int)
			{
				int realIndex = (int)index - 1;
				if (realIndex >= 0 && realIndex < this.Properties.Count)
				{
					IEnumerator enumerator = this.Properties.Values.GetEnumerator();

					int i = 0;
					while (enumerator.MoveNext())
						if (i++ == realIndex)
							return (EnvDTE.Property)enumerator.Current;
				}
			}

			throw new NotImplementedException(); // no throw exception as in MPF implementation!
		}
	}
}
