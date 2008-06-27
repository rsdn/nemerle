using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Package.Automation;
using Microsoft.VisualStudio.Package;
using System.Runtime.InteropServices;

namespace Nemerle.VisualStudio.Project
{
	[CLSCompliant(false), ComVisible(true)]
	public class NemerleOAProperties : OAProperties
	{
		public NemerleOAProperties(NodeProperties target)
			: base(target)
		{
		}

		public override EnvDTE.Property Item(object index)
		{
			if (index is string)
			{
				string indexAsString = (string)index;
				if (Properties.ContainsKey(indexAsString))
				{
					return (EnvDTE.Property)Properties[indexAsString];
				}
			}
			else if (index is int)
			{
				int realIndex = (int)index - 1;
				if (realIndex >= 0 && realIndex < Properties.Count)
				{
					System.Collections.IEnumerator enumerator = Properties.Values.GetEnumerator();

					int i = 0;
					while (enumerator.MoveNext())
					{
						if (i++ == realIndex)
						{
							return (EnvDTE.Property)enumerator.Current;
						}
					}
				}
			}

				return base.Item(index);
			}
	}
}
