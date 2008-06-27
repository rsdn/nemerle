using System;
using System.Collections;
using System.Collections.Generic;

using EnvDTE;

namespace Nemerle.VisualStudio.FileCodeModel
{
	/// <summary>
	/// Represents a collection of <see cref="EnvDTE.CodeElements"/>
	/// </summary>
	internal class NemerleCodeElements : CodeElements
	{
		private          object            _parent;
		private          DTE               _dte;
		private readonly ProjectItem       _projectItem;
		private          List<CodeElement> _elements = new List<CodeElement>();

		public NemerleCodeElements(
			DTE dte, ProjectItem projectItem, object parent, params CodeElement[] elements)
		{
			_parent      = parent;
			_dte         = dte;
			_projectItem = projectItem;

			if (elements != null)
				_elements.AddRange(elements);
		}

		public DTE    DTE    { get { return _dte;            } }
		public object Parent { get { return _parent;         } }
		public int    Count  { get { return _elements.Count; } }

		IEnumerator CodeElements.GetEnumerator()
		{
			return _elements.GetEnumerator();
		}

		public IEnumerator GetEnumerator()
		{
			return _elements.GetEnumerator();
		}

		/// <summary>
		/// Accordingly to the MSDN and public Microsoft's samples, the CodeElements collection is 1-based.
		/// </summary>
		/// <param name="index"></param>
		/// <returns></returns>
		public CodeElement Item(object index)
		{
			return index is int? _elements[(int)index-1]: null;
		}

		public void Reserved1(object Element)
		{
			throw new NotImplementedException();
		}

		public bool CreateUniqueID(string Prefix, ref string NewName)
		{
			throw new NotImplementedException();
		}
	}
}