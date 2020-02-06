using System;
using System.Diagnostics;

using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Class used to identify a module. The module is identify using the
	/// hierarchy that contains it and its item id inside the hierarchy.
	/// </summary>
	[DebuggerStepThrough]
	sealed class ModuleID
	{
		public ModuleID(IVsHierarchy owner, uint id)
		{
			_hierarchy = owner;
			_itemID	= id;
		}

		private IVsHierarchy _hierarchy;
		public  IVsHierarchy  Hierarchy
		{
			get { return _hierarchy; }
		}

		private uint _itemID;
		public  uint  ItemID
		{
			[DebuggerStepThrough]
			get { return _itemID; }
		}

		public override int GetHashCode()
		{
			int hash = 0;

			if (null != _hierarchy)
				hash = _hierarchy.GetHashCode();

			hash = hash ^ (int)_itemID;

			return hash;
		}

		public override bool Equals(object obj)
		{
			ModuleID other = obj as ModuleID;

			if (null == obj)
				return false;

			if (!_hierarchy.Equals(other._hierarchy))
				return false;

			return _itemID == other._itemID;
		}
	}

}
