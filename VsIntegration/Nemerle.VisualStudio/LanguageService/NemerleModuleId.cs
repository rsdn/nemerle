using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.LanguageService
{
	/// <summary>
	/// Class used to identify a module. The module is identify using the hierarchy that
	/// contains it and its item id inside the hierarchy.
	/// </summary>
	internal sealed class ModuleId
	{
		private readonly IVsHierarchy ownerHierarchy;
		private readonly uint itemId;

		public ModuleId(IVsHierarchy owner, uint id)
		{
			ownerHierarchy = owner;
			itemId = id;
		}

		public IVsHierarchy Hierarchy
		{
			get { return ownerHierarchy; }
		}

		public uint ItemID
		{
			get { return itemId; }
		}

		public override int GetHashCode()
		{
			int hash = 0;
			if (null != ownerHierarchy)
			{
				hash = ownerHierarchy.GetHashCode();
			}
			hash = hash ^ (int)itemId;
			return hash;
		}

		public override bool Equals(object obj)
		{
			ModuleId other = obj as ModuleId;
			if (null == other)
			{
				return false;
			}
			if (!ownerHierarchy.Equals(other.ownerHierarchy))
			{
				return false;
			}
			return (itemId == other.itemId);
		}
	}
}