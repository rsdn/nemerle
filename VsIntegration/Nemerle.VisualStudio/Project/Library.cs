using System;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.Project
{
	public class Library : IVsSimpleLibrary2
	{
		Guid		_guid;
		_LIB_FLAGS2 _capabilities;
		LibraryNode _root;

		public Library(Guid libraryGuid)
		{
			_guid = libraryGuid;
			_root = new LibraryNode("", LibraryNode.LibraryNodeType.Package);
		}

		public _LIB_FLAGS2 LibraryCapabilities
		{
			get { return _capabilities;  }
			set { _capabilities = value; }
		}

		internal void AddNode(LibraryNode node)
		{
			lock (this)
			{
				_root = new LibraryNode(_root);
				_root.AddNode(node);
			}
		}

		internal void RemoveNode(LibraryNode node)
		{
			lock (this)
			{
				_root = new LibraryNode(_root);
				_root.RemoveNode(node);
			}
		}

		#region IVsSimpleLibrary2 Members

		public int AddBrowseContainer(
			VSCOMPONENTSELECTORDATA[] pcdComponent, ref uint pgrfOptions, out string pbstrComponentAdded)
		{
			pbstrComponentAdded = null;
			return VSConstants.E_NOTIMPL;
		}

		public int CreateNavInfo(
			SYMBOL_DESCRIPTION_NODE[] rgSymbolNodes, uint ulcNodes, out IVsNavInfo ppNavInfo)
		{
			ppNavInfo = null;
			return VSConstants.E_NOTIMPL;
		}

		public int GetBrowseContainersForHierarchy(
			IVsHierarchy pHierarchy, uint celt, VSBROWSECONTAINER[] rgBrowseContainers, uint[] pcActual)
		{
			return VSConstants.E_NOTIMPL;
		}

		public int GetGuid(out Guid pguidLib)
		{
			pguidLib = _guid;
			return VSConstants.S_OK;
		}

		public int GetLibFlags2(out uint pgrfFlags)
		{
			pgrfFlags = (uint)LibraryCapabilities;
			return VSConstants.S_OK;
		}

		public int GetList2(
			uint					 ListType,
			uint					 flags, 
			VSOBSEARCHCRITERIA2[]	pobSrch,
			out IVsSimpleObjectList2 ppIVsSimpleObjectList2)
		{
			ppIVsSimpleObjectList2 = _root;
			return VSConstants.S_OK;
		}

		public int GetSeparatorStringWithOwnership(out string pbstrSeparator)
		{
			pbstrSeparator = ".";
			return VSConstants.S_OK;
		}

		public int GetSupportedCategoryFields2(int Category, out uint pgrfCatField)
		{
			pgrfCatField = (uint)_LIB_CATEGORY2.LC_HIERARCHYTYPE | (uint)_LIB_CATEGORY2.LC_PHYSICALCONTAINERTYPE;
			return VSConstants.S_OK;
		}

		public int LoadState(IStream pIStream, LIB_PERSISTTYPE lptType)
		{
			return VSConstants.S_OK;
		}

		public int RemoveBrowseContainer(uint dwReserved, string pszLibName)
		{
			return VSConstants.E_NOTIMPL;
		}

		public int SaveState(IStream pIStream, LIB_PERSISTTYPE lptType)
		{
			return VSConstants.S_OK;
		}

		public int UpdateCounter(out uint pCurUpdate)
		{
			return ((IVsSimpleObjectList2)_root).UpdateCounter(out pCurUpdate);
		}

		#endregion
	}
}
