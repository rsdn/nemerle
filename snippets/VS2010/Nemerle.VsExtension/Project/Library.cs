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

		#region Find All Reference implementation

		/// <summary>
		/// Used to distinguish "Find All References" search from other search types.
		/// Используется для того чтобы отличить поиск "Find All References" от других видов поиска.
		/// </summary>
		public const uint FindAllReferencesMagicNum = 0x11123334;

		/// <summary>
		/// Stores existing "Find All References" results in the _findResults field.
		/// Сохраняет уже готовые результаты поиска "Find All References" в поле _findResults.
		/// </summary>
		public void OnFindAllReferencesDone(IVsSimpleObjectList2 findResults)
		{
			_findResults = findResults;
		}

		private IVsSimpleObjectList2 _findResults = null;

		/// <summary>
		/// Этот метод один для целой кучи функциональности: Find All References, Calls From, Calls To, и других
		/// </summary>
		public int GetList2(
			uint					 ListType,
			uint					 flags, 
			VSOBSEARCHCRITERIA2[]	pobSrch,
			out IVsSimpleObjectList2 ppIVsSimpleObjectList2)
		{
			if((_findResults != null) && (pobSrch != null) && (pobSrch.Length == 1) && (pobSrch[0].dwCustom == FindAllReferencesMagicNum))
			{//если _findResults заполнены, то возвращаем их
				ppIVsSimpleObjectList2 = _findResults;
				_findResults = null;
				return VSConstants.S_OK;
			}

			//TODO: (hi_octane) хочу подключить CallBrowser
			//TODO: for CallBrowser (Calls From, Calls To), we need to follow these article:
			// По реализации CallBrowser что-то есть здесь:
			// http://social.msdn.microsoft.com/Forums/en-US/vsx/thread/8bc7d011-57c5-4aef-813a-a5a38170ae68/
			// и более подробно здесь:
			// http://msdn.microsoft.com/en-us/library/bb164614(v=VS.90).aspx - VS2008, работает и в 2010
			// http://msdn.microsoft.com/en-us/library/bb164724.aspx - VS2010
			// про работу с CallBrowser через CodeModel посылают сюда:
			// http://msdn.microsoft.com/en-us/library/ms228770.aspx

			ppIVsSimpleObjectList2 = _root;
			return VSConstants.S_OK;
		}

		#endregion Find All Reference implementation

		public int GetSeparatorStringWithOwnership(out string pbstrSeparator)
		{
			pbstrSeparator = ".";
			return VSConstants.S_OK;
		}

		/// <summary>
		/// Старая реализация была не верна, но и эта почему-то не работает
		/// </summary>
		public int GetSupportedCategoryFields2(int category, out uint pCatField)
		{
			//pgrfCatField = (uint)_LIB_CATEGORY2.LC_HIERARCHYTYPE | (uint)_LIB_CATEGORY2.LC_PHYSICALCONTAINERTYPE;
			//return VSConstants.S_OK;
			pCatField = 0;

			switch(category)
			{
				case (int)LIB_CATEGORY.LC_MEMBERTYPE:
					pCatField = (uint)_LIBCAT_MEMBERTYPE.LCMT_METHOD;
					break;

				case (int)LIB_CATEGORY.LC_MEMBERACCESS:
					pCatField = (uint)_LIBCAT_MEMBERACCESS.LCMA_PUBLIC |
								(uint)_LIBCAT_MEMBERACCESS.LCMA_PRIVATE |
								(uint)_LIBCAT_MEMBERACCESS.LCMA_PROTECTED |
								(uint)_LIBCAT_MEMBERACCESS.LCMA_PACKAGE |
								(uint)_LIBCAT_MEMBERACCESS.LCMA_FRIEND |
								(uint)_LIBCAT_MEMBERACCESS.LCMA_SEALED;
					break;

				case (int)LIB_CATEGORY.LC_LISTTYPE:
					pCatField = (uint)_LIB_LISTTYPE.LLT_MEMBERS;
					break;

				case (int)LIB_CATEGORY.LC_VISIBILITY:
					pCatField = (uint)(_LIBCAT_VISIBILITY.LCV_VISIBLE |
										_LIBCAT_VISIBILITY.LCV_HIDDEN);
					break;

				default:
					pCatField = (uint)0;
					return Microsoft.VisualStudio.VSConstants.E_FAIL;
			}

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
