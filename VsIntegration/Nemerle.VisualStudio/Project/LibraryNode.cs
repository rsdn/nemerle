using System;
using System.Collections.Generic;
using System.ComponentModel.Design;

using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using VSConstants  = Microsoft.VisualStudio.VSConstants;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Single node inside the tree of the libraries in the object browser or class view.
	/// </summary>
	class LibraryNode : IVsSimpleObjectList2, IVsNavInfoNode
	{
		public const uint NullIndex = (uint)0xFFFFFFFF;

		/// <summary>
		/// Enumeration of the capabilities of a node. It is possible to combine different values
		/// to support more capabilities.
		/// This enumeration is a copy of _LIB_LISTCAPABILITIES with the Flags attribute set.
		/// </summary>
		[Flags]
		public enum LibraryNodeCapabilities
		{
			None			   = _LIB_LISTCAPABILITIES.LLC_NONE,
			HasBrowseObject	= _LIB_LISTCAPABILITIES.LLC_HASBROWSEOBJ,
			HasDescriptionPane = _LIB_LISTCAPABILITIES.LLC_HASDESCPANE,
			HasSourceContext   = _LIB_LISTCAPABILITIES.LLC_HASSOURCECONTEXT,
			HasCommands		= _LIB_LISTCAPABILITIES.LLC_HASCOMMANDS,
			AllowDragDrop	  = _LIB_LISTCAPABILITIES.LLC_ALLOWDRAGDROP,
			AllowRename		= _LIB_LISTCAPABILITIES.LLC_ALLOWRENAME,
			AllowDelete		= _LIB_LISTCAPABILITIES.LLC_ALLOWDELETE,
			AllowSourceControl = _LIB_LISTCAPABILITIES.LLC_ALLOWSCCOPS,
		}

		/// <summary>
		/// Enumeration of the possible types of node. The type of a node can be the combination
		/// of one of more of these values.
		/// This is actually a copy of the _LIB_LISTTYPE enumeration with the difference that the
		/// Flags attribute is set so that it is possible to specify more than one value.
		/// </summary>
		[Flags]
		public enum LibraryNodeType
		{
			None				   = 0,
			Hierarchy			  = _LIB_LISTTYPE.LLT_HIERARCHY,
			Namespaces			 = _LIB_LISTTYPE.LLT_NAMESPACES,
			Classes				= _LIB_LISTTYPE.LLT_CLASSES,
			Members				= _LIB_LISTTYPE.LLT_MEMBERS,
			Package				= _LIB_LISTTYPE.LLT_PACKAGE,
			PhysicalContainer	  = _LIB_LISTTYPE.LLT_PHYSICALCONTAINERS,
			Containment			= _LIB_LISTTYPE.LLT_CONTAINMENT,
			ContainedBy			= _LIB_LISTTYPE.LLT_CONTAINEDBY,
			UsesClasses			= _LIB_LISTTYPE.LLT_USESCLASSES,
			UsedByClasses		  = _LIB_LISTTYPE.LLT_USEDBYCLASSES,
			NestedClasses		  = _LIB_LISTTYPE.LLT_NESTEDCLASSES,
			InheritedInterface	 = _LIB_LISTTYPE.LLT_INHERITEDINTERFACES,
			InterfaceUsedByClasses = _LIB_LISTTYPE.LLT_INTERFACEUSEDBYCLASSES,
			Definitions			= _LIB_LISTTYPE.LLT_DEFINITIONS,
			References			 = _LIB_LISTTYPE.LLT_REFERENCES,
			DeferExpansion		 = _LIB_LISTTYPE.LLT_DEFEREXPANSION,
		}

		private string				  _name;
		private LibraryNodeType		 _type;
		private List<LibraryNode>	   _children;
		private LibraryNodeCapabilities _capabilities;
		private List<VSOBJCLIPFORMAT>   _clipboardFormats;
		private VSTREEDISPLAYDATA	   _displayData;
		private _VSTREEFLAGS			_flags;
		private CommandID			   _contextMenuID;
		private string				  _tooltip;
		private uint					_updateCount;
		private Dictionary<LibraryNodeType, LibraryNode> _filteredView;

		public LibraryNode(string name)
			: this(name, LibraryNodeType.None, LibraryNodeCapabilities.None, null)
		{
		}

		public LibraryNode(string name, LibraryNodeType type)
			: this(name, type, LibraryNodeCapabilities.None, null)
		{
		}

		public LibraryNode(
			string name, LibraryNodeType type, LibraryNodeCapabilities capabilities, CommandID contextMenuID)
		{
			_capabilities	 = capabilities;
			_contextMenuID	= contextMenuID;
			_name			 = name;
			_tooltip		  = name;
			_type			 = type;
			_children		 = new List<LibraryNode>();
			_clipboardFormats = new List<VSOBJCLIPFORMAT>();
			_filteredView	 = new Dictionary<LibraryNodeType, LibraryNode>();
		}

		public LibraryNode(LibraryNode node)
		{
			_capabilities  = node._capabilities;
			_contextMenuID = node._contextMenuID;
			_displayData   = node._displayData;
			_name		  = node._name;
			_tooltip	   = node._tooltip;
			_type		  = node._type;
			_children	  = new List<LibraryNode>();

			foreach (LibraryNode child in node._children)
				_children.Add(child);

			_clipboardFormats = new List<VSOBJCLIPFORMAT>();

			foreach (VSOBJCLIPFORMAT format in node._clipboardFormats)
				_clipboardFormats.Add(format);

			_filteredView = new Dictionary<LibraryNodeType, LibraryNode>();
			_updateCount  = node._updateCount;
		}

		protected void SetCapabilityFlag(LibraryNodeCapabilities flag, bool value)
		{
			if (value) _capabilities |= flag;
			else	   _capabilities &= ~flag;
		}

		/// <summary>
		/// Get or Set if the node can be deleted.
		/// </summary>
		public bool CanDelete
		{
			get { return (_capabilities & LibraryNodeCapabilities.AllowDelete) != 0; }
			set { SetCapabilityFlag(LibraryNodeCapabilities.AllowDelete, value);	 }
		}

		/// <summary>
		/// Get or Set if the node can be associated with some source code.
		/// </summary>
		public bool CanGoToSource
		{
			get { return (_capabilities & LibraryNodeCapabilities.HasSourceContext) != 0; }
			set { SetCapabilityFlag(LibraryNodeCapabilities.HasSourceContext, value);	 }
		}

		/// <summary>
		/// Get or Set if the node can be renamed.
		/// </summary>
		public bool CanRename
		{
			get { return (_capabilities & LibraryNodeCapabilities.AllowRename) != 0; }
			set { SetCapabilityFlag(LibraryNodeCapabilities.AllowRename, value);	 }
		}

		public LibraryNodeCapabilities Capabilities
		{
			get { return _capabilities;  }
			set { _capabilities = value; }
		}

		public _VSTREEFLAGS Flags
		{
			get { return _flags;  }
			set { _flags = value; }
		}

		public string TooltipText
		{
			get { return _tooltip;  }
			set { _tooltip = value; }
		}

		internal void AddNode(LibraryNode node)
		{
			lock (_children)
			{
				_children.Add(node);
			}

			_updateCount++;
		}

		internal void RemoveNode(LibraryNode node)
		{
			lock (_children)
			{
				_children.Remove(node);
			}

			_updateCount++;
		}

		protected virtual object BrowseObject
		{
			get { return null; }
		}

		protected virtual uint CategoryField(LIB_CATEGORY category)
		{
			switch (category)
			{
				case LIB_CATEGORY.LC_LISTTYPE:
					LibraryNodeType subTypes = LibraryNodeType.None;

					foreach (LibraryNode node in _children)
						subTypes |= node._type;

					return (uint)subTypes;

				case (LIB_CATEGORY)_LIB_CATEGORY2.LC_HIERARCHYTYPE:
					return (uint)_LIBCAT_HIERARCHYTYPE.LCHT_UNKNOWN;

				default:
					throw new NotImplementedException();
			}
		}

		protected virtual LibraryNode Clone()
		{
			return new LibraryNode(this);
		}

		/// <summary>
		/// Performs the operations needed to delete this node.
		/// </summary>
		protected virtual void Delete()
		{
		}

		/// <summary>
		/// Perform a Drag and Drop operation on this node.
		/// </summary>
		protected virtual void DoDragDrop(OleDataObject dataObject, uint keyState, uint effect)
		{
		}

		protected virtual uint EnumClipboardFormats(_VSOBJCFFLAGS flags, VSOBJCLIPFORMAT[] formats)
		{
			if ((null == formats) || (formats.Length == 0))
				return (uint)_clipboardFormats.Count;

			uint itemsToCopy = (uint)_clipboardFormats.Count;

			if (itemsToCopy > (uint)formats.Length)
				itemsToCopy = (uint)formats.Length;

			Array.Copy(_clipboardFormats.ToArray(), formats, (int)itemsToCopy);

			return itemsToCopy;
		}

		protected virtual void FillDescription(_VSOBJDESCOPTIONS flags, IVsObjectBrowserDescription3 description)
		{
			description.ClearDescriptionText();
			description.AddDescriptionText3(_name, VSOBDESCRIPTIONSECTION.OBDS_NAME, null);
		}

		protected IVsSimpleObjectList2 FilterView(LibraryNodeType filterType)
		{
			LibraryNode filtered;

			if (_filteredView.TryGetValue(filterType, out filtered))
				return filtered;

			filtered = Clone();

			for (int i = 0; i < filtered._children.Count; )
			{
				if (0 == (filtered._children[i]._type & filterType))
					filtered._children.RemoveAt(i);
				else
					i += 1;
			}
			
			_filteredView.Add(filterType, filtered);

			return filtered;
		}

		protected virtual void GotoSource(VSOBJGOTOSRCTYPE gotoType)
		{
			// Do nothing.
		}

		public string Name
		{
			get { return _name;  }
			set { _name = value; }
		}

		public LibraryNodeType NodeType
		{
			get { return _type;  }
			set { _type = value; }
		}

		/// <summary>
		/// Finds the source files associated with this node.
		/// </summary>
		/// <param name="hierarchy">The hierarchy containing the items.</param>
		/// <param name="itemId">The item id of the item.</param>
		/// <param name="itemsCount">Number of items.</param>
		protected virtual void SourceItems(out IVsHierarchy hierarchy, out uint itemId, out uint itemsCount)
		{
			hierarchy  = null;
			itemId	 = 0;
			itemsCount = 0;
		}

		protected virtual void Rename(string newName, uint flags)
		{
			_name = newName;
		}

		public virtual string UniqueName
		{
			get { return Name; }
		}

		#region IVsSimpleObjectList2 Members

		int IVsSimpleObjectList2.CanDelete(uint index, out int pfOK)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			pfOK = _children[(int)index].CanDelete ? 1 : 0;
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.CanGoToSource(uint index, VSOBJGOTOSRCTYPE SrcType, out int pfOK)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			pfOK = _children[(int)index].CanGoToSource ? 1 : 0;
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.CanRename(uint index, string pszNewName, out int pfOK)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			pfOK = _children[(int)index].CanRename ? 1 : 0;
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.CountSourceItems(uint index, out IVsHierarchy ppHier, out uint pItemid, out uint pcItems)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			_children[(int)index].SourceItems(out ppHier, out pItemid, out pcItems);
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.DoDelete(uint index, uint grfFlags)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			_children[(int)index].Delete();
			_children.RemoveAt((int)index);

			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.DoDragDrop(uint index, IDataObject pDataObject, uint grfKeyState, ref uint pdwEffect)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			OleDataObject dataObject = new OleDataObject(pDataObject);

			_children[(int)index].DoDragDrop(dataObject, grfKeyState, pdwEffect);

			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.DoRename(uint index, string pszNewName, uint grfFlags)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			_children[(int)index].Rename(pszNewName, grfFlags);
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.EnumClipboardFormats(uint index, uint grfFlags, uint celt, VSOBJCLIPFORMAT[] rgcfFormats, uint[] pcActual)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			uint copied = _children[(int)index].EnumClipboardFormats((_VSOBJCFFLAGS)grfFlags, rgcfFormats);

			if ((null != pcActual) && (pcActual.Length > 0))
				pcActual[0] = copied;

			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.FillDescription2(uint index, uint grfOptions, IVsObjectBrowserDescription3 pobDesc)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			_children[(int)index].FillDescription((_VSOBJDESCOPTIONS)grfOptions, pobDesc);
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetBrowseObject(uint index, out object ppdispBrowseObj)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			ppdispBrowseObj = _children[(int)index].BrowseObject;

			if (null == ppdispBrowseObj)
				return VSConstants.E_NOTIMPL;

			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetCapabilities2(out uint pgrfCapabilities)
		{
			pgrfCapabilities = (uint)Capabilities;
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetCategoryField2(uint index, int Category, out uint pfCatField)
		{
			LibraryNode node;

			if (NullIndex == index)
			{
				node = this;
			}
			else if (index < (uint)_children.Count)
			{
				node = _children[(int)index];
			}
			else
			{
				throw new ArgumentOutOfRangeException("index");
			}

			pfCatField = node.CategoryField((LIB_CATEGORY)Category);
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetClipboardFormat(uint index, uint grfFlags, FORMATETC[] pFormatetc, STGMEDIUM[] pMedium)
		{
			return VSConstants.E_NOTIMPL;
		}

		int IVsSimpleObjectList2.GetContextMenu(uint index, out Guid pclsidActive, out int pnMenuId, out IOleCommandTarget ppCmdTrgtActive)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			CommandID commandId = _children[(int)index]._contextMenuID;

			if (commandId == null)
			{
				pclsidActive	= Guid.Empty;
				pnMenuId		= 0;
				ppCmdTrgtActive = null;

				return VSConstants.E_NOTIMPL;
			}

			pclsidActive	= commandId.Guid;
			pnMenuId		= commandId.ID;
			ppCmdTrgtActive = _children[(int)index] as IOleCommandTarget;

			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetDisplayData(uint index, VSTREEDISPLAYDATA[] pData)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			pData[0] = _children[(int)index]._displayData;
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetExpandable3(uint index, uint ListTypeExcluded, out int pfExpandable)
		{
			// There is a not empty implementation of GetCategoryField2, so this method should
			// return E_NOTIMPL.
			pfExpandable = 0;
			return VSConstants.E_NOTIMPL;
		}

		int IVsSimpleObjectList2.GetExtendedClipboardVariant(uint index, uint grfFlags, VSOBJCLIPFORMAT[] pcfFormat, out object pvarFormat)
		{
			pvarFormat = null;
			return VSConstants.E_NOTIMPL;
		}

		int IVsSimpleObjectList2.GetFlags(out uint pFlags)
		{
			pFlags = (uint)Flags;
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetItemCount(out uint pCount)
		{
			pCount = (uint)_children.Count;
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetList2(uint index, uint ListType, uint flags, VSOBSEARCHCRITERIA2[] pobSrch, out IVsSimpleObjectList2 ppIVsSimpleObjectList2)
		{
			// TODO: Use the flags and list type to actually filter the result.
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			ppIVsSimpleObjectList2 = _children[(int)index].FilterView((LibraryNodeType)ListType);
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetMultipleSourceItems(uint index, uint grfGSI, uint cItems, VSITEMSELECTION[] rgItemSel)
		{
			return VSConstants.E_NOTIMPL;
		}

		int IVsSimpleObjectList2.GetNavInfo(uint index, out IVsNavInfo ppNavInfo)
		{
			ppNavInfo = null;
			return VSConstants.E_NOTIMPL;
		}

		int IVsSimpleObjectList2.GetNavInfoNode(uint index, out IVsNavInfoNode ppNavInfoNode)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			ppNavInfoNode = _children[(int)index] as IVsNavInfoNode;
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetProperty(uint index, int propid, out object pvar)
		{
			pvar = null;
			return VSConstants.E_NOTIMPL;
		}

		int IVsSimpleObjectList2.GetSourceContextWithOwnership(uint index, out string pbstrFilename, out uint pulLineNum)
		{
			pbstrFilename = null;
			pulLineNum = (uint)0;
			return VSConstants.E_NOTIMPL;
		}

		int IVsSimpleObjectList2.GetTextWithOwnership(uint index, VSTREETEXTOPTIONS tto, out string pbstrText)
		{
			// TODO: make use of the text option.
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			pbstrText = _children[(int)index]._name;
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetTipTextWithOwnership(uint index, VSTREETOOLTIPTYPE eTipType, out string pbstrText)
		{
			// TODO: Make use of the tooltip type.
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			pbstrText = _children[(int)index].TooltipText;
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.GetUserContext(uint index, out object ppunkUserCtx)
		{
			ppunkUserCtx = null;
			return VSConstants.E_NOTIMPL;
		}

		int IVsSimpleObjectList2.GoToSource(uint index, VSOBJGOTOSRCTYPE SrcType)
		{
			if (index >= (uint)_children.Count)
				throw new ArgumentOutOfRangeException("index");

			_children[(int)index].GotoSource(SrcType);
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.LocateNavInfoNode(IVsNavInfoNode pNavInfoNode, out uint pulIndex)
		{
			if (null == pNavInfoNode)
				throw new ArgumentNullException("pNavInfoNode");

			pulIndex = NullIndex;

			string nodeName;

			ErrorHandler.ThrowOnFailure(pNavInfoNode.get_Name(out nodeName));

			for (int i = 0; i < _children.Count; i++)
			{
				if (0 == string.Compare(_children[i].UniqueName, nodeName, StringComparison.OrdinalIgnoreCase))
				{
					pulIndex = (uint)i;
					return VSConstants.S_OK;
				}
			}

			return VSConstants.S_FALSE;
		}

		int IVsSimpleObjectList2.OnClose(VSTREECLOSEACTIONS[] ptca)
		{
			// Do Nothing.
			return VSConstants.S_OK;
		}

		int IVsSimpleObjectList2.QueryDragDrop(uint index, IDataObject pDataObject, uint grfKeyState, ref uint pdwEffect)
		{
			return VSConstants.E_NOTIMPL;
		}

		int IVsSimpleObjectList2.ShowHelp(uint index)
		{
			return VSConstants.E_NOTIMPL;
		}

		int IVsSimpleObjectList2.UpdateCounter(out uint pCurUpdate)
		{
			pCurUpdate = _updateCount;
			return VSConstants.S_OK;
		}

		#endregion

		#region IVsNavInfoNode Members

		int IVsNavInfoNode.get_Name(out string pbstrName)
		{
			pbstrName = UniqueName;
			return VSConstants.S_OK;
		}

		int IVsNavInfoNode.get_Type(out uint pllt)
		{
			pllt = (uint)_type;
			return VSConstants.S_OK;
		}

		#endregion
	}
}
