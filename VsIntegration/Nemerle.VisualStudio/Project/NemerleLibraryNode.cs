using System;
using System.Globalization;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// This is a specialized version of the LibraryNode that handles the
	/// Nemerle's items. The main difference from the generic one is that it
	/// supports navigation to the location inside the source code where the 
	/// element is defined.
	/// </summary>
	internal class NemerleLibraryNode : LibraryNode
	{
		IVsHierarchy _ownerHierarchy;
		uint		 _fileId;
		TextSpan	 _sourceSpan;
		string	   _fileMoniker;

		internal NemerleLibraryNode(
			ScopeNode	scope,
			string	   namePrefix,
			IVsHierarchy hierarchy,
			uint		 itemId)
			: base(scope.Name)
		{
			_sourceSpan = new TextSpan();

			//if (scope is FunctionNode)
			//{
			//  this.NodeType = LibraryNodeType.Members;
			//}
			//else if (scope is ClassNode)
			//{
			//  this.NodeType = LibraryNodeType.Classes;
			//  this.Name = string.Format(CultureInfo.InvariantCulture, "{0}{1}", namePrefix, scope.Name);
			//}

			//this.ownerHierarchy = hierarchy;
			//this.fileId = itemId;

			//// Now check if we have all the information to navigate to the source location.
			//if ((null != ownerHierarchy) && (VSConstants.VSITEMID_NIL != fileId))
			//{
			//  if ((0 != Location.Compare(Location.None, scope.Start)) && (0 != Location.Compare(Location.None, scope.End)))
			//  {
			//	sourceSpan = new TextSpan();
			//	sourceSpan.iStartIndex = scope.Start.column;
			//	if (scope.Start.line > 0)
			//	{
			//	  sourceSpan.iStartLine = scope.Start.line - 1;
			//	}
			//	sourceSpan.iEndIndex = scope.End.column;
			//	if (scope.End.line > 0)
			//	{
			//	  sourceSpan.iEndLine = scope.End.line - 1;
			//	}
			//	this.CanGoToSource = true;
			//  }
			//}

			throw new NotImplementedException();
		}

		internal NemerleLibraryNode(NemerleLibraryNode node) : base(node)
		{
			_fileId		 = node._fileId;
			_ownerHierarchy = node._ownerHierarchy;
			_fileMoniker	= node._fileMoniker;
		}

		protected override uint CategoryField(LIB_CATEGORY category)
		{
			switch (category)
			{
				case (LIB_CATEGORY)_LIB_CATEGORY2.LC_MEMBERINHERITANCE:
					if (NodeType == LibraryNodeType.Members)
						return (uint)_LIBCAT_MEMBERINHERITANCE.LCMI_IMMEDIATE;
					break;
			}

			return base.CategoryField(category);
		}

		protected override LibraryNode Clone()
		{
			return new NemerleLibraryNode(this);
		}

		protected override void GotoSource(VSOBJGOTOSRCTYPE gotoType)
		{
			// We do not support the "Goto Reference"
			//
			if (VSOBJGOTOSRCTYPE.GS_REFERENCE == gotoType)
				return;

			// There is no difference between definition and declaration, so here we
			// don't check for the other flags.

			IVsWindowFrame frame;
			IntPtr		 documentData = FindDocDataFromRDT();

			try
			{
				// Now we can try to open the editor. We assume that the owner hierarchy is
				// a project and we want to use its OpenItem method.
				//
				IVsProject3 project = _ownerHierarchy as IVsProject3;

				if (null == project)
					return;

				Guid viewGuid = VSConstants.LOGVIEWID_Code;

				ErrorHandler.ThrowOnFailure(project.OpenItem(_fileId, ref viewGuid, documentData, out frame));
			}
			finally
			{
				if (IntPtr.Zero != documentData)
				{
					Marshal.Release(documentData);
					documentData = IntPtr.Zero;
				}
			}

			// Make sure that the document window is visible.
			//
			ErrorHandler.ThrowOnFailure(frame.Show());

			// Get the code window from the window frame.
			//
			object docView;

			ErrorHandler.ThrowOnFailure(frame.GetProperty((int)__VSFPROPID.VSFPROPID_DocView, out docView));

			IVsCodeWindow codeWindow = docView as IVsCodeWindow;

			if (null == codeWindow)
			{
				object docData;

				ErrorHandler.ThrowOnFailure(frame.GetProperty((int)__VSFPROPID.VSFPROPID_DocData, out docData));

				codeWindow = docData as IVsCodeWindow;

				if (codeWindow == null)
					return;
			}

			// Get the primary view from the code window.
			//
			IVsTextView textView;

			ErrorHandler.ThrowOnFailure(codeWindow.GetPrimaryView(out textView));

			// Set the cursor at the beginning of the declaration.
			//
			ErrorHandler.ThrowOnFailure(textView.SetCaretPos(_sourceSpan.iStartLine, _sourceSpan.iStartIndex));

			// Make sure that the text is visible.
			//
			TextSpan visibleSpan = new TextSpan();

			visibleSpan.iStartLine  = _sourceSpan.iStartLine;
			visibleSpan.iStartIndex = _sourceSpan.iStartIndex;
			visibleSpan.iEndLine	= _sourceSpan.iStartLine;
			visibleSpan.iEndIndex   = _sourceSpan.iStartIndex + 1;

			ErrorHandler.ThrowOnFailure(textView.EnsureSpanVisible(visibleSpan));
		}

		protected override void SourceItems(out IVsHierarchy hierarchy, out uint itemId, out uint itemsCount)
		{
			hierarchy  = _ownerHierarchy;
			itemId	 = _fileId;
			itemsCount = 1;
		}

		public override string UniqueName
		{
			get
			{
				if (string.IsNullOrEmpty(_fileMoniker))
					ErrorHandler.ThrowOnFailure(_ownerHierarchy.GetCanonicalName(_fileId, out _fileMoniker));

				return string.Format(CultureInfo.InvariantCulture, "{0}/{1}", _fileMoniker, Name);
			}
		}

		IntPtr FindDocDataFromRDT()
		{
			// Get a reference to the RDT.
			//
			IVsRunningDocumentTable rdt =
				NemerlePackage.GetGlobalService(typeof (SVsRunningDocumentTable)) as IVsRunningDocumentTable;

			if (rdt == null)
				return IntPtr.Zero;

			// Get the enumeration of the running documents.
			//
			IEnumRunningDocuments documents;

			ErrorHandler.ThrowOnFailure(rdt.GetRunningDocumentsEnum(out documents));

			IntPtr documentData = IntPtr.Zero;
			uint[] docCookie	= new uint[1];
			uint   fetched;

			while ((VSConstants.S_OK == documents.Next(1, docCookie, out fetched)) && (1 == fetched))
			{
				uint		 flags;
				uint		 editLocks;
				uint		 readLocks;
				string	   moniker;
				IVsHierarchy docHierarchy;
				uint		 docId;
				IntPtr	   docData = IntPtr.Zero;

				try
				{
					ErrorHandler.ThrowOnFailure(rdt.GetDocumentInfo(
						docCookie[0],
						out flags,
						out readLocks,
						out editLocks,
						out moniker,
						out docHierarchy,
						out docId,
						out docData));

					// Check if this document is the one we are looking for.
					//
					if ((docId == _fileId) && (_ownerHierarchy.Equals(docHierarchy)))
					{
						documentData = docData;
						docData	  = IntPtr.Zero;
						break;
					}
				}
				finally
				{
					if (docData != IntPtr.Zero)
						Marshal.Release(docData);
				}
			}

			return documentData;
		}
	}
}