using System;
using System.Diagnostics;

using Microsoft.VisualStudio.TextManager.Interop;

namespace Nemerle.VisualStudio.Project
{
	[DebuggerStepThrough]
	internal class HierarchyEventArgs : EventArgs
	{
		uint		 _itemId;
		string	   _fileName;
		IVsTextLines _buffer;

		/// <summary>
		/// Initializes a new instance of the <see cref="HierarchyEventArgs"/> class.
		/// </summary>
		/// <param name="itemId">The item id.</param>
		/// <param name="fileName">Name of the file.</param>
		public HierarchyEventArgs(uint itemId, string fileName)
		{
			ErrorHelper.ThrowIsNullOrEmpty(fileName, "fileName");

			_itemId   = itemId;
			_fileName = fileName;
		}

		/// <summary>
		/// Gets the name of the file.
		/// </summary>
		/// <value>The name of the file.</value>
		public string FileName { get { return _fileName; } }

		/// <summary>
		/// Gets the item ID.
		/// </summary>
		/// <value>The item ID.</value>
		public uint ItemID { get { return _itemId; } }

		/// <summary>
		/// Gets or sets the text buffer.
		/// </summary>
		/// <value>The text buffer.</value>
		public IVsTextLines TextBuffer
		{
			get { return _buffer;  }
			set { _buffer = value; }
		}
	}
}
