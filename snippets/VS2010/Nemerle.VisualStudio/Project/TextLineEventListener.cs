using System;
using System.Diagnostics;

using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project
{
	internal class TextLineEventListener : IVsTextLinesEvents, IDisposable
	{
		//private const int defaultDelay = 2000;

		private string		   _fileName;
		private ModuleID		 _fileId;
		private IVsTextLines	 _buffer;
		private bool			 _isDirty;
		private IConnectionPoint _connectionPoint;
		private uint			 _connectionCookie;

		public TextLineEventListener(IVsTextLines buffer, string fileName, ModuleID id)
		{
			_buffer   = buffer;
			_fileId   = id;
			_fileName = fileName;

			IConnectionPointContainer container = buffer as IConnectionPointContainer;
			
			if (null != container)
			{
				Guid eventsGuid = typeof(IVsTextLinesEvents).GUID;

				container.FindConnectionPoint(ref eventsGuid, out _connectionPoint);
				_connectionPoint.Advise(this, out _connectionCookie);
			}
		}

		#region Properties

		public ModuleID FileID
		{
			get { return _fileId; }
		}

		public string FileName
		{
			get { return _fileName;  }
			set { _fileName = value; }
		}

		#endregion

		#region Events

		private	  EventHandler<HierarchyEventArgs> _onFileChanged;
		public event EventHandler<HierarchyEventArgs>  OnFileChanged
		{
			add	{ _onFileChanged += value; }
			remove { _onFileChanged -= value; }
		}

		#endregion

		#region IVsTextLinesEvents Members

		void IVsTextLinesEvents.OnChangeLineAttributes(int iFirstLine, int iLastLine)
		{
			// Do Nothing
		}

		void IVsTextLinesEvents.OnChangeLineText(TextLineChange[] pTextLineChange, int fLast)
		{
			_isDirty = true;
		}

		#endregion

		#region IDisposable Members

		public void Dispose()
		{
			if (_connectionPoint != null && _connectionCookie != 0)
			{
				_connectionPoint.Unadvise(_connectionCookie);
				Debug.WriteLine("\n\tUnadvised from TextLinesEvents\n");
			}

			_connectionCookie = 0;
			_connectionPoint  = null;

			_buffer = null;
			_fileId = null;
		}

		#endregion

		#region Idle Time Processing

		public void OnIdle()
		{
			if (!_isDirty)
				return;

			if (_onFileChanged != null)
			{
				HierarchyEventArgs args = new HierarchyEventArgs(_fileId.ItemID, _fileName);

				args.TextBuffer = _buffer;

				_onFileChanged(_fileId.Hierarchy, args);
			}

			_isDirty = false;
		}

		#endregion
	}
}
