using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Compiler.Utils;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Auxiliary class to merge file changes to files in RDT
	/// </summary>
	class RDTFileTextMerger : IFileTextMerger, IDisposable
	{
		#region FileChange variant

		abstract class FileChange
		{
			public abstract void ProcessChanges(IVsTextLines vsTextLines, TextSpan[] span);
		}

		class AddLinesChange : FileChange
		{
			public readonly int Start;
			public readonly List<string> NewLines;

			public AddLinesChange(int start, IList<string> newLines)
			{
				Start = start;
				NewLines = new List<string>(newLines);
			}

			public override void ProcessChanges(IVsTextLines vsTextLines, TextSpan[] span)
			{
				string text = Utils.ConvertToText(this.NewLines);

				Debug.Print("AddLinesChange : add to line " + this.Start +
					" lines (" + this.NewLines.Count + ") :\n"
					+ text);

				/*int endLine = this.Start + this.NewLines.Count;
				int endLength;
				ErrorHandler.ThrowOnFailure(vsTextLines.GetLengthOfLine(endLine, out endLength));
				*/
				GCHandle handle = GCHandle.Alloc(text, GCHandleType.Pinned);
				try
				{
					ErrorHandler.ThrowOnFailure(
						vsTextLines.ReplaceLines(this.Start - 1, 0,
												 this.Start - 1, 0,
												 handle.AddrOfPinnedObject(), text.Length, span));
				}
				finally
				{
					handle.Free();
				}
			}
		}

		class ReplaceLinesChange : FileChange
		{
			public readonly int Start;
			public readonly int End;
			public readonly List<string> NewLines;

			public ReplaceLinesChange(int start, int end, IList<string> newLines)
			{
				Start = start;
				End = end;
				NewLines = new List<string>(newLines);
			}

			public override void ProcessChanges(IVsTextLines vsTextLines, TextSpan[] span)
			{
				string text = Utils.ConvertToText(this.NewLines);

				Debug.Print("ReplaceLinesChange : replace lines (" + this.Start +
							"," + this.End + ") with lines (" + this.NewLines.Count + ") :\n"
							+ text);

				//int endLength;
				//ErrorHandler.ThrowOnFailure(vsTextLines.GetLengthOfLine(this.End-1, out endLength));

				GCHandle handle = GCHandle.Alloc(text, GCHandleType.Pinned);
				try
				{
					ErrorHandler.ThrowOnFailure(
						vsTextLines.ReplaceLines(this.Start - 1, 0,
												 this.End, 0, // endLength,
												 handle.AddrOfPinnedObject(), text.Length, span));
				}
				finally
				{
					handle.Free();
				}
			}
		}

		class RemoveLinesChange : FileChange
		{
			public readonly int Start;
			public readonly int End;

			public RemoveLinesChange(int start, int end)
			{
				Start = start;
				End = end;
			}

			public override void ProcessChanges(IVsTextLines vsTextLines, TextSpan[] span)
			{
				string text = "";

				Debug.Print("RemoveLinesChange : replace lines (" + this.Start +
								"," + this.End + ") with\n" + text);

				//int endLength;
				//ErrorHandler.ThrowOnFailure(vsTextLines.GetLengthOfLine(this.End-1, out endLength));

				GCHandle handle = GCHandle.Alloc(text, GCHandleType.Pinned);
				try
				{
					ErrorHandler.ThrowOnFailure(
						vsTextLines.ReplaceLines(this.Start - 1, 0,
												 this.End, 0, //endLength,
												 handle.AddrOfPinnedObject(), text.Length, span));
				}
				finally
				{
					handle.Free();
				}
			}
		}

		#endregion

		#region fields

		readonly FileNode _fileNode;

		List<FileChange> _fileChanges;

		#endregion

		public RDTFileTextMerger(FileNode fileNode)
		{
			_fileNode = fileNode;
			_fileChanges = new List<FileChange>();
		}

		public void Dispose()
		{
			Flush();
		}

		public void AddLines(int start, IList<string> newLines)
		{
			_fileChanges.Add(new AddLinesChange(start, newLines));
		}

		public void ReplaceLines(int start, int end, IList<string> newLines)
		{
			_fileChanges.Add(new ReplaceLinesChange(start, end, newLines));
		}

		public void RemoveLines(int start, int end)
		{
			_fileChanges.Add(new RemoveLinesChange(start, end));
		}

		private delegate void ProcessFileChangesFunc(IVsTextLines vsTextLines);

		private void ProcessFileChanges(IVsTextLines vsTextLines)
		{
			TextSpan[] span = new TextSpan[1];

			foreach (FileChange fileChange in _fileChanges)
				fileChange.ProcessChanges(vsTextLines, span);

			_fileChanges.Clear();
		}

		public void Flush()
		{
			ProcessFileChangesHelper(ProcessFileChanges);
			// TODO - how to handle a case when file is not in RDT?
		}

		private void ProcessFileChangesHelper(ProcessFileChangesFunc processFunc)
		{
			string filePath = _fileNode.GetMkDocument();

			IVsRunningDocumentTable rdt = _fileNode.ProjectMgr.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;

			// (kberes) Shouldn't this be an InvalidOperationException instead with some not to annoying errormessage to the user?
			if (rdt == null)
				ErrorHandler.ThrowOnFailure(VSConstants.E_FAIL); // Fixme: 

			IVsHierarchy hier;
			uint itemId, cookie;
			IntPtr docData = IntPtr.Zero;

			// Find the document in rdt.
			// Getting a edit lock on the document. Must be released later.
			ErrorHandler.ThrowOnFailure(rdt.FindAndLockDocument((uint)_VSRDTFLAGS.RDT_EditLock,
				filePath, out hier, out itemId, out docData, out cookie));

			if (docData == IntPtr.Zero) // if file not open (usually it's a *.Designer.n)...
			{ // ... open it.
				FileDocumentManager manager = (FileDocumentManager)_fileNode.GetDocumentManager();
				IVsWindowFrame frame;

				// Open the document in rdt.
				ErrorHandler.ThrowOnFailure(manager.Open(false, false, VSConstants.LOGVIEWID_Code,
					out frame, WindowFrameShowAction.DoNotShow));
				// Find the document in rdt.
				ErrorHandler.ThrowOnFailure(rdt.FindAndLockDocument((uint)_VSRDTFLAGS.RDT_EditLock,
					filePath, out hier, out itemId, out docData, out cookie));
			}

			if (docData != IntPtr.Zero)
			{
				object obj = Marshal.GetObjectForIUnknown(docData);
				IVsPersistDocData persistDocData = obj as IVsPersistDocData;
				Marshal.Release(docData);

				try
				{
					// Try to get the Text lines
					IVsTextLines srpTextLines = persistDocData as IVsTextLines;
					if (srpTextLines == null)
					{
						// Try getting a text buffer provider first
						IVsTextBufferProvider srpTextBufferProvider = persistDocData as IVsTextBufferProvider;
						if (srpTextBufferProvider != null)
							ErrorHandler.ThrowOnFailure(srpTextBufferProvider.GetTextBuffer(out srpTextLines));
						// TODO : handle null case
					}

					//int endLine, endIndex;
					//srpTextLines.GetLastLineIndex(out endLine, out endIndex);

					// Lock the buffer before changing its content.
					ErrorHandler.ThrowOnFailure(srpTextLines.LockBuffer());
					try
					{
						processFunc(srpTextLines);
					}
					finally
					{
						// Make sure that the buffer is unlocked also in case of exception.
						srpTextLines.UnlockBuffer();
					}
				}
				finally
				{
					ErrorHandler.ThrowOnFailure(rdt.UnlockDocument((uint)(_VSRDTFLAGS.RDT_Unlock_NoSave), cookie)); //_VSRDTFLAGS.RDT_RequestUnlock | 
				}
			}
			else
			{
				//Debug.Print("Document hasn't been found in RDT");
				throw new ApplicationException("Document hasn't been found in RDT");
			}
		}
	}
}
