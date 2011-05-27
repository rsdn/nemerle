using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Compiler;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Inplementation of the service that build the information to expose to the symbols
	/// navigation tools (class view or object browser) from the Nemerle files 
	/// inside a hierarchy.
	/// </summary>
	[Guid(NemerleConstants.LibraryManagerGuidString)]
	public class NemerleLibraryManager : INemerleLibraryManager, IVsRunningDocTableEvents, IDisposable
	{
		/// <summary>
		/// Class storing the data about a parsing task on a nemerle module.
		/// A module in Nemerle is a source file, so here we use the file name to
		/// identify it.
		/// </summary>
		[DebuggerStepThrough] 
		class LibraryTask
		{
			public LibraryTask(string fileName, string text)
			{
				_fileName = fileName;
				_text	 = text;
			}

			private string _fileName;
			public  string  FileName
			{
				get { return _fileName; }
			}

			private ModuleID _moduleID;
			public  ModuleID  ModuleID
			{
				get { return _moduleID;  }
				set { _moduleID = value; }
			}

			private string _text;
			public  string  Text
			{
				get { return _text; }
			}
		}

		IServiceProvider   _provider;
		uint			   _objectManagerCookie;
		uint			   _runningDocTableCookie;
		Library			_library;
		Thread			 _parseThread;
		ManualResetEvent   _requestPresent;
		ManualResetEvent   _shutDownStarted;
		Queue<LibraryTask> _requests;

		Dictionary<uint, TextLineEventListener>	 _documents;
		Dictionary<IVsHierarchy, HierarchyListener> _hierarchies;
		Dictionary<ModuleID, LibraryNode>		   _files;

		public NemerleLibraryManager(IServiceProvider provider)
		{
			_provider		= provider;
			_documents	   = new Dictionary<uint, TextLineEventListener>();
			_hierarchies	 = new Dictionary<IVsHierarchy, HierarchyListener>();
			_files		   = new Dictionary<ModuleID, LibraryNode>();
			_requests		= new Queue<LibraryTask>();
			_requestPresent  = new ManualResetEvent(false);
			_shutDownStarted = new ManualResetEvent(false);
			_parseThread	 = new Thread(ParseThread) {Name = "Parse thread"};

			_library		 = new Library(new Guid(NemerleConstants.LibraryGuidString));
			_library.LibraryCapabilities = (_LIB_FLAGS2)(_LIB_FLAGS.LF_PROJECT) | _LIB_FLAGS2.LF_SUPPORTSFILTERING | _LIB_FLAGS2.LF_SUPPORTSCALLBROWSER;

			_parseThread.Start();
		}

		void RegisterForRDTEvents()
		{
			if (0 != _runningDocTableCookie)
				return;

			IVsRunningDocumentTable rdt =
				_provider.GetService(typeof (SVsRunningDocumentTable)) as IVsRunningDocumentTable;

			if (null != rdt)
				// Do not throw here in case of error, simply skip the registration.
				rdt.AdviseRunningDocTableEvents(this, out _runningDocTableCookie);
		}

		void UnregisterRDTEvents()
		{
			if (0 == _runningDocTableCookie)
				return;

			IVsRunningDocumentTable rdt =
				_provider.GetService(typeof (SVsRunningDocumentTable)) as IVsRunningDocumentTable;

			if (null != rdt)
				// Do not throw in case of error.
				rdt.UnadviseRunningDocTableEvents(_runningDocTableCookie);

			_runningDocTableCookie = 0;
		}

		/// <summary>
		/// Hack. Based on the fact that we have only one fixed root _library. FindAllReferences
		/// search results are stored in the _library just before VS environment ask the _library
		/// for them.
		/// 
		/// Хак. Используем то что у нас одна фиксированная _library, сохраняем в неё 
		/// уже найденные результаты для FindAllReferences непосредственно перед тем как
		/// среда VS попросит у _library эти результаты поиска.
		/// </summary>
		public void OnFindAllReferencesDone(IVsSimpleObjectList2 findResults)
		{
			_library.OnFindAllReferencesDone(findResults);
		}

		#region IDisposable Members

		public void Dispose()
		{
			// Make sure that the parse thread can exit.
			//
			if (null != _shutDownStarted)
				_shutDownStarted.Set();

			if ((null != _parseThread) && _parseThread.IsAlive)
			{
				_parseThread.Join(500);

				if (_parseThread.IsAlive)
					_parseThread.Abort();

				_parseThread = null;
			}

			_requests.Clear();

			// Dispose all the listeners.
			//
			foreach (HierarchyListener listener in _hierarchies.Values)
				listener.Dispose();

			_hierarchies.Clear();

			foreach (TextLineEventListener textListener in _documents.Values)
				textListener.Dispose();

			_documents.Clear();

			// Remove this library from the object manager.
			//
			if (0 != _objectManagerCookie)
			{
				IVsObjectManager2 mgr = _provider.GetService(typeof (SVsObjectManager)) as IVsObjectManager2;

				if (null != mgr)
					mgr.UnregisterLibrary(_objectManagerCookie);

				_objectManagerCookie = 0;
			}

			// Unregister this object from the RDT events.
			//
			UnregisterRDTEvents();

			// Dispose the events used to syncronize the threads.
			//
			if (null != _requestPresent)
			{
				_requestPresent.Close();
				_requestPresent = null;
			}

			if (null != _shutDownStarted)
			{
				_shutDownStarted.Close();
				_shutDownStarted = null;
			}
		}

		#endregion

		#region INemerleLibraryManager

		public void RegisterHierarchy(IVsHierarchy hierarchy)
		{
			if (hierarchy == null || _hierarchies.ContainsKey(hierarchy))
				return;

			if (_objectManagerCookie == 0)
			{
				IVsObjectManager2 objManager =
					_provider.GetService(typeof (SVsObjectManager)) as IVsObjectManager2;

				if (null == objManager)
					return;

				ErrorHandler.ThrowOnFailure(
					objManager.RegisterSimpleLibrary(_library, out _objectManagerCookie));
			}

			HierarchyListener listener = new HierarchyListener(hierarchy);

			listener.ItemAdded   += OnFileChanged;
			listener.ItemDeleted += OnDeleteFile;

			listener.StartListening(true);

			_hierarchies.Add(hierarchy, listener);
			RegisterForRDTEvents();
		}

		public void UnregisterHierarchy(IVsHierarchy hierarchy)
		{
			if ((null == hierarchy) || !_hierarchies.ContainsKey(hierarchy))
				return;

			HierarchyListener listener = _hierarchies[hierarchy];

			if (null != listener)
				listener.Dispose();

			_hierarchies.Remove(hierarchy);

			if (0 == _hierarchies.Count)
				UnregisterRDTEvents();

			lock (_files)
			{
				ModuleID[] keys = new ModuleID[_files.Keys.Count];

				_files.Keys.CopyTo(keys, 0);

				foreach (ModuleID id in keys)
				{
					if (hierarchy.Equals(id.Hierarchy))
					{
						_library.RemoveNode(_files[id]);
						_files.Remove(id);
					}
				}
			}

			// Remove the document listeners.
			//
			uint[] docKeys = new uint[_documents.Keys.Count];

			_documents.Keys.CopyTo(docKeys, 0);

			foreach (uint id in docKeys)
			{
				TextLineEventListener docListener = _documents[id];

				if (hierarchy.Equals(docListener.FileID.Hierarchy))
				{
					_documents.Remove(id);
					docListener.Dispose();
				}
			}
		}

		#endregion

		#region Parse Thread

		/// <summary>
		/// Main function of the parsing thread.
		/// This function waits on the queue of the parsing requests and build 
		/// the parsing tree for a specific file. The resulting tree is built
		/// using LibraryNode objects so that it can be used inside the class
		/// view or object browser.
		/// </summary>
		void ParseThread()
		{
			const int waitTimeout = 500;

			// Define the array of events this function is interest in.
			//
			WaitHandle[] eventsToWait = new WaitHandle[] { _requestPresent, _shutDownStarted };

			// Execute the tasks.
			//
			while (true)
			{
				// Wait for a task or a shutdown request.
				//
				int waitResult = WaitHandle.WaitAny(eventsToWait, waitTimeout, false);

				if (1 == waitResult)
					// The shutdown of this component is started, so exit the thread.
					return;

				LibraryTask task = null;

				lock (_requests)
				{
					if (_requests.Count != 0)
						task = _requests.Dequeue();

					if (_requests.Count == 0)
						_requestPresent.Reset();
				}

				if (null == task)
					continue;

				ScopeNode scope = null;

				if (task.Text == null)
				{
					if (File.Exists(task.FileName))
					{
						Debug.WriteLine("Parse request (no text): " + task.FileName + " " + task.ModuleID);
						return;
					}
				}
				else
				{
					Debug.WriteLine("Parse request: " + task.FileName + " " + task.ModuleID);
					return;
				}

				LibraryNode module = new LibraryNode(
					Path.GetFileName(task.FileName),
					LibraryNode.LibraryNodeType.PhysicalContainer);

				CreateModuleTree(module, module, scope, "", task.ModuleID);

				if (task.ModuleID != null)
				{
					LibraryNode previousItem;

					lock (_files)
						if (_files.TryGetValue(task.ModuleID, out previousItem))
							_files.Remove(task.ModuleID);

					_library.RemoveNode(previousItem);
				}

				_library.AddNode(module);

				if (task.ModuleID != null)
					lock (_files)
						_files.Add(task.ModuleID, module);
			}
		}

		void CreateModuleTree(
			LibraryNode root,
			LibraryNode current,
			ScopeNode   scope,
			string	  namePrefix,
			ModuleID	moduleId)
		{
			if ((null == root) || (null == scope) || (null == scope.NestedScopes))
				return;

			foreach (ScopeNode subItem in scope.NestedScopes)
			{
				NemerleLibraryNode newNode = new NemerleLibraryNode(
					subItem, namePrefix, moduleId.Hierarchy, moduleId.ItemID);

				string newNamePrefix = namePrefix;

				// The classes are always added to the root node, the functions to the current node.
				//
				if ((newNode.NodeType & LibraryNode.LibraryNodeType.Members) != LibraryNode.LibraryNodeType.None)
				{
					current.AddNode(newNode);
				}
				else if ((newNode.NodeType & LibraryNode.LibraryNodeType.Classes) != LibraryNode.LibraryNodeType.None)
				{
					// Classes are always added to the root.
					//
					root.AddNode(newNode);
					newNamePrefix = newNode.Name + ".";
				}

				// Now use recursion to get the other types.
				//
				CreateModuleTree(root, newNode, subItem, newNamePrefix, moduleId);
			}
		}

		#endregion

		void CreateParseRequest(string file, string text, ModuleID id)
		{
			LibraryTask task = new LibraryTask(file, text);

			task.ModuleID = id;

			lock (_requests)
				_requests.Enqueue(task);

			_requestPresent.Set();
		}

		#region Hierarchy Events

		void OnFileChanged(object sender, HierarchyEventArgs args)
		{
			IVsHierarchy hierarchy = sender as IVsHierarchy;

			if (null == hierarchy)
				return;

			string fileText = null;

			if (null != args.TextBuffer)
			{
				int lastLine;
				int lastIndex;
				int hr = args.TextBuffer.GetLastLineIndex(out lastLine, out lastIndex);

				if (ErrorHandler.Failed(hr))
					return;

				hr = args.TextBuffer.GetLineText(0, 0, lastLine, lastIndex, out fileText);

				if (ErrorHandler.Failed(hr))
					return;

				var projectInfo = ProjectInfo.FindProject(hierarchy);
				if (null != projectInfo)
				{
					int fileIndex = Nemerle.Compiler.Location.GetFileIndex(args.FileName);
					projectInfo.Engine.NotifySourceChanged(new StringSource(fileIndex, fileText));
				}
			}

			CreateParseRequest(args.FileName, fileText, new ModuleID(hierarchy, args.ItemID));
		}

		void OnDeleteFile(object sender, HierarchyEventArgs args)
		{
			IVsHierarchy hierarchy = sender as IVsHierarchy;

			if (null == hierarchy)
				return;

			ModuleID	id = new ModuleID(hierarchy, args.ItemID);
			LibraryNode node;

			lock (_files)
				if (_files.TryGetValue(id, out node))
					_files.Remove(id);

			if (null != node)
				_library.RemoveNode(node);
		}

		#endregion

		#region IVsRunningDocTableEvents Members

		public int OnAfterAttributeChange(uint docCookie, uint grfAttribs)
		{
			if ((grfAttribs & (uint)(__VSRDTATTRIB.RDTA_MkDocument)) == (uint)__VSRDTATTRIB.RDTA_MkDocument)
			{
				IVsRunningDocumentTable rdt = 
					_provider.GetService(typeof (SVsRunningDocumentTable)) as IVsRunningDocumentTable;

				if (rdt != null)
				{
					uint		 flags, readLocks, editLocks, itemid;
					IVsHierarchy hier;
					IntPtr	   docData = IntPtr.Zero;
					string	   moniker;

					try
					{
						ErrorHandler.Failed(rdt.GetDocumentInfo(
							docCookie,
							out flags,
							out readLocks,
							out editLocks,
							out moniker,
							out hier,
							out itemid,
							out docData));

						TextLineEventListener listner;

						if (_documents.TryGetValue(docCookie, out listner))
							listner.FileName = moniker;
					}
					finally
					{
						if (IntPtr.Zero != docData)
							Marshal.Release(docData);
					}
				}
			}

			return VSConstants.S_OK;
		}

		public int OnAfterDocumentWindowHide(uint docCookie, IVsWindowFrame pFrame)
		{
			return VSConstants.S_OK;
		}

		public int OnAfterFirstDocumentLock(
			uint docCookie,
			uint dwRDTLockType,
			uint dwReadLocksRemaining,
			uint dwEditLocksRemaining)
		{
			return VSConstants.S_OK;
		}

		public int OnAfterSave(uint docCookie)
		{
			return VSConstants.S_OK;
		}

		public int OnBeforeDocumentWindowShow(uint docCookie, int fFirstShow, IVsWindowFrame pFrame)
		{
			// Check if this document is in the list of the documents.
			//
			if (_documents.ContainsKey(docCookie))
				return VSConstants.S_OK;

			// Get the information about this document from the RDT.
			//
			IVsRunningDocumentTable rdt =
				_provider.GetService(typeof (SVsRunningDocumentTable)) as IVsRunningDocumentTable;

			if (null != rdt)
			{
				// Note that here we don't want to throw in case of error.
				uint		 flags;
				uint		 readLocks;
				uint		 writeLoks;
				string	   documentMoniker;
				IVsHierarchy hierarchy;
				uint		 itemId;
				IntPtr	   unkDocData;

				int hr = rdt.GetDocumentInfo(
					docCookie,
					out flags,
					out readLocks, 
					out writeLoks,
					out documentMoniker,
					out hierarchy,
					out itemId,
					out unkDocData);

				try
				{
					if (ErrorHandler.Failed(hr) || (IntPtr.Zero == unkDocData))
						return VSConstants.S_OK;

					// Check if the herarchy is one of the hierarchies this service is monitoring.
					//
					if (!_hierarchies.ContainsKey(hierarchy))
						// This hierarchy is not monitored, we can exit now.
						return VSConstants.S_OK;

					// Check the extension of the file to see if a listener is required.
					//
					string extension = Path.GetExtension(documentMoniker);

					if (string.Compare(extension, NemerleConstants.FileExtension, StringComparison.OrdinalIgnoreCase) != 0)
						return VSConstants.S_OK;

					// Create the module id for this document.
					//
					ModuleID docId = new ModuleID(hierarchy, itemId);

					// Try to get the text buffer.
					//
					IVsTextLines buffer = Marshal.GetObjectForIUnknown(unkDocData) as IVsTextLines;

					// Create the listener.
					//
					TextLineEventListener listener = new TextLineEventListener(buffer, documentMoniker, docId);

					// Set the event handler for the change event. Note that there is no
					// difference between the AddFile and FileChanged operation, so we 
					// can use the same handler.
					//
					listener.OnFileChanged += OnFileChanged;

					// Add the listener to the dictionary, so we will not create it anymore.
					//
					_documents.Add(docCookie, listener);
				}
				finally
				{
					if (IntPtr.Zero != unkDocData)
						Marshal.Release(unkDocData);
				}
			}

			// Always return success.
			//
			return VSConstants.S_OK;
		}

		public int OnBeforeLastDocumentUnlock(
			uint docCookie,
			uint dwRDTLockType,
			uint dwReadLocksRemaining,
			uint dwEditLocksRemaining)
		{
			if ((0 != dwEditLocksRemaining) || (0 != dwReadLocksRemaining))
				return VSConstants.S_OK;

			TextLineEventListener listener;

			if (!_documents.TryGetValue(docCookie, out listener) || (null == listener))
				return VSConstants.S_OK;

			using (listener)
			{
				_documents.Remove(docCookie);

				// Now make sure that the information about this file are up to date
				// (e.g. it is possible that Class View shows something strange if the
				// file was closed without saving the changes).
				//
				HierarchyEventArgs args = new HierarchyEventArgs(listener.FileID.ItemID, listener.FileName);

				OnFileChanged(listener.FileID.Hierarchy, args);
			}
			return VSConstants.S_OK;
		}

		#endregion

		public void OnIdle()
		{
			foreach (TextLineEventListener listener in _documents.Values)
				listener.OnIdle();
		}
	}
}