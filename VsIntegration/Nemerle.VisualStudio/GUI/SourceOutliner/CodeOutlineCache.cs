/***************************************************************************

A derivative work based on the SourceOutliner Power Toy sample.

Copyright (c) 2006 Microsoft Corporation. All rights reserved.

***************************************************************************/

using EnvDTE;
using EnvDTE80;
using System;
using System.Diagnostics;

namespace Nemerle.VisualStudio.GUI.SourceOutliner
{
	/// <summary>
	/// Class that caches and manages source files for outlining.
	/// </summary>
	[CLSCompliant(false)]
	public class CodeOutlineCache
	{
		private CodeOutlineFileManager _fileManager;
		private SourceOutlinerControl _control;
		private EnvDTE.DTE _dte;
		private CodeModelEvents _codeModelEvents;

		// The cached document.
		private Document _document;

		/// <summary>
		/// Initializes a new instance of the CodeOutlineCache class.
		/// </summary>
		/// <param name="control">The SourceOutlinerControl.</param>
		/// <param name="dte">A DTE object exposing the Visual Studio automation object model.</param>
		public CodeOutlineCache(SourceOutlinerControl control, EnvDTE.DTE dte)
		{
			Debug.Assert(control != null);
			Debug.Assert(dte != null);

			_control = control;
			_dte = dte;

			// Initialize the events.
			AdviseCodeModelEvents();
		}

		private SourceOutlinerToolWindow _toolWindow;

		/// <summary>
		/// Loads a document into display TreeViews, updates the cache, and 
		/// rebuilds the file manager that represents the document.
		/// </summary>
		/// <param name="d">The Document to load.</param>
		/// <param name="tw">The tool window associated with the cache.</param>
		/// <remarks>
		/// If the document is in the cache, it is reused.
		/// </remarks>
		public void AddDocumentToCache(Document d, SourceOutlinerToolWindow tw)
		{
			Debug.Assert(d != null);

			_toolWindow = tw;

			if (d == _document)
			{
				return;
			}

			if (_document != null)
			{
				// Unregister events for the previous document.
				tw.UnRegisterTreeEvents(_fileManager.TreeView, _fileManager.FilterView);
				_control.RemoveTreeFromControls(_fileManager.TreeView);
				_control.RemoveTreeFromControls(_fileManager.FilterView);
			}

			_document = d;
			_fileManager = new CodeOutlineFileManager(_control, _dte, d, tw);

			tw.RegisterTreeEvents(_fileManager.TreeView, _fileManager.FilterView);

			// Load the control.
			_control.AddTreeToControls(_fileManager.TreeView);
			_control.AddTreeToControls(_fileManager.FilterView);
			_fileManager.State = CodeOutlineFileManager.OutlineFileManagerState.StartLoadingCodeModel;
			_fileManager.HideTrees();

			_control.HideTrees();
			_control.TreeView = _fileManager.TreeView;
			_control.FilterView = _fileManager.FilterView;

			// Re-display the last CodeElementType selected for this document.
			tw.SelectedType = _fileManager.ElementFilter;

			// Re-display the last filter text entered for this document, but only if the file is loaded.
			if (_fileManager.State == CodeOutlineFileManager.OutlineFileManagerState.DoneLoadingCodeModel)
			{
				_fileManager.ReApplyText();
				tw.SelectedFilterText = _fileManager.FilterText;
			}
			else
			{
				_control.Reset();
			}
		}

		/// <summary>
		/// Gets the CodeOutlineFileManager object associated with the 
		/// source file currently being displayed and outlined.
		/// </summary>
		/// <returns>
		/// The file manager that represents the document,
		/// or null if no file is being displayed.
		/// </returns>
		public CodeOutlineFileManager CurrentFileManager
		{
			get
			{
				return _fileManager;
			}
		}

		/// <summary>
		/// Registers for code model events.
		/// </summary>
		private void AdviseCodeModelEvents()
		{
			try
			{
				EnvDTE80.DTE2 dte2 = _dte as EnvDTE80.DTE2;
				if (dte2 == null)
					throw new NullReferenceException("dte2 is NULL");

				EnvDTE80.Events2 events2 = dte2.Events as EnvDTE80.Events2;
				if (events2 == null)
					throw new NullReferenceException("events2 is NULL");

				_codeModelEvents = events2.get_CodeModelEvents(null);
				if (_codeModelEvents != null)
				{
					_codeModelEvents.ElementAdded += new _dispCodeModelEvents_ElementAddedEventHandler(codeModelEvents_Added);
					_codeModelEvents.ElementChanged += new _dispCodeModelEvents_ElementChangedEventHandler(codeModelEvents_Changed);
					_codeModelEvents.ElementDeleted += new _dispCodeModelEvents_ElementDeletedEventHandler(codeModelEvents_Deleted);
				}
			}
			catch (System.ArgumentException)
			{    // Failed to get CodeModelEvents, this should not occur.
				Utils.DisplayMessage(Resources.ErrorPrefix, "Failed to get Code Model events.");
				throw;
			}
		}

		/// <summary>
		/// Rebuilds the document cache.
		/// </summary>
		private void ForceReload()
		{
			Document currentDocument = _document;
			_document = null;
			AddDocumentToCache(currentDocument, _toolWindow);
		}

		/// <summary>
		/// Raised when a CodeElement object has been created.
		/// </summary>
		/// <param name="newElement">The CodeElement object that was added.</param>
		private void codeModelEvents_Added(CodeElement newElement)
		{
			try
			{
				if (CurrentFileManager == null)
				{
					return;
				}

				if (!CurrentFileManager.FileIsOutlined)
				{
					ForceReload();
					return;
				}

				_control.ShowWaitWhileReadyMessage();
				_fileManager.OnCodeModelElementAdd(newElement);
				_control.HideWaitWhileReadyMessage();
			}
			catch (Exception ex)
			{
				Utils.DisplayMessage(Resources.ErrorPrefix, "codeModelEvents_Added exception: " + ex.ToString());
			}
		}

		/// <summary>
		/// Raised when a CodeElement object has been deleted.
		/// </summary>
		/// <param name="parent">The parent object for the CodeElement.</param>
		/// <param name="deletedElement">The CodeElement object that was deleted.</param>
		private void codeModelEvents_Deleted(object parent, CodeElement deletedElement)
		{
			try
			{
				if (CurrentFileManager == null)
				{
					return;
				}

				if (!CurrentFileManager.FileIsOutlined)
				{
					ForceReload();
					return;
				}

				_fileManager.OnCodeModelElementDeleted(parent, deletedElement);
			}
			catch (Exception ex)
			{
				Utils.DisplayMessage(Resources.ErrorPrefix, "codeModelEvents_Deleted exception: " + ex.ToString());
			}
		}

		/// <summary>
		/// Raised when a CodeElement object has been changed.
		/// </summary>
		/// <param name="modifiedElement">The CodeElement that was changed.</param>
		/// <param name="iChangeType">The type of change event that was fired.</param>
		private void codeModelEvents_Changed(CodeElement modifiedElement, vsCMChangeKind iChangeType)
		{
			try
			{
				if (CurrentFileManager == null)
				{
					return;
				}

				ForceReload();
			}
			catch (Exception ex)
			{
				Utils.DisplayMessage(Resources.ErrorPrefix, "codeModelEvents_Changed exception: " + ex.ToString());
			}
		}
	}
}
