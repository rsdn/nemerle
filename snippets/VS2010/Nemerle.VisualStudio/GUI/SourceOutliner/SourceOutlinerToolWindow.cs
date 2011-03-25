/***************************************************************************

A derivative work based on the SourceOutliner Power Toy sample.

Copyright (c) 2006 Microsoft Corporation. All rights reserved.

***************************************************************************/

using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using EnvDTE;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.GUI.SourceOutliner
{
	/// <summary>
	/// Class that implements the tool window exposed by this package and hosts a user control.
	/// </summary>
	/// <remarks>
	/// In Visual Studio, tool windows are composed of a frame (implemented by the shell) and a pane, 
	/// usually implemented by the package implementer.
	/// This class derives from the ToolWindowPane class provided from the MPF in order to use its 
	/// implementation of the IVsWindowPane interface.
	/// </remarks>
	[CLSCompliant(false)]
	[Guid("7ABB5A16-1E8A-4259-A797-A55D79DAE596")]
	public class SourceOutlinerToolWindow : ToolWindowPane
	{
		// This is the user control hosted by the tool window. It is exposed to the base class 
		// using the Window property. Note that, even if this class implements IDispose, 
		// Dispose is not called on this object because ToolWindowPane calls Dispose on 
		// the object returned by the Window property.
		private const uint _delayBetweenCodeElementSelection = 500;
		private const uint _delayBetweenIdleProcessing = 500;
		private readonly SourceOutlinerControl _control;
		public CodeOutlineCache _codeCache;
		private bool _codeElementSelectedOnIdle;
		private int _colNum;

		private DTE _dte;
		private EditorSupport _editSupport;
		private Events _events;
		private bool _isSlnClosing;
		private uint _lastTickCount;
		private uint _lastTickCountBeforeUpdate;
		private int _lineNum;
		private SolutionEvents _solnEvents;
		private bool _swallowEnterKey;
		private bool _swallowSelectedIndexChanged_toolStripComboBox;
		private bool _swallowTextChanged_filterStringTextBox;
		private WindowEvents _windowsEvents;
		private bool firstActivation = true;

		// Declared Public for unit testing.

		/// <summary>
		/// Initializes a new instance of the SourceOutlineToolWindow class.
		/// </summary>
		public SourceOutlinerToolWindow() : base(null)
		{
			Caption = Resources.SourceOutlinerWindowTitle;

			// Set the image that will appear on the tab of the window frame
			// when docked with another window.
			// The resource ID correspond to the one defined in the resx file,
			// while the Index is the offset in the bitmap strip. Each image in
			// the strip is 16x16.
			BitmapResourceID = 301;
			BitmapIndex = 1;

			_control = new SourceOutlinerControl();

			// Populate the filter dropdown in the combo box with the 
			// list of possible code elements to be displayed.
			foreach (string name in Enum.GetNames(typeof (CodeElementType)))
			{
				_control.filterToolStripCombo.Items.Add(name);
			}
			_control.filterToolStripCombo.SelectedIndex = 0;

			// Wire up the event handlers for the UI.
			_control.filterToolStripCombo.SelectedIndexChanged += toolStripComboBox_SelectedIndexChanged;
			_control.filterStringTextBox.TextChanged += filterStringTextBox_TextChanged;
			_control.filterStringTextBox.KeyDown += filterStringTextBox_KeyDown;
			_control.filterStringTextBox.KeyPress += filterStringTextBox_KeyPress;
			_control.filterStringTextBox.MouseDown += filterStringTextBox_MouseDown;
			_control.filterStringTextBox.Enter += filterStringTextBox_Enter;
			_control.filterStringTextBox.Leave += filterStringTextBox_Leave;
			_control.clearFilterButton.Click += clearFilterButton_Click;
		}

		/// <summary>
		/// Gets the handle to the user control hosted in the Tool Window.
		/// </summary>
		/// <returns>An IWin32Window object.</returns>
		public override IWin32Window Window
		{
			get { return _control; }
		}

		/// <summary>
		/// Gets or sets the Package property.
		/// </summary>
		/// <returns>A Package object.</returns>
		public new NemerlePackage Package
		{
			get { return (NemerlePackage)base.Package; }
			set { base.Package = value; }
		}

		/// <summary>
		/// Gets or sets the filter text string showing in the textbox.
		/// </summary>
		/// <returns>A filter text string.</returns>
		public string SelectedFilterText
		{
			get { return _control.filterStringTextBox.Text; }
			set
			{
				// Display the text and ignore the TextChanged event.
				if (_control.filterStringTextBox.Text != value)
				{
					_swallowTextChanged_filterStringTextBox = true;
					_control.filterStringTextBox.Text = value;
				}
			}
		}

		/// <summary>
		/// Gets or sets the selected element type in the combo control.
		/// </summary>
		/// <returns>A value from the CodeElementType enumeration.</returns>
		public CodeElementType SelectedType
		{
			get { return (CodeElementType)Enum.Parse(typeof (CodeElementType), _control.filterToolStripCombo.SelectedItem.ToString()); }
			set
			{
				// Select the type in the combo and ignore the SelectedIndexChanged event.
				var currentType = (CodeElementType)Enum.Parse(typeof (CodeElementType),
					_control.filterToolStripCombo.SelectedItem.ToString());
				if (currentType != value)
				{
					string name = Enum.GetName(typeof (CodeElementType), value);
					int index = _control.filterToolStripCombo.FindStringExact(name);
					if (index != -1)
					{
						_swallowSelectedIndexChanged_toolStripComboBox = true;
						_control.filterToolStripCombo.SelectedIndex = index;
					}
				}
			}
		}

		/// <summary>
		/// Gets the current code cache.
		/// </summary>
		/// <returns>A CodeOutlineCache object.</returns>
		public CodeOutlineCache CodeCache
		{
			get { return _codeCache; }
		}

		public DTE Dte
		{
			get { return _dte ?? (_dte = InitializeDTE()); }
		}

		/// <summary> 
		/// Cleans up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing)
		{
			try
			{
				if (disposing)
				{
					var windowFrame = (IVsWindowFrame)Frame;

					if (windowFrame != null)
					{
						windowFrame.CloseFrame((uint)__FRAMECLOSE.FRAMECLOSE_SaveIfDirty);
					}

					if (_control != null)
					{
						_control.Dispose();
					}
				}
			}
			catch
			{
			}
			finally
			{
				base.Dispose(disposing);
			}
		}

		/// <summary>
		/// Sets the Visual Studio IDE object.
		/// </summary>
		private DTE InitializeDTE()
		{
			// Store the dte so that it can be used later.
			var dte = Package.GetService<DTE>();

			_events = dte.Events;

			// Registers handlers for the Activated and Closing events from the text window.
			_windowsEvents = _events.get_WindowEvents(null);
			_windowsEvents.WindowActivated += windowsEvents_WindowActivated;
			_windowsEvents.WindowClosing += windowsEvents_WindowClosing;

			// Registers handlers for certain solution events.
			_solnEvents = _events.SolutionEvents;
			_solnEvents.Opened += solnEvents_Opened;
			_solnEvents.QueryCloseSolution += solnEvents_QueryCloseSolution;

			// Get the code model data.
			_codeCache = new CodeOutlineCache(_control, dte);
			return dte;
		}

		/// <summary>
		/// Occurs when the clearFilterButton button is clicked.
		/// </summary>
		/// <param name="sender">The source Button object for this event.</param>
		/// <param name="e">The EventArgs object that contains the event data.</param>
		private void clearFilterButton_Click(object sender, EventArgs e)
		{
			try
			{
				_control.filterStringTextBox.Text = _control.filterStringTextBox.Focused? String.Empty: "<Filter>";
				_control.filterToolStripCombo.SelectedIndex = 0;

				// Reset the filters.
				if (_codeCache.CurrentFileManager != null)
				{
					_codeCache.CurrentFileManager.ResetFilters();
				}
				_control.clearFilterButton.Enabled = false;
			}
			catch (Exception ex)
			{
				Utils.DisplayMessage(Resources.ErrorPrefix, "clearFilterButton_Click exception: " + ex);
			}
		}

		/// <summary>
		/// Occurs when a key is pressed while the text box has focus. 
		/// </summary>
		/// <param name="sender">The source TextBox object for this event.</param>
		/// <param name="e">The KeyEventArgs object that contains the event data.</param>
		private void filterStringTextBox_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Down)
			{
				if ((_control.VisibleTreeView.Nodes.Count > 0))
				{
					_control.VisibleTreeView.SelectedNode = _control.VisibleTreeView.Nodes[0];
					_control.VisibleTreeView.Focus();
				}
			}

			if (e.KeyCode == Keys.Up)
			{
				if (_control.VisibleTreeView.Nodes.Count > 0)
				{
					_control.VisibleTreeView.SelectedNode = _control.VisibleTreeView.Nodes[_control.VisibleTreeView.Nodes.Count - 1];
					_control.VisibleTreeView.Focus();
				}
			}

			if (e.KeyCode == Keys.Enter)
			{
				_swallowEnterKey = true;
				NavigateToSelectedTreeNode();
			}
		}

		/// <summary>
		/// Occurs when the mouse pointer is over the text box control and a mouse button is pressed. 
		/// </summary>
		/// <param name="sender">The source TextBox object for this event.</param>
		/// <param name="e">The MouseEventArgs object that contains the event data.</param>
		private void filterStringTextBox_MouseDown(object sender, MouseEventArgs e)
		{
			if (_control.filterStringTextBox.Text == "<Filter>")
			{
				_swallowTextChanged_filterStringTextBox = true;
				_control.filterStringTextBox.Text = "";
			}
		}

		/// <summary>
		/// Occurs when a key is pressed while the text box has focus. 
		/// </summary>
		/// <param name="sender">The source TextBox object for this event.</param>
		/// <param name="e">The KeyPressEventArgs object that contains the event data.</param>
		private void filterStringTextBox_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (_swallowEnterKey)
			{
				e.Handled = true;
				_swallowEnterKey = false;
			}
		}

		/// <summary>
		/// Occurs when the text box control is entered.
		/// </summary>
		/// <param name="sender">The source TextBox object for this event.</param>
		/// <param name="e">The EventArgs object that contains the event data.</param>
		private void filterStringTextBox_Enter(object sender, EventArgs e)
		{
			if (_control.filterStringTextBox.Text == "<Filter>")
			{
				_swallowTextChanged_filterStringTextBox = true;
				_control.filterStringTextBox.Text = "";
			}
		}

		/// <summary>
		/// Occurs when the input focus leaves the text box control. 
		/// </summary>
		/// <param name="sender">The source TextBox object for this event.</param>
		/// <param name="e">The EventArgs object that contains the event data.</param>
		private void filterStringTextBox_Leave(object sender, EventArgs e)
		{
			TestFilterStringControlLeave();
		}

		/// <summary>
		/// Occurs when the Text property value changes. 
		/// </summary>
		/// <param name="sender">The source TextBox object for this event.</param>
		/// <param name="e">The EventArgs object that contains the event data.</param>
		private void filterStringTextBox_TextChanged(object sender, EventArgs e)
		{
			try
			{
				if (_swallowTextChanged_filterStringTextBox)
				{
					_swallowTextChanged_filterStringTextBox = false;
				}
				else
				{
					_codeCache.CurrentFileManager.FilterText = _control.filterStringTextBox.Text;
					_codeCache.CurrentFileManager.SelectCodeElement();
				}
				CheckClearFilterButton();
			}
			catch (Exception ex)
			{
				Utils.DisplayMessage(Resources.ErrorPrefix, "filterStringTextBox_TextChanged exception: " + ex);
			}
		}

		/// <summary>
		/// Resets the text box, if needed.
		/// </summary>
		private void TestFilterStringControlLeave()
		{
			if (_control.filterStringTextBox.Text.Length == 0)
			{
				_swallowTextChanged_filterStringTextBox = true;
				_control.filterStringTextBox.Text = "<Filter>";
			}
		}

		/// <summary>
		/// Enables or disables the Clear Filter button.
		/// </summary>
		private void CheckClearFilterButton()
		{
			if (((_control.filterStringTextBox.Text.Length == 0) ||
				(_control.filterStringTextBox.Text == "<Filter>")) &&
				(_control.filterToolStripCombo.SelectedIndex == 0))
			{
				_control.clearFilterButton.Enabled = false;
			}
			else
			{
				_control.clearFilterButton.Enabled = true;
			}
		}

		/// <summary>
		/// Occurs when the SelectedIndex property has changed.  
		/// </summary>
		/// <param name="sender">The source ComboBox object for this event.</param>
		/// <param name="e">The EventArgs object that contains the event data.</param>
		private void toolStripComboBox_SelectedIndexChanged(object sender, EventArgs e)
		{
			try
			{
				var selectedType = (CodeElementType)Enum.Parse(typeof (CodeElementType),
					_control.filterToolStripCombo.SelectedItem.ToString());

				if (_swallowSelectedIndexChanged_toolStripComboBox)
				{
					_swallowSelectedIndexChanged_toolStripComboBox = false;
				}
				else if (_codeCache.CurrentFileManager != null)
				{
					_codeCache.CurrentFileManager.ElementFilter = selectedType;
					_codeCache.CurrentFileManager.SelectCodeElement();
					CheckClearFilterButton();
				}
			}
			catch (Exception ex)
			{
				Utils.DisplayMessage(Resources.ErrorPrefix, "toolStripComboBox_SelectedIndexChanged exception: " + ex);
			}
		}

		/// <summary>
		/// Occurs when the text window is activated.
		/// </summary>
		/// <param name="gotFocus">The window that received the focus.</param>
		/// <param name="lostFocus">The window that lost the focus.</param>
		public void windowsEvents_WindowActivated(Window gotFocus, Window lostFocus)
		{
			try
			{
				if (gotFocus == null)
				{
					Debug.Fail("gotFocus == null");
					Utils.DisplayMessage(Resources.ErrorPrefix, "windowsEvents_WindowActivated has a null gotFocus parameter.");
					return;
				}

				if (lostFocus != null)
				{
					if (_control.filterStringTextBox.Text.Length == 0)
					{
						_swallowTextChanged_filterStringTextBox = true;
						_control.filterStringTextBox.Text = "<Filter>";
					}
				}

				if (_control.filterStringTextBox.Focused)
				{
					if (_control.filterStringTextBox.Text == "<Filter>")
					{
						_swallowTextChanged_filterStringTextBox = true;
						_control.filterStringTextBox.Text = "";
					}
				}

				if (gotFocus.Type.Equals(vsWindowType.vsWindowTypeDocument) && !_isSlnClosing)
				{
					// A document window got focus, so add its document to the cache.
					_codeCache.AddDocumentToCache(gotFocus.Document, this);
				}
				else if (firstActivation && (gotFocus.Object == this)
					&& (lostFocus != null) && (lostFocus.Document != null))
				{
					// Got focus for the first time, this happens when the tool window is first opened.
					// If the window losing focus has a document, add that to the cache. 
					firstActivation = false;
					_codeCache.AddDocumentToCache(lostFocus.Document, this);
				}
			}
			catch (Exception ex)
			{
				Utils.DisplayMessage(Resources.ErrorPrefix, "windowsEvents_WindowActivated exception: " + ex);
			}
		}

		/// <summary>
		/// Occurs just before the text window closes.
		/// </summary>
		/// <param name="window">The window that is closing.</param>
		public void windowsEvents_WindowClosing(Window window)
		{
			if (window == null)
			{
				Debug.Fail("window == null");
				Utils.DisplayMessage(Resources.ErrorPrefix, "windowsEvents_WindowClosing has a null window parameter.");
				return;
			}

			if ((_codeCache != null) && (_codeCache.CurrentFileManager != null) &&
				(_codeCache.CurrentFileManager.Document == window.Document))
			{
				_codeCache.CurrentFileManager.State
					= CodeOutlineFileManager.OutlineFileManagerState.WaitToStartOver;
				_control.HideWaitWhileReadyMessage();
				_control.Enabled = false;
				_control.TreeView.Visible = false;
				_control.FilterView.Visible = false;
			}
		}

		/// <summary>
		/// Occurs immediately after opening a solution or project.
		/// </summary>
		private void solnEvents_Opened()
		{
			_isSlnClosing = false;
		}

		/// <summary>
		/// Occurs when a solution is closing.
		/// </summary>
		/// <param name="fCancel">true to cancel the close, otherwise false.</param>
		private void solnEvents_QueryCloseSolution(ref bool fCancel)
		{
			_isSlnClosing = true;
			_control.HideTrees();
			_swallowSelectedIndexChanged_toolStripComboBox = true;
			_control.filterToolStripCombo.SelectedIndex = 0;
			_swallowTextChanged_filterStringTextBox = true;
			_control.filterStringTextBox.Text = "<Filter>";
			_control.Enabled = false;
		}

		/// <summary>
		/// Occurs after the tree node is selected.
		/// </summary>
		/// <param name="sender">The source TreeView object for this event.</param>
		/// <param name="e">The TreeViewEventArgs object that contains the event data.</param>
		private void codeTreeView_AfterSelect(object sender, TreeViewEventArgs e)
		{
			try
			{
				if ((e.Action == TreeViewAction.ByKeyboard) || (e.Action == TreeViewAction.ByMouse))
				{
					var codeElement = e.Node as CodeElementWrapper;
					_editSupport = new EditorSupport();
					_editSupport.GoToCodeElement(codeElement, Dte);
				}
			}
			catch (Exception ex)
			{
				Utils.DisplayMessage(Resources.ErrorPrefix, "codeTreeView_AfterSelect exception: " + ex);
			}
		}

		/// <summary>
		/// Occurs when the tree view control is entered.  
		/// </summary>
		/// <param name="sender">The source TreeView object for this event.</param>
		/// <param name="e">The EventArgs object that contains the event data.</param>
		private void codeTreeView_Enter(object sender, EventArgs e)
		{
			try
			{
				// Make sure the selected element in the outline window is also selected in the text window.
				var codeElement = _control.VisibleTreeView.SelectedNode as CodeElementWrapper;
				_editSupport = new EditorSupport();
				_editSupport.GoToCodeElement(codeElement, Dte);
			}
			catch (Exception ex)
			{
				Utils.DisplayMessage(Resources.ErrorPrefix, "codeTreeView_Enter exception: " + ex);
			}
		}

		/// <summary>
		/// Occurs when a key is pressed while the tree view has focus. 
		/// </summary>
		/// <param name="sender">The source TreeView object for this event.</param>
		/// <param name="e">The KeyEventArgs object that contains the event data.</param>
		private void codeTreeView_KeyDown(object sender, KeyEventArgs e)
		{
			try
			{
				if (e.KeyCode == Keys.Enter)
				{
					_swallowEnterKey = true;
					NavigateToSelectedTreeNode();
				}
			}
			catch (Exception ex)
			{
				Utils.DisplayMessage(Resources.ErrorPrefix, "codeTreeView_KeyDown exception: " + ex);
			}
		}

		/// <summary>
		/// Occurs when a key is pressed while the tree view has focus. 
		/// </summary>
		/// <param name="sender">The source TreeView object for this event.</param>
		/// <param name="e">The KeyPressEventArgs object that contains the event data.</param>
		private void codeTreeView_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (_swallowEnterKey)
			{
				e.Handled = true;
				_swallowEnterKey = false;
			}
		}

		/// <summary>
		/// Occurs when the tree view control is double-clicked.
		/// </summary>
		/// <param name="sender">The source TreeView object for this event.</param>
		/// <param name="e">The EventArgs object that contains the event data.</param>
		private void codeTreeView_DoubleClick(object sender, EventArgs e)
		{
			try
			{
				var codeElement = _control.VisibleTreeView.SelectedNode as CodeElementWrapper;

				// This can happen if there were no matching code elements.
				if (codeElement == null)
				{
					return;
				}

				_editSupport = new EditorSupport();
				_editSupport.ActivateCodeWindow(codeElement, Dte);
			}
			catch (Exception ex)
			{
				Utils.DisplayMessage(Resources.ErrorPrefix, "codeTreeView_DoubleClick exception: " + ex);
			}
		}

		/// <summary>
		/// Registers TreeView events.
		/// </summary>
		/// <param name="tree">The hierarchical tree.</param>
		/// <param name="filter">The filtered tree.</param>
		public void RegisterTreeEvents(TreeView tree, TreeView filter)
		{
			if (tree == null)
			{
				Debug.Fail("tree == null");
				Utils.DisplayMessage(Resources.ErrorPrefix, "RegisterTreeEvents has a null tree parameter.");
				return;
			}

			if (filter == null)
			{
				Debug.Fail("filter == null");
				Utils.DisplayMessage(Resources.ErrorPrefix, "RegisterTreeEvents has a null filter parameter.");
				return;
			}

			tree.AfterSelect += codeTreeView_AfterSelect;
			tree.KeyDown += codeTreeView_KeyDown;
			tree.KeyPress += codeTreeView_KeyPress;
			tree.DoubleClick += codeTreeView_DoubleClick;
			tree.Enter += codeTreeView_Enter;
			filter.AfterSelect += codeTreeView_AfterSelect;
			filter.KeyDown += codeTreeView_KeyDown;
			filter.KeyPress += codeTreeView_KeyPress;
			filter.DoubleClick += codeTreeView_DoubleClick;
			filter.Enter += codeTreeView_Enter;
		}

		/// <summary>
		/// Unregisters TreeView events.
		/// </summary>
		/// <param name="tree">The hierarchical tree.</param>
		/// <param name="filter">The filtered tree.</param>
		public void UnRegisterTreeEvents(TreeView tree, TreeView filter)
		{
			if (tree == null)
			{
				Debug.Fail("tree == null");
				Utils.DisplayMessage(Resources.ErrorPrefix, "UnRegisterTreeEvents has a null tree parameter.");
				return;
			}

			if (filter == null)
			{
				Debug.Fail("filter == null");
				Utils.DisplayMessage(Resources.ErrorPrefix, "UnRegisterTreeEvents has a null filter parameter.");
				return;
			}

			tree.AfterSelect -= codeTreeView_AfterSelect;
			tree.KeyDown -= codeTreeView_KeyDown;
			tree.KeyPress -= codeTreeView_KeyPress;
			tree.DoubleClick -= codeTreeView_DoubleClick;
			filter.AfterSelect -= codeTreeView_AfterSelect;
			filter.KeyDown -= codeTreeView_KeyDown;
			filter.KeyPress -= codeTreeView_KeyPress;
			filter.DoubleClick -= codeTreeView_DoubleClick;
		}

		/// <summary>
		/// Navigates from the selected node in the tree to its code 
		/// element in the editor window, and gives the editor focus.
		/// </summary>
		private void NavigateToSelectedTreeNode()
		{
			var codeElement = _control.VisibleTreeView.SelectedNode as CodeElementWrapper;

			// This can happen if there were no matching code elements.
			if (codeElement == null)
			{
				return;
			}

			// Switch to the code window.
			_editSupport = new EditorSupport();
			_editSupport.ActivateCodeWindow(codeElement, Dte);
		}

		// Any component that needs idle time, the ability to process
		// messages before they are translated (for example, to call TranslateAccelerator 
		// or IsDialogMessage), notification about modal states, or the ability to push message 
		// loops must implement this interface and register with the Component Manager.

		/// <summary>
		/// Called during each iteration of a message loop that the component pushed.
		/// </summary>
		/// <param name="uReason">The reason for the call.</param>
		/// <param name="pvLoopData">The component's private data.</param>
		/// <param name="pMsgPeeked">The peeked message, or NULL if no message is in the queue.</param>
		/// <returns>
		/// TRUE (not zero) if the message loop should continue, otherwise FALSE (zero).
		/// If false is returned, the component manager terminates the loop without
		/// removing pMsgPeeked from the queue.
		/// </returns>
		/// <remarks>
		/// This method is called after peeking the next message in the queue (via PeekMessage)
		/// but before the message is removed from the queue.  This method may be additionally 
		/// called when the next message has already been removed from the queue, in which case
		/// pMsgPeeked is passed as NULL.
		/// </remarks>
		public int FContinueMessageLoop(uint uReason, IntPtr pvLoopData, MSG[] pMsgPeeked)
		{
			throw new Exception("The method or operation is not implemented.");
		}

		/// <summary>
		/// Called when the Visual Studio IDE goes idle to give 
		/// the component a chance to perform idle time tasks.  
		/// </summary>
		/// <remarks>
		/// The component may periodically call FContinueIdle and, if it returns
		/// false, the component should terminate its idle time processing and return.  
		/// If a component reaches a point where it has no idle tasks and does not need
		/// FDoIdle calls, it should remove its idle task registration via 
		/// FUpdateComponentRegistration.  If this method is called while the component
		/// is performing a tracking operation, the component should only perform idle time 
		/// tasks that it deems appropriate to perform during tracking.
		/// </remarks>
		public void OnIdle()
		{
			if (Dte == null || _codeCache == null)
			{
				// Initialize is in progress.
				//
				return;
			}

			var tickCount = (uint)Environment.TickCount;
			if (tickCount < _lastTickCount)
			{
				// The tick count rolled over, so treat this as if the timeout has expired 
				// to keep from waiting until the count gets up to the required value again.
			}
			else
			{
				// Check to see when the last occurrence was.  Only search once per second.
				if ((tickCount - _lastTickCount) < _delayBetweenIdleProcessing)
				{
					return;
				}
			}

			try
			{
				if (_codeCache.CurrentFileManager != null)
				{
					CodeOutlineFileManager.OutlineFileManagerState
						state = _codeCache.CurrentFileManager.State;
					switch (state)
					{
						case CodeOutlineFileManager.OutlineFileManagerState.Failed:
							_control.ShowException(_codeCache.CurrentFileManager.ParseException);
							_control.Enabled = true;
							return;

						case CodeOutlineFileManager.OutlineFileManagerState.StartLoadingCodeModel:
							// Load completely anew.
							_control.ShowWaitWhileReadyMessage();
							_codeCache.CurrentFileManager.Load();
							return;

						case CodeOutlineFileManager.OutlineFileManagerState.LoadingCodeModel:
							// Continue loading after an interruption.
							_codeCache.CurrentFileManager.ContinueLoading();
							return;

						case CodeOutlineFileManager.OutlineFileManagerState.DoneLoadingCodeModel:
							// Loading is complete.
							_codeCache.CurrentFileManager.FinishLoading();
							_codeCache.CurrentFileManager.TreeView.Refresh();
							_codeCache.CurrentFileManager.FilterView.Refresh();
							_control.Enabled = _codeCache.CurrentFileManager.FileIsOutlined;
							if (_control.Enabled)
							{
								var selectedType = (CodeElementType)Enum.Parse(typeof (CodeElementType),
									_control.filterToolStripCombo.SelectedItem.ToString());
								_codeCache.CurrentFileManager.ElementFilter = selectedType;
							}

							_control.HideWaitWhileReadyMessage();
							_control.Reset();

							_codeCache.CurrentFileManager.State = CodeOutlineFileManager.OutlineFileManagerState.WaitToStartOver;
							return;

						case CodeOutlineFileManager.OutlineFileManagerState.WaitToStartOver:
							break;
					}
				}

				// Get the current active TextPoint from the DTE.
				if (!_control.Enabled || Dte.ActiveDocument == null ||
					_codeCache.CurrentFileManager == null ||
					_codeCache.CurrentFileManager.TreeViewFocused)
				{
					return;
				}

				var sel = (TextSelection)Dte.ActiveDocument.Selection;
				if (sel == null)
				{
					return;
				}

				TextPoint tp = sel.ActivePoint;

				if ((tp.Line == _lineNum) && (tp.LineCharOffset == _colNum))
				{
					if (!_codeElementSelectedOnIdle &&
						((tickCount - _lastTickCountBeforeUpdate) > _delayBetweenCodeElementSelection))
					{
						_codeElementSelectedOnIdle = true;

						// Turn off pretty listing to fix the problem with line autocompletion  
						// being invoked when the code element position is determined.
						EnvDTE.Properties properties = Dte.get_Properties("TextEditor", "Basic-Specific");
						Property property = null;
						foreach (Property p in properties)
						{
							if (p.Name == "PrettyListing")
							{
								property = p;
								break;
							}
						}
						bool currentPrettyListing = true;
						if (property != null)
						{
							currentPrettyListing = (bool)property.Value;
							property.Value = false;
						}

						_codeCache.CurrentFileManager.SelectCodeElement(tp);

						// Set pretty listing back to its previous value.
						if (property != null)
						{
							property.Value = currentPrettyListing;
						}

						_lastTickCountBeforeUpdate = tickCount;
					}
				}
				else
				{
					_codeElementSelectedOnIdle = false;
				}

				_lineNum = tp.Line;
				_colNum = tp.LineCharOffset;
			}
			catch (Exception ex)
			{
				//exceptions from time to time occur in Nemerle parser
				if (_codeCache.CurrentFileManager != null)
					_codeCache.CurrentFileManager.OnException(ex);

				//Utils.DisplayMessage(Resources.ErrorPrefix, "FDoIdle exception: " + ex.ToString());
			}
			_lastTickCount = tickCount;
		}

		/// <summary>
		/// Gives the component a chance to process the message before it is translated and dispatched.
		/// </summary>
		/// <param name="pMsg">The message to process.</param>
		/// <returns>TRUE (not zero) if the message is consumed, otherwise FALSE (zero).</returns>
		/// <remarks>
		/// The component can do TranslateAccelerator, do IsDialogMessage, modify pMsg, or take some other action.
		/// </remarks>
		public int FPreTranslateMessage(MSG[] pMsg)
		{
			return 0;
		}

		/// <summary>
		/// Called when component manager wishes to know if the 
		/// component is in a state where it can terminate.
		/// </summary>
		/// <param name="fPromptUser">
		/// TRUE (not zero) to prompt the user for permission to terminate, otherwise FALSE (zero).
		/// </param>
		/// <returns>TRUE (not zero) if okay to terminate, otherwise FALSE (zero).</returns>
		/// <remarks>
		/// If fPromptUser is false, the component should simply return true if 
		/// it can terminate or false otherwise.
		/// If fPromptUser is true, the component should return true if it can
		/// terminate without prompting the user; otherwise, it should prompt the
		/// user by either (a) asking the user if it can terminate and returning true
		/// or false appropriately, or (b) giving an indication as to why it cannot 
		/// terminate and returning false.
		/// </remarks>
		public int FQueryTerminate(int fPromptUser)
		{
			return 1;
		}

		/// <summary>
		/// A reserved method; must return TRUE (not zero).
		/// </summary>
		public int FReserved1(uint dwReserved, uint message, IntPtr wParam, IntPtr lParam)
		{
			return 1;
		}

		/// <summary>
		/// Retrieves a window associated with the component.
		/// </summary>
		/// <param name="dwReserved">Reserved for future use and should be zero.</param>
		/// <param name="dwWhich">An value from the olecWindow enumeration.</param>
		/// <returns>The desired window or NULL if no such window exists.</returns>
		public IntPtr HwndGetWindow(uint dwWhich, uint dwReserved)
		{
			return Window.Handle;
		}

		/// <summary>
		/// Notifies the component when a new object is being activated.
		/// </summary>
		/// <param name="dwReserved">Reserved for future use.</param>
		/// <param name="fHostIsActivating">TRUE (not zero) if the host is the object being activated, otherwise FALSE (zero).</param>
		/// <param name="fSameComponent">TRUE (not zero) if pic is the same component as the callee of this method, otherwise FALSE (zero).</param>
		/// <param name="pchostinfo">An OLECHOSTINFO that contains information about the host.</param>
		/// <param name="pcrinfo">An OLECRINFO that contains information about pic.</param>
		/// <param name="pic">The IOleComponent object to activate.</param>
		/// <remarks>
		/// If pic is non-NULL, then it is the component that is being activated.
		/// In this case, fSameComponent is true if pic is the same component as
		/// the callee of this method, and pcrinfo is the information about the pic.
		/// If pic is NULL and fHostIsActivating is true, then the host is the
		/// object being activated, and pchostinfo is its host info.
		/// If pic is NULL and fHostIsActivating is false, then there is no current
		/// active object.
		/// If pic is being activated and pcrinfo->grf has the olecrfExclusiveBorderSpace 
		/// bit set, the component should hide its border space tools (toolbars, 
		/// status bars, etc.), and it should also do this if the host is activating and 
		/// pchostinfo->grfchostf has the olechostfExclusiveBorderSpace bit set.
		/// In either of these cases, the component should unhide its border space
		/// tools the next time it is activated.
		/// If pic is being activated and pcrinfo->grf has the olecrfExclusiveActivation
		/// bit is set, then pic is being activated in 'ExclusiveActive' mode.  The
		/// component should retrieve the top frame window that is hosting pic
		/// (via pic->HwndGetWindow(olecWindowFrameToplevel, 0)).  
		/// If this window is different from the component's own top frame window, 
		/// the component should disable its windows and do the things it would do
		/// when receiving an OnEnterState(olecstateModal, true) notification. 
		/// Otherwise, if the component is top-level, it should refuse to have its window 
		/// activated by appropriately processing WM_MOUSEACTIVATE.
		/// The component should remain in one of these states until the 
		/// ExclusiveActive mode ends, indicated by a future call to OnActivationChange 
		/// with the ExclusiveActivation bit not set or with a NULL pcrinfo.
		/// </remarks>
		public void OnActivationChange(
			IOleComponent pic,
			int fSameComponent,
			OLECRINFO[] pcrinfo,
			int fHostIsActivating,
			OLECHOSTINFO[] pchostinfo,
			uint dwReserved)
		{
		}

		/// <summary>
		/// Notifies the component when the host application gains or loses activation.
		/// </summary>
		/// <param name="dwOtherThreadID">The ID of the thread that owns the window.</param>
		/// <param name="fActive">TRUE (not zero) if the host application is being activated, otherwise FALSE (zero).</param>
		/// <remarks>
		/// If fActive is TRUE, the host application is being activated and
		/// dwOtherThreadID is the ID of the thread owning the window being deactivated.
		/// If fActive is false, the host application is being deactivated and 
		/// dwOtherThreadID is the ID of the thread owning the window being activated.
		/// This method is not called when both the window being activated
		/// and the window being deactivated belong to the host application.
		/// </remarks>
		public void OnAppActivate(int fActive, uint dwOtherThreadID)
		{
		}

		/// <summary>
		/// Notifies the component when the application enters or exits (as indicated by fEnter).
		/// </summary>
		/// <param name="fEnter">TRUE (not zero) for enter and FALSE (zero) for exit.</param>
		/// <param name="uStateID">The state identifier from the olecstate enumeration.</param>
		/// <remarks>
		/// If n calls are made with a true fEnter, the component should consider 
		/// the state to be in effect until n calls are made with a false fEnter.
		/// The component should be aware that it is possible for this method to
		/// be called with a false fEnter more times than it was called with a true
		/// fEnter.  For example, if the component is maintaining a state counter
		/// incremented when this method is called with a true fEnter and decremented
		/// when called with a false fEnter, then the counter should not be decremented
		/// for a false fEnter if it is already at zero.
		/// </remarks>
		public void OnEnterState(uint uStateID, int fEnter)
		{
		}

		/// <summary>
		/// Notifies the active component that it has lost its active status
		/// because the host or another component has become active.
		/// </summary>
		public void OnLoseActivation()
		{
		}

		/// <summary>
		/// Called when the component manager wishes to terminate the component's registration.
		/// </summary>
		/// <remarks>
		/// The component should revoke its registration with the component manager,
		/// release references to component manager and perform any necessary cleanup.
		/// </remarks>
		public void Terminate()
		{
		}
	}
}
