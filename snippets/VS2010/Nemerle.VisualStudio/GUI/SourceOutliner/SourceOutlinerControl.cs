/***************************************************************************

A derivative work based on the SourceOutliner Power Toy sample.

Copyright (c) 2006 Microsoft Corporation. All rights reserved.

***************************************************************************/

using System;
using System.Drawing;
using System.Windows.Forms;

namespace Nemerle.VisualStudio.GUI.SourceOutliner
{
	/// <summary>
	/// Control that displays a source outline.
	/// </summary>
	public partial class SourceOutlinerControl : UserControl
	{
		private ImageList _treeViewImages = new ImageList();
		private Boolean _treeViewWasVisible = false;

		/// <summary>
		/// Initializes a new instance of the SourceOutlinerControl class.
		/// </summary>
		public SourceOutlinerControl()
		{
			InitializeComponent();

			// Create the image list for the icons and load it up
			// with the icon strip in the resources.
			_treeViewImages.ImageSize = new Size(16, 16);
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SourceOutlinerControl));
			System.Drawing.Image img = ((System.Drawing.Image)(resources.GetObject("TreeViewIcons")));
			if (img != null)
			{
				_treeViewImages.Images.AddStrip(img);
			}
			_treeViewImages.TransparentColor = Color.Magenta;

			System.Windows.Forms.ProfessionalColorTable colorTable = new System.Windows.Forms.ProfessionalColorTable();
			colorTable.UseSystemColors = true;
			this.filterToolStrip.Renderer = new System.Windows.Forms.ToolStripProfessionalRenderer(colorTable);

		}

		/// <summary>
		/// Gets the TreeView that is currently being displayed.
		/// </summary>
		/// <returns>A TreeView object.</returns>
		public TreeView VisibleTreeView
		{
			get
			{
				if (codeTreeView.Visible)
				{
					return (codeTreeView);
				}
				else
				{
					return (codeFilterView);
				}
			}
		}

		/// <summary>
		/// Gets or sets the TreeView object for the unfiltered (hierarchical) view.
		/// </summary>
		/// <returns>A TreeView object.</returns>
		public TreeView TreeView
		{
			get
			{
				return (codeTreeView);
			}
			set
			{
				codeTreeView = value;
			}
		}

		/// <summary>
		/// Gets or sets the TreeView object for the filtered view.
		/// </summary>
		/// <returns>A TreeView object.</returns>
		public TreeView FilterView
		{
			get
			{
				return (codeFilterView);
			}
			set
			{
				codeFilterView = value;
			}
		}

		/// <summary>
		/// Makes both the filtered and unfiltered TreeViews invisible.
		/// </summary>
		public void HideTrees()
		{
			codeTreeView.Visible = false;
			codeFilterView.Visible = false;
		}

		/// <summary>
		/// Adds a TreeView object to the list of controls.
		/// </summary>
		/// <param name="tv">The TreeView object to be added.</param>
		public void AddTreeToControls(TreeView tv)
		{
			RemoveTreeFromControls(tv);
			tv.Dock = System.Windows.Forms.DockStyle.Fill;
			tv.Location = new System.Drawing.Point(0, 45);
			tv.Size = new System.Drawing.Size(352, 294);
			tv.ImageList = this._treeViewImages;
			this.Controls.Add(tv);
			this.Controls.SetChildIndex(tv, 0);
		}

		/// <summary>
		/// Deletes a TreeView object from the list of controls.
		/// </summary>
		/// <param name="tv">The TreeView object to be deleted.</param>
		public void RemoveTreeFromControls(TreeView tv)
		{
			this.Controls.Remove(tv);
		}

		/// <summary>
		/// Makes the unfiltered TreeView visible, and makes the filtered TreeView
		/// and the 'Please Wait' message invisible.
		/// </summary>
		public void ShowTree()
		{
			codeTreeView.Visible = true;
			codeFilterView.Visible = false;
			textBoxWait.Visible = false;
		}

		/// <summary>
		/// Makes the filtered TreeView visible, and makes the unfiltered TreeView 
		/// and the 'Please Wait' message invisible.
		/// </summary>
		public void ShowFilter()
		{
			codeTreeView.Visible = false;
			codeFilterView.Visible = true;
			textBoxWait.Visible = false;
		}

		public void ShowException(Exception parseException)
		{
			HideTrees();
			textBoxWait.UseWaitCursor = false;
			if (parseException != null)
				textBoxWait.Text = parseException.ToString();
			else
				textBoxWait.Text = "Unknown exception (parseException == null).";
		}

		/// <summary>
		/// Makes the filtered and unfiltered TreeViews invisible, 
		/// and makes the 'Please Wait' message visible. 
		/// </summary>
		public void ShowWaitWhileReadyMessage()
		{
			_treeViewWasVisible = codeTreeView.Visible;

			codeTreeView.Visible = false;
			codeFilterView.Visible = false;

			textBoxWait.UseWaitCursor = true;
			textBoxWait.Text = Environment.NewLine + Environment.NewLine + "Updating, please wait...";

			textBoxWait.Visible = true;

			this.Refresh();
		}

		/// <summary>
		/// Restores the visibility of the filtered and unfiltered TreeViews,
		/// and makes the 'Please Wait' message invisible.
		/// </summary>
		public void HideWaitWhileReadyMessage()
		{
			codeTreeView.Visible = _treeViewWasVisible;
			codeFilterView.Visible = !_treeViewWasVisible;
			textBoxWait.Visible = false;
			this.Refresh();
		}

		/// <summary>
		/// Shows the appropriate TreeView if the source file
		///  has been loaded, otherwise does nothing.
		/// </summary>
		public void Reset()
		{
			if (!textBoxWait.Visible)
			{
				if (filterToolStripCombo.SelectedIndex == 0)
					ShowTree();
				else
					ShowFilter();
			}
		}
	}
}