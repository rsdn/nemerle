/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

namespace Nemerle.VisualStudio.GUI.SourceOutliner
{
	partial class SourceOutlinerControl
	{
		/// <summary> 
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary> 
		/// Cleans up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing)
		{
			if (disposing && (components != null))
			{
				components.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Component Designer generated code

		/// <summary> 
		/// Required method for Designer support - do not modify 
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SourceOutlinerControl));
			this.filterToolStrip = new System.Windows.Forms.ToolStrip();
			this.filterToolStripCombo = new System.Windows.Forms.ToolStripComboBox();
			this.clearFilterButton = new System.Windows.Forms.ToolStripButton();
			this.filterStringTextBox = new System.Windows.Forms.TextBox();
			this.codeTreeView = new System.Windows.Forms.TreeView();
			this.codeFilterView = new System.Windows.Forms.TreeView();
			this.textBoxWait = new System.Windows.Forms.TextBox();
			this.filterToolStrip.SuspendLayout();
			this.SuspendLayout();
			// 
			// filterToolStrip
			// 
			this.filterToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
			this.filterToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.filterToolStripCombo,
            this.clearFilterButton});
			this.filterToolStrip.Location = new System.Drawing.Point(0, 0);
			this.filterToolStrip.Name = "filterToolStrip";
			this.filterToolStrip.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional;
			this.filterToolStrip.Size = new System.Drawing.Size(352, 25);
			this.filterToolStrip.TabIndex = 0;
			this.filterToolStrip.Text = "toolStrip1";
			// 
			// filterToolStripCombo
			// 
			this.filterToolStripCombo.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.filterToolStripCombo.MaxDropDownItems = 12;
			this.filterToolStripCombo.Name = "filterToolStripCombo";
			this.filterToolStripCombo.Size = new System.Drawing.Size(121, 25);
			this.filterToolStripCombo.Sorted = true;
			this.filterToolStripCombo.ToolTipText = "Filter By Type";
			// 
			// clearFilterButton
			// 
			this.clearFilterButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
			this.clearFilterButton.Enabled = false;
			this.clearFilterButton.Image = ((System.Drawing.Image)(resources.GetObject("clearFilterButton.Image")));
			this.clearFilterButton.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.clearFilterButton.Name = "clearFilterButton";
			this.clearFilterButton.Size = new System.Drawing.Size(23, 22);
			this.clearFilterButton.Text = "Clear";
			this.clearFilterButton.ToolTipText = "Clear Filters";
			// 
			// filterStringTextBox
			// 
			this.filterStringTextBox.Dock = System.Windows.Forms.DockStyle.Top;
			this.filterStringTextBox.Location = new System.Drawing.Point(0, 25);
			this.filterStringTextBox.Name = "filterStringTextBox";
			this.filterStringTextBox.Size = new System.Drawing.Size(352, 20);
			this.filterStringTextBox.TabIndex = 2;
			this.filterStringTextBox.Text = "<Filter>";
			// 
			// codeTreeView
			// 
			this.codeTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
			this.codeTreeView.HideSelection = false;
			this.codeTreeView.Location = new System.Drawing.Point(0, 45);
			this.codeTreeView.Name = "codeTreeView";
			this.codeTreeView.ShowNodeToolTips = true;
			this.codeTreeView.Size = new System.Drawing.Size(352, 294);
			this.codeTreeView.TabIndex = 1;
			// 
			// codeFilterView
			// 
			this.codeFilterView.Dock = System.Windows.Forms.DockStyle.Fill;
			this.codeFilterView.FullRowSelect = true;
			this.codeFilterView.HideSelection = false;
			this.codeFilterView.Location = new System.Drawing.Point(0, 45);
			this.codeFilterView.Name = "codeFilterView";
			this.codeFilterView.ShowLines = false;
			this.codeFilterView.ShowNodeToolTips = true;
			this.codeFilterView.ShowRootLines = false;
			this.codeFilterView.Size = new System.Drawing.Size(352, 294);
			this.codeFilterView.TabIndex = 3;
			this.codeFilterView.Visible = false;
			// 
			// textBoxWait
			// 
			this.textBoxWait.BackColor = System.Drawing.SystemColors.Window;
			this.textBoxWait.Dock = System.Windows.Forms.DockStyle.Fill;
			this.textBoxWait.Location = new System.Drawing.Point(0, 45);
			this.textBoxWait.Name = "textBoxWait";
			this.textBoxWait.ReadOnly = true;
			this.textBoxWait.Multiline = true;
			this.textBoxWait.WordWrap = false;
			this.textBoxWait.Size = new System.Drawing.Size(352, 294);
			this.textBoxWait.TabIndex = 1;
			this.textBoxWait.Text = "Updating, please wait...";
			this.textBoxWait.UseWaitCursor = true;
			this.textBoxWait.Visible = false;
			// 
			// SourceOutlinerControl
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.Controls.Add(this.textBoxWait);
			this.Controls.Add(this.codeFilterView);
			this.Controls.Add(this.codeTreeView);
			this.Controls.Add(this.filterStringTextBox);
			this.Controls.Add(this.filterToolStrip);
			this.Enabled = false;
			this.Name = "SourceOutlinerControl";
			this.Size = new System.Drawing.Size(352, 339);
			this.filterToolStrip.ResumeLayout(false);
			this.filterToolStrip.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();

		}

		#endregion Component Designer generated code

		public System.Windows.Forms.ToolStripComboBox filterToolStripCombo;
		public System.Windows.Forms.TextBox filterStringTextBox;
		protected System.Windows.Forms.TreeView codeTreeView;
		public System.Windows.Forms.ToolStrip filterToolStrip;
		public System.Windows.Forms.ToolStripButton clearFilterButton;
		protected System.Windows.Forms.TreeView codeFilterView;
		private System.Windows.Forms.TextBox textBoxWait;
	}
}
