namespace Nemerle.VisualStudio.GUI
{
	partial class GotoUsageForm
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary>
		/// Clean up any resources being used.
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

		#region Windows Form Designer generated code

		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(GotoUsageForm));
			this._listView = new Nemerle.VisualStudio.GUI.AutoSizeListView();
			this.FileName = new System.Windows.Forms.ColumnHeader();
			this.LineNumber = new System.Windows.Forms.ColumnHeader();
			this.TypeOfUsage = new System.Windows.Forms.ColumnHeader();
			this.LineOfCode = new System.Windows.Forms.ColumnHeader();
			this.SuspendLayout();
			// 
			// _listView
			// 
			this._listView.BorderStyle = System.Windows.Forms.BorderStyle.None;
			this._listView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.FileName,
            this.LineNumber,
            this.TypeOfUsage,
            this.LineOfCode});
			this._listView.Dock = System.Windows.Forms.DockStyle.Fill;
			this._listView.FullRowSelect = true;
			this._listView.Location = new System.Drawing.Point(0, 0);
			this._listView.Name = "_listView";
			this._listView.ShowItemToolTips = true;
			this._listView.Size = new System.Drawing.Size(670, 274);
			this._listView.TabIndex = 0;
			this._listView.UseCompatibleStateImageBehavior = false;
			this._listView.View = System.Windows.Forms.View.Details;
			this._listView.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.HandleListViewMouseDoubleClick);
			// 
			// FileName
			// 
			this.FileName.Text = "File";
			this.FileName.Width = 100;
			// 
			// LineNumber
			// 
			this.LineNumber.Text = "Line";
			this.LineNumber.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
			this.LineNumber.Width = 96;
			// 
			// TypeOfUsage
			// 
			this.TypeOfUsage.Text = "Type";
			// 
			// LineOfCode
			// 
			this.LineOfCode.Text = "Code";
			this.LineOfCode.Width = 237;
			// 
			// GotoUsageForm
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(670, 274);
			this.Controls.Add(this._listView);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
			this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
			this.KeyPreview = true;
			this.Name = "GotoUsageForm";
			this.ShowInTaskbar = false;
			this.Text = "Found Usages:";
			this.ResumeLayout(false);

		}

		#endregion

		private AutoSizeListView _listView;
		private System.Windows.Forms.ColumnHeader FileName;
		private System.Windows.Forms.ColumnHeader LineNumber;
		private System.Windows.Forms.ColumnHeader LineOfCode;
		private System.Windows.Forms.ColumnHeader TypeOfUsage;
	}
}