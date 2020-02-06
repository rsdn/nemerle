namespace Nemerle.VisualStudio.GUI
{
	partial class GoToFileForm
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
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(GoToFileForm));
			this.cbFiles = new System.Windows.Forms.ComboBox();
			this.tbOk = new System.Windows.Forms.Button();
			this.tbCancel = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// cbFiles
			// 
			this.cbFiles.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.Suggest;
			this.cbFiles.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
			this.cbFiles.FormattingEnabled = true;
			this.cbFiles.Location = new System.Drawing.Point(12, 12);
			this.cbFiles.Name = "cbFiles";
			this.cbFiles.Size = new System.Drawing.Size(419, 21);
			this.cbFiles.TabIndex = 0;
			// 
			// tbOk
			// 
			this.tbOk.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.tbOk.Location = new System.Drawing.Point(275, 40);
			this.tbOk.Name = "tbOk";
			this.tbOk.Size = new System.Drawing.Size(75, 23);
			this.tbOk.TabIndex = 1;
			this.tbOk.Text = "&OK";
			this.tbOk.UseVisualStyleBackColor = true;
			// 
			// tbCancel
			// 
			this.tbCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.tbCancel.Location = new System.Drawing.Point(356, 40);
			this.tbCancel.Name = "tbCancel";
			this.tbCancel.Size = new System.Drawing.Size(75, 23);
			this.tbCancel.TabIndex = 2;
			this.tbCancel.Text = "&Cancel";
			this.tbCancel.UseVisualStyleBackColor = true;
			// 
			// GoToFileForm
			// 
			this.AcceptButton = this.tbOk;
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.CancelButton = this.tbCancel;
			this.ClientSize = new System.Drawing.Size(439, 72);
			this.Controls.Add(this.tbCancel);
			this.Controls.Add(this.tbOk);
			this.Controls.Add(this.cbFiles);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
			this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
			this.MinimizeBox = false;
			this.Name = "GoToFileForm";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Go to file";
			this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.GoToFileForm_FormClosing);
			this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.ComboBox cbFiles;
		private System.Windows.Forms.Button tbOk;
		private System.Windows.Forms.Button tbCancel;
	}
}