namespace Nemerle.VisualStudio.GUI
{
	partial class Options
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
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Options));
      this.okButton = new System.Windows.Forms.Button();
      this.cancelButton = new System.Windows.Forms.Button();
      this.highlightUsagesCheckBox = new System.Windows.Forms.CheckBox();
      this.highlightUsagesUnlessTerminalSessionCheckBox = new System.Windows.Forms.CheckBox();
      this.SuspendLayout();
      // 
      // okButton
      // 
      this.okButton.DialogResult = System.Windows.Forms.DialogResult.OK;
      this.okButton.Location = new System.Drawing.Point(355, 208);
      this.okButton.Name = "okButton";
      this.okButton.Size = new System.Drawing.Size(75, 23);
      this.okButton.TabIndex = 0;
      this.okButton.Text = "OK";
      this.okButton.UseVisualStyleBackColor = true;
      // 
      // cancelButton
      // 
      this.cancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
      this.cancelButton.Location = new System.Drawing.Point(436, 208);
      this.cancelButton.Name = "cancelButton";
      this.cancelButton.Size = new System.Drawing.Size(75, 23);
      this.cancelButton.TabIndex = 1;
      this.cancelButton.Text = "Cancel";
      this.cancelButton.UseVisualStyleBackColor = true;
      // 
      // highlightUsagesCheckBox
      // 
      this.highlightUsagesCheckBox.AutoSize = true;
      this.highlightUsagesCheckBox.Location = new System.Drawing.Point(12, 12);
      this.highlightUsagesCheckBox.Name = "highlightUsagesCheckBox";
      this.highlightUsagesCheckBox.Size = new System.Drawing.Size(104, 17);
      this.highlightUsagesCheckBox.TabIndex = 2;
      this.highlightUsagesCheckBox.Text = "Highlight usages";
      this.highlightUsagesCheckBox.UseVisualStyleBackColor = true;
      this.highlightUsagesCheckBox.CheckedChanged += new System.EventHandler(this.highlightUsagesCheckBox_CheckedChanged);
      // 
      // highlightUsagesUnlessTerminalSessionCheckBox
      // 
      this.highlightUsagesUnlessTerminalSessionCheckBox.AutoSize = true;
      this.highlightUsagesUnlessTerminalSessionCheckBox.Location = new System.Drawing.Point(26, 35);
      this.highlightUsagesUnlessTerminalSessionCheckBox.Name = "highlightUsagesUnlessTerminalSessionCheckBox";
      this.highlightUsagesUnlessTerminalSessionCheckBox.Size = new System.Drawing.Size(186, 17);
      this.highlightUsagesUnlessTerminalSessionCheckBox.TabIndex = 3;
      this.highlightUsagesUnlessTerminalSessionCheckBox.Text = "Unless terminal session is enabled";
      this.highlightUsagesUnlessTerminalSessionCheckBox.UseVisualStyleBackColor = true;
      // 
      // Options
      // 
      this.AcceptButton = this.okButton;
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.CancelButton = this.cancelButton;
      this.ClientSize = new System.Drawing.Size(523, 243);
      this.Controls.Add(this.highlightUsagesUnlessTerminalSessionCheckBox);
      this.Controls.Add(this.highlightUsagesCheckBox);
      this.Controls.Add(this.cancelButton);
      this.Controls.Add(this.okButton);
      this.Cursor = System.Windows.Forms.Cursors.Default;
      this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
      this.Name = "Options";
      this.Text = "Nemerle Options";
      this.ResumeLayout(false);
      this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.Button okButton;
		private System.Windows.Forms.Button cancelButton;
		private System.Windows.Forms.CheckBox highlightUsagesCheckBox;
		private System.Windows.Forms.CheckBox highlightUsagesUnlessTerminalSessionCheckBox;
	}
}