namespace Nemerle.VisualStudio.GUI
{
	partial class PromptProjectRenameForm
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
			this.components = new System.ComponentModel.Container();
			this._newProjectName = new System.Windows.Forms.TextBox();
			this.label1 = new System.Windows.Forms.Label();
			this.label2 = new System.Windows.Forms.Label();
			this._yesButton = new System.Windows.Forms.Button();
			this._noButton = new System.Windows.Forms.Button();
			this._errorProvider = new System.Windows.Forms.ErrorProvider(this.components);
			((System.ComponentModel.ISupportInitialize)(this._errorProvider)).BeginInit();
			this.SuspendLayout();
			// 
			// _newProjectName
			// 
			this._newProjectName.Location = new System.Drawing.Point(16, 37);
			this._newProjectName.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
			this._newProjectName.Name = "_newProjectName";
			this._newProjectName.Size = new System.Drawing.Size(347, 22);
			this._newProjectName.TabIndex = 0;
			this._newProjectName.TextChanged += new System.EventHandler(this._newProjectName_TextChanged);
			this._newProjectName.Validating += new System.ComponentModel.CancelEventHandler(this._newProjectName_Validating);
			// 
			// label1
			// 
			this.label1.Location = new System.Drawing.Point(16, 65);
			this.label1.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(363, 80);
			this.label1.TabIndex = 1;
			this.label1.Text = "You can create new project file (copy) with other name or change it inplace.\r\n\r\nR" +
    "ename the project?";
			// 
			// label2
			// 
			this.label2.AutoSize = true;
			this.label2.Location = new System.Drawing.Point(16, 11);
			this.label2.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(125, 17);
			this.label2.TabIndex = 2;
			this.label2.Text = "New project name:";
			// 
			// _yesButton
			// 
			this._yesButton.Location = new System.Drawing.Point(191, 149);
			this._yesButton.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
			this._yesButton.Name = "_yesButton";
			this._yesButton.Size = new System.Drawing.Size(100, 28);
			this._yesButton.TabIndex = 3;
			this._yesButton.Text = "&Yes";
			this._yesButton.UseVisualStyleBackColor = true;
			this._yesButton.Click += new System.EventHandler(this._yesButton_Click);
			// 
			// _noButton
			// 
			this._noButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this._noButton.Location = new System.Drawing.Point(299, 149);
			this._noButton.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
			this._noButton.Name = "_noButton";
			this._noButton.Size = new System.Drawing.Size(100, 28);
			this._noButton.TabIndex = 4;
			this._noButton.Text = "&No";
			this._noButton.UseVisualStyleBackColor = true;
			// 
			// _errorProvider
			// 
			this._errorProvider.ContainerControl = this;
			// 
			// PromptProjectRenameForm
			// 
			this.AcceptButton = this._yesButton;
			this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.CancelButton = this._noButton;
			this.ClientSize = new System.Drawing.Size(409, 187);
			this.Controls.Add(this._noButton);
			this.Controls.Add(this._yesButton);
			this.Controls.Add(this.label2);
			this.Controls.Add(this.label1);
			this.Controls.Add(this._newProjectName);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "PromptProjectRenameForm";
			this.ShowIcon = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Rename project";
			this.TopMost = true;
			this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.PromptProjectRenameForm_FormClosing);
			((System.ComponentModel.ISupportInitialize)(this._errorProvider)).EndInit();
			this.ResumeLayout(false);
			this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.TextBox _newProjectName;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.Button _yesButton;
		private System.Windows.Forms.Button _noButton;
		private System.Windows.Forms.ErrorProvider _errorProvider;
	}
}