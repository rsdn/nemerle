namespace RSDN.Nemerle.Utils
{
    partial class Form1
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
			System.Windows.Forms.ToolStrip toolBarMain;
			System.Windows.Forms.ToolStripButton btnParse;
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
			System.Windows.Forms.ToolStripButton btnFillCode;
			System.Windows.Forms.SplitContainer splitContainer1;
			System.Windows.Forms.SplitContainer splitContainer2;
			this.txtSource = new System.Windows.Forms.RichTextBox();
			this.txtGenerated = new System.Windows.Forms.RichTextBox();
			this.txtErrors = new System.Windows.Forms.RichTextBox();
			this.btnCompile = new System.Windows.Forms.ToolStripButton();
			toolBarMain = new System.Windows.Forms.ToolStrip();
			btnParse = new System.Windows.Forms.ToolStripButton();
			btnFillCode = new System.Windows.Forms.ToolStripButton();
			splitContainer1 = new System.Windows.Forms.SplitContainer();
			splitContainer2 = new System.Windows.Forms.SplitContainer();
			toolBarMain.SuspendLayout();
			splitContainer1.Panel1.SuspendLayout();
			splitContainer1.Panel2.SuspendLayout();
			splitContainer1.SuspendLayout();
			splitContainer2.Panel1.SuspendLayout();
			splitContainer2.Panel2.SuspendLayout();
			splitContainer2.SuspendLayout();
			this.SuspendLayout();
			// 
			// toolBarMain
			// 
			toolBarMain.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            btnParse,
            btnFillCode,
            this.btnCompile});
			toolBarMain.Location = new System.Drawing.Point(0, 0);
			toolBarMain.Name = "toolBarMain";
			toolBarMain.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional;
			toolBarMain.Size = new System.Drawing.Size(702, 25);
			toolBarMain.TabIndex = 2;
			toolBarMain.Text = "toolStrip1";
			// 
			// btnParse
			// 
			btnParse.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
			btnParse.Image = ((System.Drawing.Image)(resources.GetObject("btnParse.Image")));
			btnParse.ImageTransparentColor = System.Drawing.Color.Magenta;
			btnParse.Name = "btnParse";
			btnParse.Size = new System.Drawing.Size(38, 22);
			btnParse.Text = "Parse";
			btnParse.Click += new System.EventHandler(this.btnParse_Click);
			// 
			// btnFillCode
			// 
			btnFillCode.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
			btnFillCode.Image = ((System.Drawing.Image)(resources.GetObject("btnFillCode.Image")));
			btnFillCode.ImageTransparentColor = System.Drawing.Color.Magenta;
			btnFillCode.Name = "btnFillCode";
			btnFillCode.Size = new System.Drawing.Size(49, 22);
			btnFillCode.Text = "Fill code";
			btnFillCode.Click += new System.EventHandler(this.btnFillCode_Click);
			// 
			// splitContainer1
			// 
			splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
			splitContainer1.Location = new System.Drawing.Point(0, 0);
			splitContainer1.Name = "splitContainer1";
			// 
			// splitContainer1.Panel1
			// 
			splitContainer1.Panel1.Controls.Add(this.txtSource);
			// 
			// splitContainer1.Panel2
			// 
			splitContainer1.Panel2.Controls.Add(this.txtGenerated);
			splitContainer1.Size = new System.Drawing.Size(702, 277);
			splitContainer1.SplitterDistance = 334;
			splitContainer1.TabIndex = 0;
			// 
			// txtSource
			// 
			this.txtSource.AcceptsTab = true;
			this.txtSource.Dock = System.Windows.Forms.DockStyle.Fill;
			this.txtSource.Location = new System.Drawing.Point(0, 0);
			this.txtSource.Name = "txtSource";
			this.txtSource.Size = new System.Drawing.Size(334, 277);
			this.txtSource.TabIndex = 0;
			this.txtSource.Text = "";
			this.txtSource.WordWrap = false;
			// 
			// txtGenerated
			// 
			this.txtGenerated.AcceptsTab = true;
			this.txtGenerated.Dock = System.Windows.Forms.DockStyle.Fill;
			this.txtGenerated.Location = new System.Drawing.Point(0, 0);
			this.txtGenerated.Name = "txtGenerated";
			this.txtGenerated.ReadOnly = true;
			this.txtGenerated.Size = new System.Drawing.Size(364, 277);
			this.txtGenerated.TabIndex = 0;
			this.txtGenerated.Text = "";
			this.txtGenerated.WordWrap = false;
			// 
			// splitContainer2
			// 
			splitContainer2.Dock = System.Windows.Forms.DockStyle.Fill;
			splitContainer2.Location = new System.Drawing.Point(0, 25);
			splitContainer2.Name = "splitContainer2";
			splitContainer2.Orientation = System.Windows.Forms.Orientation.Horizontal;
			// 
			// splitContainer2.Panel1
			// 
			splitContainer2.Panel1.Controls.Add(splitContainer1);
			// 
			// splitContainer2.Panel2
			// 
			splitContainer2.Panel2.Controls.Add(this.txtErrors);
			splitContainer2.Size = new System.Drawing.Size(702, 406);
			splitContainer2.SplitterDistance = 277;
			splitContainer2.TabIndex = 3;
			// 
			// txtErrors
			// 
			this.txtErrors.Dock = System.Windows.Forms.DockStyle.Fill;
			this.txtErrors.Location = new System.Drawing.Point(0, 0);
			this.txtErrors.Name = "txtErrors";
			this.txtErrors.Size = new System.Drawing.Size(702, 125);
			this.txtErrors.TabIndex = 0;
			this.txtErrors.Text = "";
			this.txtErrors.WordWrap = false;
			// 
			// btnCompile
			// 
			this.btnCompile.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
			this.btnCompile.Image = ((System.Drawing.Image)(resources.GetObject("btnCompile.Image")));
			this.btnCompile.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.btnCompile.Name = "btnCompile";
			this.btnCompile.Size = new System.Drawing.Size(48, 22);
			this.btnCompile.Text = "Compile";
			this.btnCompile.Click += new System.EventHandler(this.btnCompile_Click);
			// 
			// Form1
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(702, 431);
			this.Controls.Add(splitContainer2);
			this.Controls.Add(toolBarMain);
			this.Name = "Form1";
			this.Text = "NemerleCodeProvider Tester";
			toolBarMain.ResumeLayout(false);
			toolBarMain.PerformLayout();
			splitContainer1.Panel1.ResumeLayout(false);
			splitContainer1.Panel2.ResumeLayout(false);
			splitContainer1.ResumeLayout(false);
			splitContainer2.Panel1.ResumeLayout(false);
			splitContainer2.Panel2.ResumeLayout(false);
			splitContainer2.ResumeLayout(false);
			this.ResumeLayout(false);
			this.PerformLayout();

        }

        #endregion

		private System.Windows.Forms.RichTextBox txtGenerated;
		private System.Windows.Forms.RichTextBox txtSource;
		private System.Windows.Forms.RichTextBox txtErrors;
		private System.Windows.Forms.ToolStripButton btnCompile;
    }
}

