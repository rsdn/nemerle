namespace WinFormTestHint
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
					System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
					this.button1 = new System.Windows.Forms.Button();
					this.richTextBox1 = new System.Windows.Forms.RichTextBox();
					this.checkBox1 = new System.Windows.Forms.CheckBox();
					this.textBox1 = new System.Windows.Forms.TextBox();
					this.label1 = new System.Windows.Forms.Label();
					this.checkBox2 = new System.Windows.Forms.CheckBox();
					this.SuspendLayout();
					// 
					// button1
					// 
					this.button1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
					this.button1.Location = new System.Drawing.Point(255, 333);
					this.button1.Name = "button1";
					this.button1.Size = new System.Drawing.Size(278, 41);
					this.button1.TabIndex = 0;
					this.button1.Text = "test button";
					this.button1.UseVisualStyleBackColor = true;
					this.button1.MouseEnter += new System.EventHandler(this.button1_MouseEnter);
					// 
					// richTextBox1
					// 
					this.richTextBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
											| System.Windows.Forms.AnchorStyles.Left)
											| System.Windows.Forms.AnchorStyles.Right)));
					this.richTextBox1.Location = new System.Drawing.Point(10, 11);
					this.richTextBox1.Name = "richTextBox1";
					this.richTextBox1.Size = new System.Drawing.Size(538, 307);
					this.richTextBox1.TabIndex = 4;
					this.richTextBox1.Text = resources.GetString("richTextBox1.Text");
					// 
					// checkBox1
					// 
					this.checkBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
					this.checkBox1.AutoSize = true;
					this.checkBox1.Location = new System.Drawing.Point(10, 331);
					this.checkBox1.Name = "checkBox1";
					this.checkBox1.Size = new System.Drawing.Size(151, 19);
					this.checkBox1.TabIndex = 5;
					this.checkBox1.Text = "Change hint after 2 sec";
					this.checkBox1.UseVisualStyleBackColor = true;
					// 
					// textBox1
					// 
					this.textBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
					this.textBox1.Location = new System.Drawing.Point(77, 351);
					this.textBox1.Name = "textBox1";
					this.textBox1.Size = new System.Drawing.Size(62, 20);
					this.textBox1.TabIndex = 6;
					this.textBox1.Text = "600";
					// 
					// label1
					// 
					this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
					this.label1.AutoSize = true;
					this.label1.Location = new System.Drawing.Point(10, 353);
					this.label1.Name = "label1";
					this.label1.Size = new System.Drawing.Size(68, 15);
					this.label1.TabIndex = 7;
					this.label1.Text = "Wrap width";
					// 
					// checkBox2
					// 
					this.checkBox2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
					this.checkBox2.AutoSize = true;
					this.checkBox2.Location = new System.Drawing.Point(10, 375);
					this.checkBox2.Name = "checkBox2";
					this.checkBox2.Size = new System.Drawing.Size(227, 19);
					this.checkBox2.TabIndex = 8;
					this.checkBox2.Text = "Change wrap width to 200 after 2 sec";
					this.checkBox2.UseVisualStyleBackColor = true;
					// 
					// Form1
					// 
					this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
					this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
					this.ClientSize = new System.Drawing.Size(569, 420);
					this.Controls.Add(this.checkBox2);
					this.Controls.Add(this.label1);
					this.Controls.Add(this.textBox1);
					this.Controls.Add(this.checkBox1);
					this.Controls.Add(this.richTextBox1);
					this.Controls.Add(this.button1);
					this.Name = "Form1";
					this.Text = "Form1";
					this.ResumeLayout(false);
					this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.RichTextBox richTextBox1;
        private System.Windows.Forms.CheckBox checkBox1;
        private System.Windows.Forms.TextBox textBox1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckBox checkBox2;
    }
}

