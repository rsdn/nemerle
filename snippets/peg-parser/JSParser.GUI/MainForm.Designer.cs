/*
 * Created by SharpDevelop.
 * User: smatveev
 * Date: 07.07.2010
 * Time: 11:00
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */
namespace Test
{
    partial class MainForm
    {
        /// <summary>
        /// Designer variable used to keep track of non-visual components.
        /// </summary>
        private System.ComponentModel.IContainer components = null;
        
        /// <summary>
        /// Disposes resources used by the form.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }
        
        /// <summary>
        /// This method is required for Windows Forms designer support.
        /// Do not change the method contents inside the source code editor. The Forms designer might
        /// not be able to load this method if it was changed manually.
        /// </summary>
        private void InitializeComponent()
        {
        	System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
        	this.txt_input = new System.Windows.Forms.TextBox();
        	this.txt_parse = new System.Windows.Forms.Button();
        	this.txt_output = new System.Windows.Forms.TextBox();
        	this.SuspendLayout();
        	// 
        	// txt_input
        	// 
        	this.txt_input.Font = new System.Drawing.Font("Courier New", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
        	this.txt_input.Location = new System.Drawing.Point(12, 12);
        	this.txt_input.Multiline = true;
        	this.txt_input.Name = "txt_input";
        	this.txt_input.ScrollBars = System.Windows.Forms.ScrollBars.Both;
        	this.txt_input.Size = new System.Drawing.Size(450, 240);
        	this.txt_input.TabIndex = 0;
        	this.txt_input.Text = resources.GetString("txt_input.Text");
        	this.txt_input.WordWrap = false;
        	// 
        	// txt_parse
        	// 
        	this.txt_parse.Location = new System.Drawing.Point(387, 258);
        	this.txt_parse.Name = "txt_parse";
        	this.txt_parse.Size = new System.Drawing.Size(75, 23);
        	this.txt_parse.TabIndex = 1;
        	this.txt_parse.Text = "Parse!";
        	this.txt_parse.UseVisualStyleBackColor = true;
        	this.txt_parse.Click += new System.EventHandler(this.Txt_parseClick);
        	// 
        	// txt_output
        	// 
        	this.txt_output.Font = new System.Drawing.Font("Courier New", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
        	this.txt_output.Location = new System.Drawing.Point(12, 287);
        	this.txt_output.Multiline = true;
        	this.txt_output.Name = "txt_output";
        	this.txt_output.ReadOnly = true;
        	this.txt_output.ScrollBars = System.Windows.Forms.ScrollBars.Both;
        	this.txt_output.Size = new System.Drawing.Size(450, 240);
        	this.txt_output.TabIndex = 2;
        	this.txt_output.WordWrap = false;
        	// 
        	// MainForm
        	// 
        	this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
        	this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        	this.ClientSize = new System.Drawing.Size(474, 539);
        	this.Controls.Add(this.txt_output);
        	this.Controls.Add(this.txt_parse);
        	this.Controls.Add(this.txt_input);
        	this.Name = "MainForm";
        	this.Text = "JavaScript parser";
        	this.ResumeLayout(false);
        	this.PerformLayout();
        }
        private System.Windows.Forms.TextBox txt_output;
        private System.Windows.Forms.Button txt_parse;
        private System.Windows.Forms.TextBox txt_input;
    }
}
