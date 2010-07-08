/*
 * Created by SharpDevelop.
 * User: smatveev
 * Date: 07.07.2010
 * Time: 11:00
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

using JSEngine;

namespace Test
{
    /// <summary>
    /// Description of MainForm.
    /// </summary>
    public partial class MainForm : Form
    {
        public MainForm()
        {
            //
            // The InitializeComponent() call is required for Windows Forms designer support.
            //
            InitializeComponent();
            
            //
            // TODO: Add constructor code after the InitializeComponent() call.
            //
        }

        void Txt_parseClick(object sender, EventArgs e)
        {
            var engine = new JSParser();
            var result = engine.Parse(txt_input.Text);
            if(result.IsSome) {
                var sb = new System.Text.StringBuilder();
                foreach(var s in result.Value)
                    s.ToString(sb, "    ", "");
                txt_output.Text = sb.ToString();
            } else {
                txt_output.Text = "Can't parse input.";
            }
        }
    }
}
