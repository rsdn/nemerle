using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Forms;
using WpfHint;

namespace WinFormTestHint
{
  public partial class Form1 : Form
  {
    private Hint hint = new Hint();
    private readonly Timer timer = new Timer { Interval = 2000 };

    public Form1()
    {
      InitializeComponent();
      timer.Tick += timer_Tick;
      hint.Click += hint_Click;
    }

    static void hint_Click(Hint ht, string handler)
    {
      Console.WriteLine("Hint clicked, handler name: " + handler);
    }

    private void button1_MouseEnter(object sender, EventArgs e)
    {
      if (hint.IsOpen) return;

      timer.Start();

      var but = (Button)sender;
      var rt  = but.RectangleToScreen(but.ClientRectangle);

      hint.WrapWidth = Int32.Parse(textBox1.Text);
			var rect = new Rect(rt.Left, rt.Top, rt.Width, rt.Height);
			hint.Show(IntPtr.Zero, rect, GetHintContent, richTextBox1.Text);
    }

		string GetHintContent(string key)
		{
			switch (key)
			{
				case "int":    return "<hint>System.<b>Int32</b>!!!</hint>";
				case "string": return "<hint>System.String!!!</hint>";
				default:       return "<hint>key '" + key + "' not found!</hint>";
			}
		}

    void timer_Tick(object sender, EventArgs e)
    {
      timer.Stop();
      if (checkBox1.Checked)
        hint.Text = "<hint>Hello world !!!</hint>";
      else if (checkBox2.Checked)
        hint.WrapWidth = 200;
    }
  }
}
