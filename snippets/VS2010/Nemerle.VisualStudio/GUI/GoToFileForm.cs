using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;

namespace Nemerle.VisualStudio.GUI
{
	public partial class GoToFileForm : Form
	{
		public GoToFileForm()
		{
			InitializeComponent();
		}

		List<string> _fileNames = new List<string>();

		public void SetFiles(IEnumerable<string> files)
		{
			cbFiles.BeginUpdate();
			cbFiles.Items.Clear();
			foreach (var file in files)
			{
				_fileNames.Add(file);
				cbFiles.Items.Add(Path.GetFileName(file));
			}
			cbFiles.EndUpdate();
		}

		public string SelectedFileName
		{
			get { return cbFiles.SelectedIndex > -1 ? _fileNames[cbFiles.SelectedIndex] : null; }
		}

		private void GoToFileForm_FormClosing(object sender, FormClosingEventArgs e)
		{
			if (DialogResult == DialogResult.OK && cbFiles.SelectedIndex <= 0 && e.CloseReason == CloseReason.UserClosing)
				e.Cancel = false;
		}
	}
}
