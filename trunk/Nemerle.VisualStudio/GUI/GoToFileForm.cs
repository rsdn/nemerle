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
            cbFiles.Items.Clear();
            foreach (var file in files)
            {
            	_fileNames.Add(file);
				cbFiles.Items.Add(Path.GetFileName(file));
            }
        }

    	public string SelectedFileName
    	{
			get { return cbFiles.SelectedIndex > -1? _fileNames[cbFiles.SelectedIndex] : null; }
    	}
    }
}
