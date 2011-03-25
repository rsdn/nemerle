using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Nemerle.VisualStudio.Properties;

namespace Nemerle.VisualStudio.GUI
{
	public partial class Options : Form
	{
		public Options()
		{
			InitializeComponent();
			highlightUsagesCheckBox.Checked = Settings.Default.HighlightUsages;
			highlightUsagesUnlessTerminalSessionCheckBox.Checked =
				Settings.Default.HighlightUsagesUnlessTerminalSession;
		}

		private void highlightUsagesCheckBox_CheckedChanged(object sender, EventArgs e)
		{
			highlightUsagesUnlessTerminalSessionCheckBox.Enabled =
				highlightUsagesCheckBox.Checked;
		}

		public void Save()
		{
			Settings.Default.HighlightUsages = highlightUsagesCheckBox.Checked;
			Settings.Default.HighlightUsagesUnlessTerminalSession =
				highlightUsagesUnlessTerminalSessionCheckBox.Checked;
		}
	}
}