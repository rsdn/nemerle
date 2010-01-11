using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using Nemerle.Completion2;
using System.IO;

using Nemerle.VisualStudio.Project;
using Nemerle.VisualStudio.LanguageService;

namespace Nemerle.VisualStudio.GUI
{
	public partial class GotoUsageForm : Form
	{
		public GotoInfo Result;

		public GotoUsageForm(IList<GotoInfo> gotoInfos)
		{
			InitializeComponent();

			foreach (GotoInfo gotoInfo in gotoInfos)
			{
				ProjectInfo project = ProjectInfo.FindProject(gotoInfo.FilePath);
				string text;
				if (project == null)
				{
					string[] strs = File.ReadAllLines(gotoInfo.FilePath);
					int line = gotoInfo.Line - 1;
					if (line < strs.Length)
						text = strs[line].Trim().Replace("\t", "  ");
					else
						text = "???";
				}
				else
					text = gotoInfo.GetLineOfCode(project.Engine).Trim().Replace("\t", "  ");

				ListViewItem lvi = new ListViewItem(new string[]
				{
					Path.GetFileName(gotoInfo.FilePath),
					gotoInfo.Line.ToString(),
					gotoInfo.UsageTypeToString(),
					text
				});
				lvi.ToolTipText = gotoInfo.FilePath;
				lvi.Tag = gotoInfo;
				_listView.Items.Add(lvi);
			}
		}

		protected override void OnLoad(EventArgs e)
		{
			base.OnLoad(e);

			Rectangle parentRect = Screen.FromControl(this).WorkingArea;

			// Adjust size
			//
			_listView.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);

			ClientSize = _listView.PreferredSize;

			if (Width < 800)
				Width = 800;
			if (Height < 300)
				Height = 300;
			if (Width > parentRect.Width * 4/5)
				Width  = parentRect.Width * 4/5;
			if (Height > parentRect.Height*2/3)
				Height = parentRect.Height*2/3;

			// Adjust position
			//
			Left = parentRect.Left + (parentRect.Width  - Width)  / 2;
			Top  = parentRect.Top  + (parentRect.Height - Height) / 2;

			// Select first item by default
			//
			if (_listView.Items.Count > 0)
				_listView.SelectedIndices.Add(0);
		}

		protected override void OnKeyPress(KeyPressEventArgs e)
		{
			base.OnKeyPress(e);

			if (e.KeyChar == (char)Keys.Escape)
				Close(DialogResult.Cancel);
			else if (e.KeyChar == '\r' || e.KeyChar == ' ')
				Close(DialogResult.OK);
		}

		protected override void OnFormClosing(FormClosingEventArgs e)
		{
			base.OnClosing(e);

			if (DialogResult == DialogResult.OK)
				if (_listView.FocusedItem.Tag == null)
				{
					e.Cancel = true;
					MessageBox.Show(this, "You must select an item.", "Goto",
						MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
				else
					Result = (GotoInfo)_listView.FocusedItem.Tag;
		}

		private void HandleListViewMouseDoubleClick(object sender, MouseEventArgs e)
		{
			Close(DialogResult.OK);
		}

		private void Close(DialogResult result)
		{
			DialogResult = result;
			Close();
		}
	}
}