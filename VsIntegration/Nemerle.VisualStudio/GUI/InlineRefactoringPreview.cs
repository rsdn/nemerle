using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;

using Nemerle.Completion2;

namespace Nemerle.VisualStudio.GUI
{
	public partial class InlineRefactoringPreview : Form
	{
		public InlineRefactoringPreview(Nemerle.Completion2.Project project)
		{
			InitializeComponent();
			_project = project;
		}

		private void FillUsagesTextBox()
		{
			var prevTextLength = 0;
			rtbFoundUsages.Text = "";
			foreach (var usage in Usages)
			{
				var loc = usage.Location;

				var strUsageLine = loc.Line.ToString();
				var line = string.Format("{0}: {1}\n", strUsageLine, usage.GetLineOfCode(_project.Engine));
				rtbFoundUsages.AppendText(line);

				int lineStart = prevTextLength;
				int selectionLength = loc.EndColumn - loc.Column;
				rtbFoundUsages.SelectionStart = lineStart + strUsageLine.Length + 2 + loc.Column - 1;
				rtbFoundUsages.SelectionLength = selectionLength;
				rtbFoundUsages.SelectionBackColor = Color.LightGreen;
				rtbFoundUsages.SelectedText = ExpressionToInline;

				prevTextLength = rtbFoundUsages.Text.Length;
			}
			rtbFoundUsages.SelectionStart = rtbFoundUsages.SelectionLength = 0;
		}  

		public GotoInfo[] Usages{ get; set;}
		private Nemerle.Completion2.Project _project;

		public string ExpressionToInline
		{
			get; set;
		}

		private void InlineRefactoringPreview_Shown(object sender, EventArgs e)
		{
			FillUsagesTextBox();
		}
	}
}
