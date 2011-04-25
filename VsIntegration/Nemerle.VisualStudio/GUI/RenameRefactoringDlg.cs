using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using Nemerle.Completion2;

namespace Nemerle.VisualStudio.GUI
{
	public partial class RenameRefactoringDlg : Form
	{
		public RenameRefactoringDlg(Nemerle.Completion2.IIdeEngine engine, GotoInfo[] usages)
		{
			InitializeComponent();
			_usages = usages;
			FillUsagesTextBox(engine);
		}

		private void FillUsagesTextBox(Nemerle.Completion2.IIdeEngine engine)
		{
			string currentFileName = null;
			var lastTextLength = 0;
			rtbFoundUsages.Text = "";
			foreach(var usage in from use in _usages
									 orderby use.Location.File
									 select use)
			{
				var loc = usage.Location;
				if(loc.File != currentFileName)
				{
					currentFileName = loc.File;
					var fileString = string.Format("File: {0}\n", Path.GetFileName(currentFileName));
					rtbFoundUsages.AppendText(fileString);

					rtbFoundUsages.SelectionStart = lastTextLength;
					rtbFoundUsages.SelectionLength = fileString.Length;
					rtbFoundUsages.SelectionColor = Color.CornflowerBlue;

					lastTextLength = rtbFoundUsages.Text.Length;
				}
				var strUsageLine = loc.Line.ToString();
				var line = string.Format("{0}: {1}\n", strUsageLine, usage.GetLineOfCode(engine));
				rtbFoundUsages.AppendText(line);

				int lineStart = lastTextLength;
				int selectionLength = loc.EndColumn - loc.Column;
				rtbFoundUsages.SelectionStart = lineStart + strUsageLine.Length + 2 + loc.Column - 1;
				rtbFoundUsages.SelectionLength = selectionLength;

				if (_oldName == null)
				{
					_oldName = rtbFoundUsages.SelectedText;
					NewName = _oldName;
				}

				rtbFoundUsages.SelectionBackColor = Color.LightGreen;
				lastTextLength = rtbFoundUsages.Text.Length;
			}
			rtbFoundUsages.SelectionStart = rtbFoundUsages.SelectionLength = 0;
		}

		private GotoInfo[] _usages;
		private string _oldName = null;

		public string NewName
		{
			get { return tbNewName.Text; }
			private set { tbNewName.Text = value;}
		}

		private void tbNewName_TextChanged(object sender, EventArgs e)
		{
			var error = IsValidNewName(tbNewName.Text);
			if (error != null)
			{
				errorProvider1.SetError(tbNewName, error + " Cannot perform rename!");
				btnOK.Enabled = false;
			}
			else
			{
				errorProvider1.SetError(tbNewName, "");
				btnOK.Enabled = true;
			}
		}

		private static readonly Regex _nameRegex = new Regex(@"[\w_][\w\d_]*", RegexOptions.Compiled);

		private string IsValidNewName(string name)
		{
			if (string.IsNullOrEmpty(name))
				return "Name cannot be empty.";

			if (name == _oldName)
				return "This is the original name.";

			var match = _nameRegex.Match(name);
			if (!match.Success || match.ToString() != name)
				return "This is not a valid identifier.";

			return null;
		}
	}
}
