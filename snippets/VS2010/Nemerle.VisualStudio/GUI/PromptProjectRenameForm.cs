﻿using System;
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
	public partial class PromptProjectRenameForm : Form
	{
		public PromptProjectRenameForm(string projectName)
		{
			InitializeComponent();
			ProjectName = projectName;
			_newProjectName.Text = projectName + "-VS_2010";
		}

		public string ProjectName { get; private set; }

		private void PromptProjectRenameForm_FormClosing(object _, FormClosingEventArgs e)
		{
			e.Cancel = !ValidateData();

			if (e.Cancel)
				return;

			DialogResult = System.Windows.Forms.DialogResult.Yes;
			ProjectName = _newProjectName.Text;
		}

		bool ValidateData()
		{
			var newName = _newProjectName.Text;
			var isError = string.Equals(ProjectName, newName, StringComparison.InvariantCultureIgnoreCase);

			if (isError)
			{
				_errorProvider.SetError(_newProjectName, "The new project name must differ from the old name.");
				_yesButton.Enabled = false;
				return false;
			}

			var isInvalidName = newName.Any(c => Path.GetInvalidFileNameChars().Contains(c));

			if (isInvalidName)
			{
				_errorProvider.SetError(_newProjectName, "The new project name contains invalid characters.");
				_yesButton.Enabled = false;
				return false;
			}

			_errorProvider.SetError(_newProjectName, "");
			_yesButton.Enabled = true;
			return true;
		}

		private void _newProjectName_Validating(object _, CancelEventArgs e)
		{
			e.Cancel = !(_yesButton.Enabled = ValidateData());
		}

		private void _yesButton_Click(object sender, EventArgs e)
		{
			Close();
		}

		private void _newProjectName_TextChanged(object sender, EventArgs e)
		{
			_yesButton.Enabled = ValidateData();
		}
	}
}
