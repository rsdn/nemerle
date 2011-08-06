using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace Nemerle.VisualStudio.GUI.Wizards
{
	public partial class AddNewItemWizard_Macro_Form : Form
	{
		public AddNewItemWizard_Macro_Form()
		{
			InitializeComponent();
			
			_macroTypeComboBox.Items.AddRange(new MacroType[] { new MacroType.Attribute(), new MacroType.Expression() });
			_macroPhaseBomboBox.Items.AddRange(GetMacroPhaseNames());
			_macroTypeComboBox.SelectedIndex = 0;
		}

		private static string[] GetMacroPhaseNames()
		{
			return Enum.GetNames(typeof(MacroPhase)).Where(n => n != "None").ToArray();
		}

		public MacroType MacroType { get; set; }

		#region Helpers

		private MacroPhase ToMacroPhase(object value)
		{
			return (MacroPhase)Enum.Parse(typeof(MacroPhase), value.ToString());
		}

		private static bool TestFirstNameChar(char value)
		{
			return char.IsLetter(value) || value == '_';
		}

		private static bool TestNextNameChar(char value)
		{
			return char.IsLetterOrDigit(value) || value == '_';
		}

		private bool IsNotInsertedRow(int rowIndex)
		{
			var lastRowIndex = _grid.RowCount - 1;
			return rowIndex >= lastRowIndex;
		}

		bool IsLastRow(int rowIndex)
		{
			var lastRowIndex = _grid.RowCount - 2;
			return rowIndex >= lastRowIndex;
		}

		#endregion

		private void _macroTypeComboBox_SelectedIndexChanged(object sender, EventArgs e)
		{
			MacroType = (MacroType)_macroTypeComboBox.SelectedItem;

			_defineSyntaxCheckBox.Checked = MacroType.DefineSyntax;

			var expression = MacroType as MacroType.Expression;

			if (expression != null)
				Select(expression);
			else
				Select((MacroType.Attribute)this.MacroType);
		}

		private void Select(MacroType.Attribute attribute)
		{
			_macroAttributeSettingsGroupBox.Enabled = true;
			_macroPhaseBomboBox.SelectedItem = attribute.MacroPhase.ToString();
			_validOnComboBox.SelectedItem = Utils.ValidOnToString(attribute.ValidOn);
		}

		private void Select(MacroType.Expression expression)
		{
			_macroAttributeSettingsGroupBox.Enabled = false;
		}

		private void _defineSyntaxCheckBox_CheckedChanged(object sender, EventArgs e)
		{
			MacroType.DefineSyntax = _defineSyntaxCheckBox.Checked;
		}

		private void _validOnComboBox_SelectedIndexChanged(object sender, EventArgs e)
		{
			((MacroType.Attribute)MacroType).ValidOn = Utils.ToValidOn(_validOnComboBox.SelectedItem);
		}

		private void _macroPhaseBomboBox_SelectedIndexChanged(object sender, EventArgs e)
		{
			((MacroType.Attribute)MacroType).MacroPhase = ToMacroPhase(_macroPhaseBomboBox.SelectedItem);
		}

		bool ValidateData()
		{
			var parms = new string[_grid.RowCount - 1][];

			for (int i = 0; i < parms.Length; i++)
				parms[i] = new string[3];

			if (_grid.RowCount > 1) // The first row is a new row. Ignore it.
			{
				#region Validating

				var cellValidators = new Func<DataGridViewCell, string, bool>[] { ValidatingNameCell, ValidatingTypeCell, ValidatingDefaultValueCell };
				var lastRowIndex   = _grid.RowCount - 2;

				for (int i = 0; i <= lastRowIndex; i++)
				{
					var row = _grid.Rows[i];
					for (int cellIndex = 0; cellIndex < row.Cells.Count; cellIndex++)
					{
						var cell   = row.Cells[cellIndex];
						var value  = cell.FormattedValue.ToString();
						var result = cellValidators[cellIndex](cell, value);

						if (result)
							parms[i][cellIndex] = value;
						else
						{
							ShowErrorMsgForCell(cell);
							return false;
						}
					}
				}

				#endregion
			}

			MacroType.Parameters = parms.Select(p => new ParameterDef(p[0], p[1], "", p[2])).ToArray();
			//return false;
			return true;
		}

		private void ShowErrorMsgForCell(DataGridViewCell cell)
		{
			ShowErrorMsg(cell.ErrorText);
			_grid.Select();
			foreach (DataGridViewCell c in _grid.SelectedCells)
				c.Selected = false;
			cell.Selected = true;
		}

		private System.Windows.Forms.DialogResult ShowErrorMsg(string msg)
		{
			return MessageBox.Show(this, msg, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
		}

		private void _okButton_Click(object sender, EventArgs e)
		{
			if (ValidateData())
			{
				DialogResult = System.Windows.Forms.DialogResult.OK;
				Close();
			}
		}

		private void _grid_CellValidating(object sender, DataGridViewCellValidatingEventArgs e)
		{
			var column = _grid.Columns[e.ColumnIndex];
			var cell   = _grid[e.ColumnIndex, e.RowIndex];
			var value  = e.FormattedValue.ToString();
			
			switch (column.Name)
			{
				case "NameColl":         ValidatingNameCell(cell, value); return;
				case "TypeColl":         ValidatingTypeCell(cell, value); return;
				case "DefaultValueColl": ValidatingDefaultValueCell(cell, value); return;
				default: throw new ApplicationException("Invalid column name " + column.Name);
			}
		}

		private bool ValidatingDefaultValueCell(DataGridViewCell cell, string value)
		{
			cell.ErrorText = "";
			return true;
		}

		private bool ValidatingTypeCell(DataGridViewCell cell, string value)
		{
			if (IsNotInsertedRow(cell.RowIndex))
				return false;

			if (value.Length < 1)
				cell.ErrorText = "Yoe must select type of parameter.";
			if (!IsLastRow(cell.RowIndex) && value.StartsWith("params", StringComparison.InvariantCulture))
				cell.ErrorText = "Parametr array (or list) must be last parametr!";
			else
			{
				cell.ErrorText = "";
				return true;
			}

			return false;
		}

		private bool ValidatingNameCell(DataGridViewCell cell, string value)
		{
			if (IsNotInsertedRow(cell.RowIndex))
				return false;
			if (value.Length < 1)
				cell.ErrorText =  "Length must be grate then 0";
			else if (value[0] != '@' && !TestFirstNameChar(value[0]))
				cell.ErrorText = "First character of name must be '@', latter or '_'";
			else if (!value.Skip(1).All(TestNextNameChar))
				cell.ErrorText = "Name contains in characters.";
			else
			{
				cell.ErrorText = "";
				return true;
			}

			return false;
		}
	}
}
