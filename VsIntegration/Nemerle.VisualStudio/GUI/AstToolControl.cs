using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using Nemerle.Completion2;
using CompUtils = Nemerle.Compiler.Utils.Utils;
using Nemerle.Compiler;
using Nemerle.VisualStudio.Properties;
using Nemerle.VisualStudio.Project;
using Nemerle.VisualStudio.LanguageService;
using Nemerle.Compiler.Utils;
using NLocation = Nemerle.Compiler.Location;

namespace Nemerle.VisualStudio.GUI
{
	public partial class AstToolControl : UserControl
	{
		public AstToolControl()
		{
			InitializeComponent();
			_autoUpdateCheckBox.Checked = Settings.Default.AutoShowAst;
			_displayType.SelectedIndex = 1;
		}

		List<AstNodeInfo> _items = new List<AstNodeInfo>(10000);

		int _checkCount;

		public bool IsAutoUpdate
		{
			get { return _autoUpdateCheckBox.Checked; }
		}

		public void Activate(int line, int col)
		{
			_line.Text = line.ToString();
			_col.Text  = col.ToString();

			if (_items.Count <= 0)
				return;

			int index = NLocation.IndexOfMostNested2(_items,
				delegate(AstNodeInfo n) { return n.Location; }, line, col);

			if (index >= 0)
			  _grid.CurrentCell = _grid.Rows[index].Cells[0];
		}

		private void _grid_CellValueNeeded(object sender, DataGridViewCellValueEventArgs e)
		{
			try
			{
				e.Value = _items[e.RowIndex].Text;
			}
			catch { }
	}

		private void _grid_CellToolTipTextNeeded(object sender, DataGridViewCellToolTipTextNeededEventArgs e)
		{
			try
			{
				if (e.RowIndex >= 0)
					e.ToolTipText = _items[e.RowIndex].ToString();
			}
			catch { }
		}

		public static event ShowLocation ShowLocation;

		private void _grid_CellClick(object sender, DataGridViewCellEventArgs e)
		{
			try
			{
				if (ShowLocation != null)
				{
					Location loc = _items[e.RowIndex].Location;
					if (loc != NLocation.Default)
						ShowLocation(loc);
				}
			}
			catch { }
		}

		private void _autoUpdateCheckBox_CheckedChanged(object sender, EventArgs e)
		{
			Settings.Default.AutoShowAst = _autoUpdateCheckBox.Checked;
		}

		int _buildTypedtreeCount;

		public int BuildTypedtreeCount
		{
			get { return _buildTypedtreeCount; }
			set
			{
				_buildTypedtreeCount = value;
				Action action = () => { _buildTypedtreeCountLabel.Text = value.ToString(); };
				_buildTypedtreeCountLabel.BeginInvoke(action);
			}
		}

		internal void ShowInfo(NemerleSource source)
		{
			if (!IsAutoUpdate)
				return;

			Action action = () => 
			{
				_checkCountLabel.Text = (++_checkCount).ToString();
				_items.Clear();
				ProjectInfo projectInfo = source.ProjectInfo;

				if (projectInfo == null || !projectInfo.Engine.IsProjectAvailable)
					return;

				switch (_displayType.SelectedIndex)
				{
					case 0: // Tokens
						string code = source.GetText();
						
						LexerBase lex = new LexerString((ManagerClass)projectInfo.Engine, code,
							new Location(source.FileIndex, 1, 1));
						//lex.BeginParseFile();
						lex.Keywords = lex.Manager.CoreEnv.Keywords;
						AstUtils.FillList(lex, _items);
						break;
					case 1: // AST
						//var ns = projectInfo.Engine.Project.CompileUnits
						//	                    .GetTopNamespace(source.FileIndex);
						//AstUtils.FillList(ns, _items);
						break;
				}

				_grid.RowCount = _items.Count;
				_grid.Invalidate();
				_grid.Update();
			};

			_checkCountLabel.BeginInvoke(action);
		}

		private void _grid_CellFormatting(object sender, DataGridViewCellFormattingEventArgs e)
		{
			Location loc = _items[e.RowIndex].Location;
			if (loc == NLocation.Default)
			{
				e.CellStyle.ForeColor = Color.Red;
				e.FormattingApplied = true;
			}
		}
	}
}
