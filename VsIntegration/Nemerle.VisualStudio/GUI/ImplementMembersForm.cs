using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Nemerle.VisualStudio.LanguageService;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.GUI
{
	public partial class ImplementMembersForm : Form
	{
		readonly NemerleSource _source;
		readonly TypeBuilder _ty;
		readonly IEnumerable<IMember> _unimplementedMembers;

		public ImplementMembersForm(NemerleSource source, TypeBuilder ty, IEnumerable<IMember> unimplementedMembers)
		{
			_source               = source;
			_ty                   = ty;
			_unimplementedMembers = unimplementedMembers;
			InitializeComponent();
			var itfs = _unimplementedMembers.GroupBy(m => m.DeclaringType);
			FillTable(itfs);
		}
		
		void FillTable(IEnumerable<IGrouping<TypeInfo, IMember>> itfs)
		{
			var accessModaCol = (DataGridViewComboBoxColumn)_grid.Columns["AccessMods"];

			accessModaCol.Items.AddRange("public", "private", "protected", "internal", "protected internal");
			_grid.Rows.Add("All", true);

			foreach (var item in itfs)
			{
				var rowIndex = _grid.Rows.Add(item.Key.FullName + " interface", true);
				var row = _grid.Rows[rowIndex];
				row.Cells[0].Style.Font = new Font(_grid.DefaultCellStyle.Font, FontStyle.Bold);

				foreach (var m in item)
				{
					rowIndex = _grid.Rows.Add("      " + m.Name, true, false, null, m.Name);
					row = _grid.Rows[rowIndex];

					row.Tag = m;
				}
			}
		}

		private void ImplementMembersForm_Load(object sender, EventArgs e)
		{

		}
	}
}
