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
using System.Diagnostics;
using TypeMembers = System.Collections.Generic.KeyValuePair<Nemerle.Compiler.MType.Class, Nemerle.Compiler.IMember>;

namespace Nemerle.VisualStudio.GUI
{
	public partial class ImplementMembersForm : Form
	{
		readonly NemerleSource _source;
		readonly TypeBuilder _ty;
		readonly IEnumerable<IMember> _unimplementedMembers;
    int _imageSize;

		public ImplementMembersForm(NemerleSource source, TypeBuilder ty, IEnumerable<IMember> unimplementedMembers)
		{
			_source               = source;
			_ty                   = ty;
			_unimplementedMembers = unimplementedMembers;
			
      InitializeComponent();

      imageList1.Images.AddStrip(Resources.SO_TreeViewIcons);
      _imageSize = imageList1.ImageSize.Width;
      Debug.Assert(imageList1.ImageSize.Width == imageList1.ImageSize.Height);

      if (_unimplementedMembers == null)
        return;

      //var _ty.GetDirectSuperTypes().GroupJoin(_unimplementedMembers, itf => itf.tycon, m => m.DeclaringType,
      //  (t, ms) => new { Ty = t, Members = ms });
      var implItfs = _ty.GetDirectSuperTypes().Where(t => t.IsInterface);
			var itfs = _unimplementedMembers.GroupBy(m => m.DeclaringType);
      var res = implItfs.Join(itfs, t => t.tycon, itf => itf.Key, (t, itf) => new { Group = itf, Ty = t });
      var ht = new Dictionary<MType.Class, IMember[]>();
      foreach (var item in res)
        ht[item.Ty] = ReplaceGettersAndSettersByProperties(item.Group);

      FillTable(ht);
		}

    void FillTable(Dictionary<MType.Class, IMember[]> itfs)
		{
      _grid.CellPainting += CellPainting;
      _grid.CellValueChanged += CellValueChanged;
      _grid.CellValidating += CellValidating;
      _grid.CurrentCellDirtyStateChanged += CurrentCellDirtyStateChanged;
      
      var accessModaCol = (DataGridViewComboBoxColumn)_grid.Columns["AccessMods"];
      var explicitCol = (DataGridViewCheckBoxColumn)_grid.Columns["Explicit"];

			accessModaCol.Items.AddRange("public", "private", "protected", "internal", "protected internal");
			_grid.Rows.Add("All", true);

			foreach (var item in itfs)
			{
				var rowIndex = _grid.Rows.Add(item.Key + " interface", true);
				var row = _grid.Rows[rowIndex];
				row.Cells[0].Style.Font = new Font(_grid.DefaultCellStyle.Font, FontStyle.Bold);

        foreach (var m in item.Value)
				{
          var name = m.Name;

          rowIndex = _grid.Rows.Add(m.Name, true, false, null, m.Name, m);
					row = _grid.Rows[rowIndex];
          row.Cells[0].Style.Padding = new Padding(_imageSize * 2, 0, 0, 0);
          var gray = Color.FromKnownColor(KnownColor.GrayText);
          var explicitCell = (DataGridViewCheckBoxCell)row.Cells["Explicit"];
          
          row.Cells["AccessMods"].Style.ForeColor = gray;
          row.Cells["ImplName"].Style.ForeColor = gray;
          row.Cells["Signature"].Style.ForeColor = gray;
          //var x0 = item.Key.TypeOfMember(item.Value[0]);
          //var x1 = item.Key.TypeOfMember(item.Value[1]);
          //var x2 = item.Key.TypeOfMember(item.Value[2]);
          row.Tag = item;
				}
			}
		}

    private static IMember[] ReplaceGettersAndSettersByProperties(IGrouping<TypeInfo, IMember> item)
    {
      var props = item.Key.GetMembers().OfType<IProperty>();
      var mems1 = item.Select(m => props.SingleOrDefault(p => p.GetGetter() == m || p.GetSetter() == m) ?? m);
      var mems2 = mems1.Distinct().ToArray();
      return mems2;
    }

    void CurrentCellDirtyStateChanged(object sender, EventArgs e)
    {
      if (_grid.IsCurrentCellDirty)
        _grid.CommitEdit(DataGridViewDataErrorContexts.Commit);
    }

    void CellValidating(object sender, DataGridViewCellValidatingEventArgs e)
    {
    }
    void CellValueChanged(object sender, DataGridViewCellEventArgs e)
    {
      var row = _grid.Rows[e.RowIndex];

      if (row.Tag == null)
        return;

      switch (_grid.Columns[e.ColumnIndex].Name)
      {
        case "AccessMods": case "ImplName":
          row.Cells["Explicit"].Value = true;
          break;
        default:
          break;
      }

      var isImpl = (bool)row.Cells["AddImplCol"].Value;
      var isExplicit = (bool)row.Cells["Explicit"].Value;

      row.Cells["Explicit"].Style.ForeColor = Color.FromKnownColor(isImpl ? KnownColor.WindowText : KnownColor.GrayText);
      var color = Color.FromKnownColor(isImpl && isExplicit ? KnownColor.WindowText : KnownColor.GrayText);
      row.Cells["AccessMods"].Style.ForeColor = color;
      row.Cells["ImplName"].Style.ForeColor = color;
      row.Cells["Signature"].Style.ForeColor = color;
    
      _grid.Invalidate();
    }

    void CellPainting(object sender, DataGridViewCellPaintingEventArgs e)
    {
      if (e.RowIndex < 0 || e.RowIndex >= _grid.RowCount)
        return;

      // Отрисовываем картинку описывающую член типа, а затем содержимое ячейки.

      var row = _grid.Rows[e.RowIndex];
      var r   = e.CellBounds;

      if (e.ColumnIndex == 0 && row.Tag != null)
      {
        var member   = ((TypeMembers)row.Tag).Value;
        var imgIndex = Nemerle.Compiler.Utils.Utils.GetGlyphIndex(member);

        e.Paint(r, e.PaintParts);
        e.Graphics.DrawImage(imageList1.Images[imgIndex], r.X + _imageSize - 2, r.Y + (r.Height - _imageSize) / 2, 
                             _imageSize, _imageSize);
        e.Handled = true; 
      }
      else if (row.Tag == null && e.ColumnIndex > 0)
      {
        e.PaintBackground(r, true);
        e.Handled = true;
      }
    }

		private void ImplementMembersForm_Load(object sender, EventArgs e)
		{

		}



    private void pbImplement_Click(object sender, EventArgs e)
    {
      var res = _grid.Rows.Cast<DataGridViewRow>().Where(r => r.Tag != null)
        .GroupBy(r => ((TypeMembers)r.Tag).Key, r => 
          new 
          {
            Type       = ((TypeMembers)r.Tag).Key,
            Member     = ((TypeMembers)r.Tag).Value, 
            Explicit   = (bool)r.Cells["Explicit"].Value,
            AccessMods = (string)r.Cells["AccessMods"].Value,
            ImplName   = (string)r.Cells["ImplName"].Value
          });

      var res2   = res.ToArray();
      var writer = new System.IO.StringWriter();

      foreach (var item in res2)
      {
        writer.WriteLine("//" + item.Key + ":");

        foreach (var item2 in item)
        {
          Nemerle.Compiler.Utils.Utils.GenerateMemberImplementation(writer,
            _source.FileIndex, item.Key, item2.Member, item2.Explicit, item2.AccessMods, item2.ImplName);

          writer.WriteLine();
        }
      }

      Debug.WriteLine(writer.GetStringBuilder().Replace("\t", "  ").ToString());
    }
	}
}
