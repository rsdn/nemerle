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
using NUtils = Nemerle.Compiler.Utils.Utils;
using BFld = System.Reflection.BindingFlags;
using TypeMembers = System.Collections.Generic.KeyValuePair<Nemerle.Compiler.MType.Class, Nemerle.Compiler.IMember>;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;

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

			var baseTypes = itfs.Where(g => !g.Key.IsInterface);
			var tbTy = ty.GetMemType();

			foreach (var baseType in baseTypes)
				ht[baseType.Key.GetMemType()] = ReplaceGettersAndSettersByProperties(baseType);

      FillTable(ht);
		}

    void FillTable(Dictionary<MType.Class, IMember[]> itfs)
		{
      _grid.CellPainting                 += CellPainting;
      _grid.CellValueChanged             += CellValueChanged;
      _grid.CellValidating               += CellValidating;
      _grid.CurrentCellDirtyStateChanged += CurrentCellDirtyStateChanged;
      
      var accessModaCol = (DataGridViewComboBoxColumn)_grid.Columns["AccessMods"];
			var explicitCol   = (DataGridViewCheckBoxColumn)_grid.Columns["Explicit"];
			var implName      = (DataGridViewTextBoxColumn) _grid.Columns["ImplName"];

			accessModaCol.Items.AddRange("public", "private", "protected", "internal", "protected internal");
			_grid.Rows.Add("All", true);

			var haveInterfaces = itfs.Any(x => x.Key.IsInterface);

			if (!haveInterfaces)
			{
				implName.Visible = explicitCol.Visible = accessModaCol.Visible = false;
				Text = "Override members of base types";
			}

			foreach (var item in itfs)
			{
				var isInterface = item.Key.IsInterface;
				var sifix = isInterface ? " (interface)" : " (base type)";
				var rowIndex = _grid.Rows.Add(item.Key + sifix, true);
				var row = _grid.Rows[rowIndex];
				row.Cells[0].Style.Font = new Font(_grid.DefaultCellStyle.Font, FontStyle.Bold);

        foreach (var m in item.Value)
				{
          var name = m.Name;
          rowIndex = _grid.Rows.Add(m.Name, true, false, null, m.Name, m);
					row = _grid.Rows[rowIndex];
					var implementByDefault = isInterface || (m.Attributes & NemerleAttributes.Abstract) != 0;
					row.Cells["AddImplCol"].Value = implementByDefault;
					row.Cells[0].Style.Padding = new Padding(_imageSize * 2, 0, 0, 0);
          var gray = Color.FromKnownColor(KnownColor.GrayText);
          var explicitCell = (DataGridViewCheckBoxCell)row.Cells["Explicit"];
          
          row.Cells["AccessMods"].Style.ForeColor = gray;
          row.Cells["ImplName"].Style.ForeColor = gray;
          row.Cells["Signature"].Style.ForeColor = gray;
					row.Tag = new TypeMembers(item.Key, m);
				}
			}
		}

    private static IMember[] ReplaceGettersAndSettersByProperties(IGrouping<TypeInfo, IMember> item)
    {
      var props = item.Key.GetMembers().OfType<IProperty>();
      var mems1 = item.Select(m => props.SingleOrDefault(p => p.GetGetter() == m || p.GetSetter() == m) ?? m);
      var mems2 = mems1.Distinct().OrderBy(m => m.Name).ToArray();
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
      row.Cells["ImplName"].Style.ForeColor   = color;
      row.Cells["Signature"].Style.ForeColor  = color;
    
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
				var imgIndex = NUtils.GetGlyphIndex(member);

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
			var newLine = Environment.NewLine;

			var res = _grid.Rows.Cast<DataGridViewRow>()
				.Where(r => r.Tag != null && (bool)r.Cells["AddImplCol"].Value)
        .GroupBy(r => ((TypeMembers)r.Tag).Key, r => 
          new 
          {
            Type       = ((TypeMembers)r.Tag).Key,
            Member     = ((TypeMembers)r.Tag).Value, 
            Explicit   = (bool)r.Cells["Explicit"].Value,
            AccessMods = (string)r.Cells["AccessMods"].Value,
            ImplName   = (string)r.Cells["ImplName"].Value
          });

      var stubs    = res.ToArray();
			var laggSrv = _source.LanguageService;
			var pref    = laggSrv.Preferences;

			_source.LockWrite();
			var sufix = stubs.Length > 1 ? "s" : "";
			var editArray = new EditArray(_source, null, true, "implement interface" + sufix + " stub" + sufix);
			try
			{

				foreach (var stub in stubs)
				{
					var writer = new System.IO.StringWriter();
					var itfMems = stub.ToArray();
					var isInterface = stub.Key.IsInterface;
					var ty = _ty.GetMemType();

					foreach (var item2 in itfMems)
					{
						if (isInterface)
							NUtils.GenerateMemberImplementation(writer, _source.FileIndex,
								stub.Key, item2.Member, item2.Explicit, item2.AccessMods, item2.ImplName);
						else
						{
							var am = (item2.Member.Attributes | NemerleAttributes.Override)
								& ~(NemerleAttributes.Abstract | NemerleAttributes.Virtual);
							var acessMods = am.ToString().ToLower().Replace(",", "");

							NUtils.GenerateMemberImplementation(writer, _source.FileIndex,
								ty, item2.Member, false, acessMods, "");
						}

						writer.WriteLine();
					}

					var sb = writer.GetStringBuilder();
					//sb.Length -= Environment.NewLine.Length;
					if (!pref.InsertTabs && pref.IndentSize == 1)
						sb.Replace("\t", pref.MakeIndentString());

					TextPoint pt = new TextPoint();
					string indent = null;

					var member = NUtils.GetLastImplementedMembersOfInterface(_ty, stub.Key);

					if (member.IsSome)
					{
						// Используем meber.Value для получения места вставки
						var endLine = member.Value.Location.EndLine;
						var text = _source.GetLine(endLine);
						indent = text.GetLiadingSpaces();
						pt = new TextPoint(endLine + 1, 1);
						//TODO: Этот код рассчитывает на то, что за членом не идет многострочного коментария
						// или другого члена. Надо бы сделать реализацию не закладывающуюся на это.
					}
					else // Ни одного члена этого интерфейса не реализовано в классе...
					{
						// Оборачиваем реализуемые методы в #region
						sb.Insert(0, "#region " + stub.Key + "  Members" + newLine + newLine);
						sb.AppendLine("#endregion " + stub.Key + "  Members" + newLine);
						// Вставляем описание интерфейса в конец класса
						var endLine = _ty.Location.EndLine;
						var text = _source.GetLine(endLine);
						indent = text.GetLiadingSpaces();
						pt = new TextPoint(endLine, 1);
						indent += pref.MakeIndentString();
						//TODO: Этот код рассчитывает на то, что конец типа распологается на отдельной строке.
						// Надо бы сделать реализацию не закладывающуюся на это.
					}

					sb.Insert(0, indent);
					sb.Replace("\n", "\n" + indent);
					TrimEnd(sb);

					Location inertLoc = new Location(_source.FileIndex, pt, pt);
					TextSpan span = inertLoc.ToTextSpan();

					editArray.Add(new EditSpan(span, sb.ToString()));
					//_source.SetText(span, sb.ToString());
				}

				editArray.ApplyEdits();
				Close();
			}
			catch (Exception ex)
			{
				_source.ProjectInfo.ShowMessage("Error: " + ex.Message, Nemerle.Completion2.MessageType.Error);
			}
			finally
			{
				editArray.Dispose();
				_source.UnlockWrite();
			}

      //Debug.WriteLine(writer.GetStringBuilder().Replace("\t", "  ").ToString());
    }

		static void TrimEnd(StringBuilder sb)
		{
			for (int i = sb.Length - 1; i > 0; i--)
			{
				var ch = sb[i];
				if (ch != ' ' && ch != '\t')
				{
					sb.Length = i + 1;
					return;
				}
			}
		}
	}
}
