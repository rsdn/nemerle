using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Nemerle.Completion2;
using Nemerle.VisualStudio.LanguageService;
using Nemerle.Compiler;
using System.Diagnostics;
using NUtils = Nemerle.Compiler.Utils.Utils;
using TypeMembers = System.Collections.Generic.KeyValuePair<Nemerle.Compiler.FixedType.Class, Nemerle.Compiler.IMember>;
using Microsoft.VisualStudio.Package;

namespace Nemerle.VisualStudio.GUI
{
	// ReSharper disable LocalizableElement
	// ReSharper disable PossibleNullReferenceException
	public partial class ImplementMembersForm : Form
	{
		readonly NemerleSource _source;
		readonly TypeBuilder _ty;
		readonly IEnumerable<IGrouping<FixedType.Class, IMember>> _unimplementedMembers;
		readonly int _imageSize;

		class MemberImplInfo
		{
			public readonly IMember Member;
			public readonly bool Explicit;
			public readonly string AccessModifier;
			public readonly string ImplementationName;


			public MemberImplInfo(IMember member, bool @explicit, string accessModifier, string implementationName)
			{
				Member = member;
				Explicit = @explicit;
				AccessModifier = accessModifier;
				ImplementationName = implementationName;
			}
		}

		public ImplementMembersForm(NemerleSource source, TypeBuilder ty, IEnumerable<IGrouping<FixedType.Class, IMember>> unimplementedMembers)
		{
			_source               = source;
			_ty                   = ty;
			_unimplementedMembers = unimplementedMembers;
			
			InitializeComponent();

			#region Init events hendlers

			_grid.CellPainting += CellPainting;
			_grid.CellValueChanged += CellValueChanged;
			_grid.CellValidating += CellValidating;
			_grid.CurrentCellDirtyStateChanged += CurrentCellDirtyStateChanged;
			
			#endregion
			#region Init ImageList

			imageList1.Images.AddStrip(Resources.SO_TreeViewIcons);
			_imageSize = imageList1.ImageSize.Width;
			Debug.Assert(imageList1.ImageSize.Width == imageList1.ImageSize.Height);
			
			#endregion

			if (_unimplementedMembers == null)
				return;

			FillTable(MakeTypeMembersMap());
		}

		/// <summary>
		/// Групирует члены по типам в которых они объявлены и возвращает результат.
		/// При этом типы являются подстановочными типами (а не просто TypeInf), что позволяет
		/// создать для них корректные реализации (с верными значениями параметров типов).
		/// </summary>
		private Dictionary<FixedType.Class, IMember[]> MakeTypeMembersMap()
		{
			//var implItfs = _ty.GetDirectSuperTypes().Where(t => t.IsInterface);
			//var types = _unimplementedMembers.GroupBy(m => m.DeclaringType);
			//var res = implItfs.Join(types, t => t.tycon, itf => itf.Key, (t, itf) => new { Group = itf, Ty = t });

			//var ht = new Dictionary<FixedType.Class, IMember[]>();
			//foreach (var item in res)
			//  ht[item.Ty] = ReplaceGettersAndSettersByProperties(item.Group);

			//var baseTypes = types.Where(g => !g.Key.IsInterface);

			//foreach (var baseType in baseTypes)
			//  ht[baseType.Key.GetMemType()] = ReplaceGettersAndSettersByProperties(baseType);

			var ht = new Dictionary<FixedType.Class, IMember[]>();

			foreach (var item in _unimplementedMembers)
				ht[item.Key] = ReplaceGettersAndSettersByProperties(item);

			return ht;
		}

		void FillTable(Dictionary<FixedType.Class, IMember[]> typeMembersesMap)
		{
			var accessModaCol = AccessModifierColumn();
			var explicitCol   = ExplicitColumn();
			var implName      = ImplementationNameColumn();

			accessModaCol.Items.AddRange("public", "private", "protected", "internal", "protected internal");
			//_grid.Rows.Add("All", true);

			if (!HaveInterfaces(typeMembersesMap))
			{
				implName.Visible = explicitCol.Visible = accessModaCol.Visible = false;
				Text = "Override members of base types";
			}

			foreach (var item in typeMembersesMap)
			{
				var isInterface = item.Key.IsInterface;
				var sifix = isInterface ? " (interface)" : " (base type)";
				var rowIndex = _grid.Rows.Add(item.Key + sifix, true);
				var row = _grid.Rows[rowIndex];
				row.Cells[0].Style.Font = new Font(_grid.DefaultCellStyle.Font, FontStyle.Bold);

				foreach (var m in item.Value)
				{
					var name = m.Name;
					rowIndex = _grid.Rows.Add(name, true, false, null, name, m);
					row = _grid.Rows[rowIndex];
					var implementByDefault = isInterface || (m.Attributes & NemerleModifiers.Abstract) != 0;
					ImplementCell(row).Value = implementByDefault;
					row.Cells[0].Style.Padding = new Padding(_imageSize * 2, 0, 0, 0);

					var gray = Color.FromKnownColor(KnownColor.GrayText);

					AccessModifierCell    (row).Style.ForeColor = gray;
					ImplementationNameCell(row).Style.ForeColor = gray;
					SignatureCell         (row).Style.ForeColor = gray;
					row.Tag = new TypeMembers(item.Key, m);
				}
			}
		}

		private static bool HaveInterfaces(Dictionary<FixedType.Class, IMember[]> typeMembersesMap)
		{
			return typeMembersesMap.Any(x => x.Key.IsInterface);
		}

		//private static IMember[] ReplaceGettersAndSettersByProperties(IGrouping<TypeInfo, IMember> item)
		//{
		//  var props = item.Key.GetMembers().OfType<IProperty>();
		//  var mems1 = item.Select(m => props.SingleOrDefault(p => p.GetGetter() == m || p.GetSetter() == m) ?? m);
		//  var mems2 = mems1.Distinct().OrderBy(m => m.Name).ToArray();
		//  return mems2;
		//}

		private static IMember[] ReplaceGettersAndSettersByProperties(IGrouping<FixedType.Class, IMember> item)
		{
			var props = item.Key.tycon.GetMembers().OfType<IProperty>().ToArray();
			var mems1 = item.Select(m => props.SingleOrDefault(p => object.Equals(p.GetGetter(), m) || object.Equals(p.GetSetter(), m)) ?? m).ToArray();
			var mems2 = mems1.Distinct().OrderBy(m => m.Name).ToArray();
			return mems2;
		}

		void CurrentCellDirtyStateChanged(object sender, EventArgs e)
		{
			if (_grid.IsCurrentCellDirty)
				_grid.CommitEdit(DataGridViewDataErrorContexts.Commit);
		}

// ReSharper disable MemberCanBeMadeStatic.Local
		void CellValidating(object sender, DataGridViewCellValidatingEventArgs e)
		{
		}
// ReSharper restore MemberCanBeMadeStatic.Local

		bool IsExplisitImplColumn(int colIndex)
		{
			var name = _grid.Columns[colIndex].Name;
			return name == "AccessModifier" || name == "ImplementationName";
		}

		void CellValueChanged(object sender, DataGridViewCellEventArgs e)
		{
			var row = _grid.Rows[e.RowIndex];

			if (row.Tag == null)
				return;

			if (IsExplisitImplColumn(e.ColumnIndex))
					ExplicitCell(row).Value = true;

			var isImpl    = ImplementCellValue(row);
			var isExplicit = ExplicitCellValue(row);

			ExplicitCell(row).Style.ForeColor = Color.FromKnownColor(isImpl ? KnownColor.WindowText : KnownColor.GrayText);

			var color = Color.FromKnownColor(isImpl && isExplicit ? KnownColor.WindowText : KnownColor.GrayText);
			AccessModifierCell(row).Style.ForeColor = color;
			ImplementationNameCell(row).Style.ForeColor   = color;
			SignatureCell(row).Style.ForeColor  = color;
		
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
			InsertStabsIntoSource();
		}

		private void InsertStabsIntoSource()
		{
			var methodStubInfos = ReadMethodStubInfosFromDataGrideView().ToArray();
			var laggSrv         = _source.LanguageService;
			var pref            = laggSrv.Preferences;
			var sufix           = methodStubInfos.Length > 1 ? "s" : "";
			var editArray       = new EditArray(_source, null, true, "implement interface" + sufix + " stub" + sufix);

			_source.LockWrite();
			try
			{
				MakeChanges(methodStubInfos, pref, editArray);
				editArray.ApplyEdits();
				Close();
			}
			catch (Exception ex) { _source.ProjectInfo.ShowMessage("Error: " + ex.Message, MessageType.Error); }
			finally
			{
				editArray.Dispose();
				_source.UnlockWrite();
			}
		}

		private IEnumerable<IGrouping<FixedType.Class, MemberImplInfo>> ReadMethodStubInfosFromDataGrideView()
		{
			return _grid.Rows.Cast<DataGridViewRow>()
				.Where(r => r.Tag != null && (bool)ImplementCell(r).Value)
				.GroupBy(r => ((TypeMembers)r.Tag).Key, r =>
																								new MemberImplInfo(
																									((TypeMembers)r.Tag).Value, 
																									ExplicitCellValue(r),
																									AccessModifierCellValue(r),
																									ImplementationNameCellValue(r)
																									));
		}

// ReSharper disable ParameterTypeCanBeEnumerable.Local
		private void MakeChanges(IGrouping<FixedType.Class, MemberImplInfo>[] stubs, LanguagePreferences pref, EditArray editArray)
// ReSharper restore ParameterTypeCanBeEnumerable.Local
		{
			var newLine = Environment.NewLine;

			foreach (var stub in stubs)
			{
				var sb = MakeStubsForTypeMembers(stub);

				// На данном этапе в "sb" находится текст заглушек для членов тела которых отбиты одной табуляцией на отступ.
				// Заменяем этот табы на отступ указанный в настройках студии для Nemerle.

				// Кроме того члены не имеют отсупа от левого края. Отступ должен совпадать с отступом
				// типа в который помещаются плюс один отступ.

				// Кроме того пользователю будет удобно если добавляемые члены будут добавлены после 
				// последнего члена того же (т.е. типа чьи члены реализуются) типа уже имеющегося в данном типе.
				// Таким образом мы должны попытаться найти уже реализованные типы. В них найти самый послединй,
				// и вставить новые члены после него. Если в текущем типе (_ty) еще не было реализовано членов
				// подтипа (например, интерфейса) к которому относятся добавляемые члены, то производим вставку
				// в конец текущего типа.

				TextPoint pt;
				int indentCount;

				var lastImplementedMembers = NUtils.GetLastImplementedMembersOfInterface(_ty, stub.Key);

				#region Calc indent and insertion point

				if (lastImplementedMembers.IsSome)
				{
					// Используем meber.Value для получения места вставки
					var endLine = lastImplementedMembers.Value.Location.EndLine;
					var text = _source.GetLine(endLine);
					indentCount = NUtils.CalcIndentVisiblePosition(text, pref.IndentSize) / pref.IndentSize;
					pt = new TextPoint(endLine + 1, 1);
					//TODO: Этот код рассчитывает на то, что за членом не идет многострочного коментария
					// или другого члена. Надо бы сделать реализацию не закладывающуюся на это.
				}
				else // Ни одного члена этого интерфейса не реализовано в классе...
				{
					// Оборачиваем реализуемые методы в #region
					if (_cbGenerateRegion.Checked)
					{
						sb.Insert(0, "#region " + stub.Key + "  Members" + newLine + newLine);
						sb.AppendLine("#endregion" + newLine);
					}
					// Вставляем описание интерфейса в конец класса
					var endLine = _ty.Location.EndLine;
					var text = _source.GetLine(endLine);
					indentCount = (NUtils.CalcIndentVisiblePosition(text, pref.TabSize) + pref.IndentSize) / pref.IndentSize;
					pt = new TextPoint(endLine, 1);
					//TODO: Этот код рассчитывает на то, что конец типа распологается на отдельной строке.
					// Надо бы сделать реализацию не закладывающуюся на это.
				}
				
				#endregion

				var indent = new string('\t', indentCount);
				sb.Insert(0, indent);
				sb.Replace("\n", "\n" + indent);
				sb = NUtils.NormalizeIndent(sb.ToString(), pref.InsertTabs, pref.IndentSize, pref.TabSize);
				TrimEnd(sb);

				var inertLoc = new Location(_source.FileIndex, pt, pt);
				editArray.Add(new EditSpan(inertLoc.ToTextSpan(), sb.ToString()));
			}
		}

		private StringBuilder MakeStubsForTypeMembers(IGrouping<FixedType.Class, MemberImplInfo> stubInfos)
		{
			var writer = new System.IO.StringWriter();
			var members = stubInfos.ToArray();
			var isInterface = stubInfos.Key.IsInterface;
			var ty = isInterface ? stubInfos.Key : _ty.GetMemType();

			foreach (var memberImplInfo in members)
			{
				var member = memberImplInfo.Member;
				var @explicit = memberImplInfo.Explicit;
				var accessModifier = memberImplInfo.AccessModifier;
				var implementationName = memberImplInfo.ImplementationName;

				if (isInterface)
					NUtils.GenerateMemberImplementation(writer, _source.FileIndex,
																							ty, member, @explicit, accessModifier, implementationName,
																							_cbGenerateXmlDoc.Checked);
				else
				{
					// Генерируем override-методы. Для них нужно сформировать правильны модификатор доступа.
					var am = (member.Attributes | NemerleModifiers.Override)
									 & ~(NemerleModifiers.Abstract | NemerleModifiers.Virtual | NemerleModifiers.SpecialName);
					var acessMods = am.ToString().ToLower().Replace(",", "");

					NUtils.GenerateMemberImplementation(writer, _source.FileIndex,
																							ty, member, false, acessMods, "",
																							_cbGenerateXmlDoc.Checked);
				}

				writer.WriteLine(); // разделяем члены пустой строкой
			}

			return writer.GetStringBuilder();
		}

		#region Utils
		
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
		
		private DataGridViewTextBoxColumn ImplementationNameColumn()
		{
			return (DataGridViewTextBoxColumn)_grid.Columns["ImplementationName"];
		}

		private DataGridViewCheckBoxColumn ExplicitColumn()
		{
			return (DataGridViewCheckBoxColumn)_grid.Columns["Explicit"];
		}

		private DataGridViewComboBoxColumn AccessModifierColumn()
		{
			return (DataGridViewComboBoxColumn)_grid.Columns["AccessModifier"];
		}

		private static DataGridViewCell SignatureCell(DataGridViewRow row)
		{
			return row.Cells["Signature"];
		}

		private static string SignatureCellValue(DataGridViewRow row)
		{
			return (string)SignatureCell(row).Value;
		}

		private static DataGridViewCell ImplementationNameCell(DataGridViewRow row)
		{
			return row.Cells["ImplementationName"];
		}

		private static string ImplementationNameCellValue(DataGridViewRow row)
		{
			return (string)ImplementationNameCell(row).Value;
		}

		private static DataGridViewCell AccessModifierCell(DataGridViewRow row)
		{
			return row.Cells["AccessModifier"];
		}

		private static string AccessModifierCellValue(DataGridViewRow row)
		{
			return (string)AccessModifierCell(row).Value;
		}

		private static DataGridViewCell ImplementCell(DataGridViewRow row)
		{
			return row.Cells["Implement"];
		}

		private static bool ImplementCellValue(DataGridViewRow row)
		{
			return (bool)ImplementCell(row).Value;
		}

		private static DataGridViewCell ExplicitCell(DataGridViewRow row)
		{
			return row.Cells["Explicit"];
		}

		private static bool ExplicitCellValue(DataGridViewRow row)
		{
			return (bool)ExplicitCell(row).Value;
		}

		#endregion
	}
}
