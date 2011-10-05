using System;
using System.Linq;
using System.Collections;
using System.Collections.Generic;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Completion2;
using Nemerle.Compiler;
using Nemerle.Compiler.Parsetree;
//using Nemerle.Compiler.Utils;

using VsPkg = Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio;
using System.Windows.Forms;

namespace Nemerle.VisualStudio.LanguageService
{
	class NemerleTypeAndMemberDropdownBars : TypeAndMemberDropdownBars
	{
		public NemerleTypeAndMemberDropdownBars(NemerleLanguageService langService, IVsTextView forView)
			: base(null)
		{
			_languageService = langService;
			_source = (NemerleSource)langService.GetSource(forView);
		}

		NemerleSource _source;
		int _lastSelectedType = -2;
		int _lastSelectedMember = -2;

		#region MS vars

		/// <summary>The language service object that created this object and calls its SynchronizeDropdownsRsdn method</summary>
		private NemerleLanguageService _languageService;

		/// <summary>The correspoding VS object that represents the two drop down bars. The VS object uses call backs to pull information from
		/// this object and makes itself known to this object by calling SetDropdownBar</summary>
		private IVsDropdownBar _dropDownBar;

		/// <summary>The icons that prefix the type names and member signatures</summary>
		private ImageList _imageList;

		/// <summary>The current text editor window</summary>
		private IVsTextView _textView;

		/// <summary>The list of types that appear in the type drop down list.</summary>
		private TopDeclaration[] _dropDownTypes = new TopDeclaration[]{};

		/// <summary>The list of types that appear in the member drop down list. </summary>
		private List<ClassMember> _dropDownMembers = new List<ClassMember>();

		const int DropClasses = 0;
		const int DropMethods = 1;

		#endregion

		#region overrides

		public override bool OnSynchronizeDropdowns(VsPkg.LanguageService languageService,
			IVsTextView textView, int line, int col, ArrayList dropDownTypes,
			ArrayList dropDownMembers, ref int selectedType, ref int selectedMember)
		{
			throw new NotImplementedException("This method can't be invoked");
		}

		// IVsDropdownBarClient methods
		/// <include file='doc\CodeWindowManager.uex' path='docs/doc[@for="TypeAndMemberDropdownBars.GetComboAttributes"]/*' />
		public override int GetComboAttributes(int combo, out uint entries, out uint entryType, out IntPtr iList)
		{
			entries = 0;
			entryType = 0;
			if (combo == DropClasses && _dropDownTypes != null)
				entries = (uint)_dropDownTypes.Length;
			else if (_dropDownMembers != null)
				entries = (uint)_dropDownMembers.Count;
			entryType = (uint)(DropDownItemType.HasText | DropDownItemType.HasFontAttribute | DropDownItemType.HasImage);
			if (_imageList == null)
				_imageList = _languageService.GetImageList();
			iList = _imageList.Handle;

			return NativeMethods.S_OK;
		}

		private enum DropDownItemType
		{
			HasText = 1,
			HasFontAttribute = 2,
			HasImage = 4
		}

		/// <include file='doc\CodeWindowManager.uex' path='docs/doc[@for="TypeAndMemberDropdownBars.GetComboTipText"]/*' />
		public override int GetComboTipText(int combo, out string text)
		{
			text = combo == DropClasses ? "Selected type" : "Selected member";
			return NativeMethods.S_OK;
		}

		/// <include file='doc\CodeWindowManager.uex' path='docs/doc[@for="TypeAndMemberDropdownBars.GetEntryAttributes"]/*' />
		public override int GetEntryAttributes(int combo, int entry, out uint fontAttrs)
		{
			if (combo == DropClasses)
				fontAttrs = GetAttributes(GetType(entry));
			else
				fontAttrs = GetAttributes(GetMember(entry));

			return NativeMethods.S_OK;
		}

		private uint GetAttributes(ClassMember classMember)
		{
			uint attr = (uint)DROPDOWNFONTATTR.FONTATTR_PLAIN; //TODO: Implement it

			if (classMember == null)
				return attr;

			if ((classMember.Attributes & NemerleModifiers.Static) != 0)
				attr |= (uint)DROPDOWNFONTATTR.FONTATTR_ITALIC;
			return attr;
		}

		private uint GetAttributes(TopDeclaration topDeclaration)
		{
			uint attr = (uint)DROPDOWNFONTATTR.FONTATTR_PLAIN; //TODO: Implement it

			if (topDeclaration == null)
				return attr;

			if ((topDeclaration.Attributes & NemerleModifiers.Static) != 0)
				attr |= (uint)DROPDOWNFONTATTR.FONTATTR_ITALIC;

			if (topDeclaration is TopDeclaration.VariantOption)
				attr |= (uint)DROPDOWNFONTATTR.FONTATTR_BOLD;

			return attr;
		}

		/// <include file='doc\CodeWindowManager.uex' path='docs/doc[@for="TypeAndMemberDropdownBars.GetEntryImage"]/*' />
		public override int GetEntryImage(int combo, int entry, out int imgIndex)
		{
			// this happens during drawing and has to be fast 
			imgIndex = -1;

			if (entry < 0)
				return NativeMethods.S_OK;

			if (combo == DropClasses)
				imgIndex = Utils.GetGlyph(GetType(entry));
			else
				imgIndex = Utils.GetGlyph(GetMember(entry));
		
			return NativeMethods.S_OK;
		}

		/// <include file='doc\CodeWindowManager.uex' path='docs/doc[@for="TypeAndMemberDropdownBars.GetEntryText"]/*' />
		public override int GetEntryText(int combo, int entry, out string text)
		{
			text = null;

			if (entry < 0)
				return NativeMethods.S_OK;

			if (combo == DropClasses)
				text = GetLable(GetType(entry));
			else
				text = GetLable(GetMember(entry));

			return NativeMethods.S_OK;
		}

		private string GetLable(TopDeclaration topDeclaration)
		{
			if (topDeclaration == null)
				return "";

			return topDeclaration.Name + " (" + topDeclaration.GetLabel() + ")";
		}

		private string GetLable(ClassMember classMember)
		{
			if (classMember == null)
				return "";

			return classMember.GetLabel();
		}

		/// <include file='doc\CodeWindowManager.uex' path='docs/doc[@for="TypeAndMemberDropdownBars.OnComboGetFocus"]/*' />
		public override int OnComboGetFocus(int combo)
		{
			return NativeMethods.S_OK;
		}


		public ClassMember GetMember(int entryIndex)
		{
			if (entryIndex < 0 || entryIndex >= _dropDownMembers.Count)
				return null;

			return _dropDownMembers[entryIndex];
		}

		public TopDeclaration GetType(int entryIndex)
		{
			if (entryIndex < 0 || entryIndex >= _dropDownTypes.Length)
				return null;

			return _dropDownTypes[entryIndex];
		}

		public Located GetEntry(int combo, int entryIndex)
		{
			if (combo == DropClasses)
				return GetType(entryIndex);

			return GetMember(entryIndex);
		}

		/// <include file='doc\CodeWindowManager.uex' path='docs/doc[@for="TypeAndMemberDropdownBars.OnItemChosen"]/*' />
		public override int OnItemChosen(int combo, int entryIndex)
		{
			var entry = GetEntry(combo, entryIndex);

			if (entry != null && _textView != null)
			{
				int line = entry.Location.Line.ToVsLineCoord();
				int col = entry.Location.Column.ToVsColCoord();
				try
				{
					// Here we don't want to throw or to check the return value.
					_textView.CenterLines(line, 16);
				}
				catch (System.Runtime.InteropServices.COMException) { }
				ErrorHandler.ThrowOnFailure(_textView.SetCaretPos(line, col));
				SetFocus(_textView.GetWindowHandle());
				SynchronizeDropdownsRsdn(_textView, line, col);
			}
			return NativeMethods.S_OK;
		}

		/// <include file='doc\CodeWindowManager.uex' path='docs/doc[@for="TypeAndMemberDropdownBars.SetFocus"]/*' />
		[System.Runtime.InteropServices.DllImport("user32.dll")]
		static extern void SetFocus(IntPtr hwnd);

		/// <include file='doc\CodeWindowManager.uex' path='docs/doc[@for="TypeAndMemberDropdownBars.OnItemSelected"]/*' />
		public override int OnItemSelected(int combo, int index)
		{
			//nop
			return NativeMethods.S_OK;
		}

		/// <include file='doc\CodeWindowManager.uex' path='docs/doc[@for="TypeAndMemberDropdownBars.SetDropdownBar"]/*' />
		public override int SetDropdownBar(IVsDropdownBar bar)
		{
			_dropDownBar = bar;
			return NativeMethods.S_OK;
		}

		#endregion

		/// <summary>
		/// Ётот метод замен€ет SynchronizeDropdowns который скрыт внутри TypeAndMemberDropdownBars.
		/// «амена требуетс€, так как в TypeAndMemberDropdownBars захардкожено обращение
		/// к DropDownMember в котором реализована кривые (рекурсивные) оператора сравнени€
		/// привод€щие к переполнению стека при попытке проверить объект этого класса на null.
		/// </summary>
		public void SynchronizeDropdownsRsdn(IVsTextView textView, int line, int col)
		{
			if (_dropDownBar == null)
				return;

			_textView = textView;

			line = line.ToNccLineCoord();
			col  =  col.ToNccColCoord();

			if (UpdateDropDownTypes() | SyncSelectedType(line, col) | SyncSelectedMember(line, col))
			{
				ErrorHandler.ThrowOnFailure(_dropDownBar.RefreshCombo(DropClasses, _lastSelectedType   < 0 ? -1 : _lastSelectedType));
				ErrorHandler.ThrowOnFailure(_dropDownBar.RefreshCombo(DropMethods, _lastSelectedMember < 0 ? -1 : _lastSelectedMember));
			}
		}

		private bool UpdateDropDownTypes()
		{
			var decls = _source.Declarations;

			if (decls == null)
			{
				//_source.BeginParseTopDeclaration();
				return false;
			}

			if (object.ReferenceEquals(decls, _dropDownTypes))
				return false;

			System.Diagnostics.Debug.WriteLine("Drop Down Types updated!");
			_dropDownMembers.Clear();
			_dropDownTypes      = decls;
			_lastSelectedType   = -2;
			_lastSelectedMember = -2;

			return true;
		}

		/// <summary>
		/// ≈сли исходный файл был изменен, производит обновление списка типов.
		/// ≈сли список не пуст, производит, так же, обновление списка членов.
		/// ƒелает выделенным элемент в списке типов на котором в данный момент 
		/// находитс€ курсор.
		/// ¬озвращает:
		/// 1. »ндекс типа (index => 0), если курсор находитс€ на внутри типа.
		/// 2. -1, если курсор находитс€ вне типа.
		/// </summary>
		/// <param name="selectedType"></param>
		/// <returns></returns>
		private bool SyncSelectedType(int line, int col)
		{
			int selType = GetSelectedTypeIndex(line, col);

			if (_lastSelectedType != selType)
			{
				/*
				if (_lastSelectedType == -1)
					foreach (var m in _dropDownTypes)
						m.FontAttr &= ~DROPDOWNFONTATTR.FONTATTR_GRAY;
				else if (selType == -1)
					foreach (var m in _dropDownTypes)
						m.FontAttr |= DROPDOWNFONTATTR.FONTATTR_GRAY;
				*/

				_lastSelectedType = selType;
				_lastSelectedMember = -2;

				if (_dropDownMembers.Count > 0)
					_dropDownMembers.Clear();

				if (selType >= 0 && selType < _dropDownTypes.Length)
				{
					_dropDownMembers.AddRange(_dropDownTypes[selType].GetMembers()
						.OrderBy(m => m.Name));
				}

				return true;
			}

			return false;
		}

		private bool SyncSelectedMember(int line, int col)
		{
			int selectedType = _lastSelectedType;

			if (selectedType < 0 || selectedType >= _dropDownTypes.Length)
				return false;

			//var members = _dropDownTypes[selectedType].GetMembers();

			int selMember = _lastSelectedType < 0 ? -1 : GetSelectedMemberIndex(line, col);

			if (_lastSelectedMember != selMember)
			{
				/*
				if (_lastSelectedMember == -2)
					foreach (var m in members)
						m.FontAttr = DROPDOWNFONTATTR.FONTATTR_PLAIN;

				if (_lastSelectedMember == -1)
				{
					foreach (var m in members)
						m.FontAttr &= ~DROPDOWNFONTATTR.FONTATTR_GRAY;
				}
				else if (selMember == -1)
				{
					foreach (var m in members)
						m.FontAttr |= DROPDOWNFONTATTR.FONTATTR_GRAY;
				}
				*/
				
				_lastSelectedMember = selMember;
				return true;
			}

			return false;
		}

		int GetSelectedTypeIndex(int line, int col)
		{
			int idx = -1;
			TopDeclaration lastMember = null;

			for (int i = 0; i < _dropDownTypes.Length; i++)
			{
				var member = _dropDownTypes[i];

				if (member.Location.Contains(line, col))
					if (lastMember == null || member.Location.IsNestedIn(lastMember.Location))
					{
						idx = i;
						lastMember = member;
					}
			}

			return idx;
		}

		private int GetSelectedMemberIndex(int line, int col)
		{
			int idx = -1;
			var members = _dropDownMembers;

			for (int i = 0; i < members.Count; i++)
			{
				var member = members[i];

				if (member.Location.Contains(line, col))
					idx = i;
			}

			return idx;
		}

		static bool IsIn(TextSpan span, int line, int col)
		{
			return
				(line > span.iStartLine || line == span.iStartLine && col >= span.iStartIndex) &&
				(line < span.iEndLine || line == span.iEndLine && col <= span.iEndIndex);
		}
	}
}

