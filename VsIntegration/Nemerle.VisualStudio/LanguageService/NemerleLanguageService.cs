using System;
using System.Collections.Generic;
using System.Diagnostics;
//using System.Drawing;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using System.Linq;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Compiler;
using Nemerle.Compiler.Parsetree;
using Nemerle.Completion2;
using Nemerle.Compiler.Utils.Async;

using Nemerle.VisualStudio.Project;
using WpfHint;
using Color = System.Drawing.Color;
using VsShell = Microsoft.VisualStudio.Shell.VsShellUtilities;
using Nemerle.VisualStudio.GUI;
using Nemerle.Utility;
using AstUtils = Nemerle.Compiler.Utils.AstUtils;

using Nemerle.VisualStudio.Properties;
using Microsoft.VisualStudio.Package;
using System.Windows;

// ReSharper disable LocalizableElement
namespace Nemerle.VisualStudio.LanguageService
{
	///<summary>
	/// This is the base class for a language service that supplies language features including syntax highlighting, brace matching, auto-completion, IntelliSense support, and code snippet expansion.
	///</summary>
	[Guid(NemerleConstants.LanguageServiceGuidString)]
	public class NemerleLanguageService : Microsoft.VisualStudio.Package.LanguageService
	{
		#region Fields

		public bool IsSmartTagActive { get; private set; }
		public static IIdeEngine DefaultEngine { get; private set; }
		public bool IsDisposed { get; private set; }
		IVsStatusbar _statusbar;
		public NemerlePackage Package { get; private set; }
    IVsSmartTagTipWindow _smartTagWin;
		public bool ContextMenuActive { get; set; }

		#endregion

		#region Init

		public NemerleLanguageService(NemerlePackage package)
		{
			Debug.Assert(package != null, "package != null");
			Package = package;

			if (System.Threading.Thread.CurrentThread.Name == null)
				System.Threading.Thread.CurrentThread.Name = "UI Thread";

			CompiledUnitAstBrowser.ShowLocation += GotoLocation;
			AstToolControl.ShowLocation += GotoLocation;

			if (DefaultEngine == null)
			{
				try { DefaultEngine = EngineFactory.Create(EngineCallbackStub.Default, new TraceWriter(), true); }
				catch (Exception ex)
				{
					Debug.WriteLine(ex.Message);
				}
			}

			Hint = new Hint();
			Hint.WrapWidth = 900.1;
		}

		///<summary>
		///Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
		///</summary>
		public override void Dispose()
		{
			IsDisposed = true;
			try
			{
				AsyncWorker.Stop();

				AbortBackgroundParse();

				foreach (NemerleColorizer colorizer in _colorizers.Values)
					colorizer.Dispose();

				_colorizers.Clear();

				if (_preferences != null)
				{
					_preferences.Dispose();
					_preferences = null;
				}
			}
			finally
			{
				base.Dispose();
			}
		}

		#endregion

		#region SmartTags Support

		internal void ShowSmartTag(IVsTextView textView, bool showMenu, Location loc, Guid cmdSet, MenuCmd.CmdId contextMenuId, Action<MenuCmd.CmdId> exec, Func<MenuCmd.CmdId, uint> queryStatus)
		{
			ErrorHelper.ThrowIsNull(exec, "exec");

			var smartTagWin = GetSmartTagTipWindow();
			var source = (NemerleSource)GetSource(textView);
			Debug.Assert(source != null, "source == null");

			var pos = source.GetPositionOfLineIndex(loc.Line, loc.Column);
			var endPos = source.GetPositionOfLineIndex(loc.EndLine, loc.EndColumn);
			var length = endPos - pos;

			var smartTagData = new NemerleSmartTagData(pos, length, cmdSet, exec, queryStatus,
				() => IsSmartTagActive = false);
			ErrorHelper.ThrowOnFailure(smartTagWin.SetSmartTagData(smartTagData));

      var shouContextMenu = showMenu ? (uint)TipWindowFlags2.UTW_EXPANDED : 0;

			var viewEx = (IVsTextViewEx)textView;
			ErrorHelper.ThrowOnFailure(viewEx.UpdateSmartTagWindow(smartTagWin, shouContextMenu));

			IsSmartTagActive = true;
		}

		internal void ShowSmartTag(IVsTextView textView, bool showMenu, Location loc, MenuCmd.CmdId contextMenuId, Action<MenuCmd.CmdId> exec, Func<MenuCmd.CmdId, uint> queryStatus)
		{
			ShowSmartTag(textView, showMenu, loc, MenuCmd.guidNemerleProjectCmdSet, contextMenuId, exec, queryStatus);
		}

		internal void ShowSmartTag(IVsTextView textView, bool showMenu, Location loc, MenuCmd.CmdId contextMenuId, Action<MenuCmd.CmdId> exec)
		{
			ShowSmartTag(textView, showMenu, loc, MenuCmd.guidNemerleProjectCmdSet, contextMenuId, exec, null);
		}

		internal void HideSmartTag(IVsTextView textView)
		{
			var smartTagWin = GetSmartTagTipWindow();
			var viewEx = (IVsTextViewEx)textView;
			ErrorHelper.ThrowOnFailure(viewEx.UpdateSmartTagWindow(smartTagWin, 
				(uint)TipWindowFlags.UTW_DISMISS));
		}

		IVsSmartTagTipWindow GetSmartTagTipWindow()
    {
			NemerlePackage pkg = Package;

      if (_smartTagWin == null)
      {
        Type typeSTagWin = typeof(VsSmartTagTipWindowClass);
        Guid clsidSTagWin = typeSTagWin.GUID;
        Guid iidSTagWin = typeof(IVsSmartTagTipWindow).GUID;
        _smartTagWin = (IVsSmartTagTipWindow)pkg.CreateInstance(ref clsidSTagWin, ref iidSTagWin, typeSTagWin);
        Debug.Assert(_smartTagWin != null);
      }

      return _smartTagWin;
    }

    #endregion

		#region Misc

		public bool IsDefaultEngine(IIdeEngine engine)
		{
			return engine == DefaultEngine;
		}

		#endregion

		#region ParseSource

		public override AuthoringScope ParseSource(ParseRequest request)
		{
			return null; // At now we not use Microsoft implementation of parse thread!
		}

		#endregion

		#region Colorizing

		// This array contains the definition of the colorable items provided by
		// this language service.
		// This specific language does not really need to provide colorable items
		// because it does not define any item different from the default ones,
		// but the base class has an empty implementation of
		// IVsProvideColorableItems, so any language service that derives from
		// it must implement the methods of this interface, otherwise there are
		// errors when the shell loads an editor to show a file associated to
		// this language.
		private static readonly NemerleColorableItem[] _colorableItems = 
		{
			// The sequential order of these items should be consistent with the ScanTokenColor enum.
			//
			new NemerleColorableItem("Keyword",				  COLORINDEX.CI_BLUE),
			new NemerleColorableItem("Comment",				  COLORINDEX.CI_DARKGREEN),
			new NemerleColorableItem("Identifier"),
			new NemerleColorableItem("String",				   COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("Number"),
			new NemerleColorableItem("Text"),

			new NemerleColorableItem("Operator"),
			new NemerleColorableItem("Preprocessor Keyword",	 COLORINDEX.CI_BLUE,   Color.FromArgb(  0, 51, 204)),
			new NemerleColorableItem("StringEx",				 COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),
			new NemerleColorableItem("String (@ Verbatim)",   2, COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("StringEx (@ Verbatim)", 2, COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),

			new NemerleColorableItem("User Types",			   COLORINDEX.CI_CYAN,   Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("User Types (Delegates)",   COLORINDEX.CI_CYAN,   Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("User Types (Enums)",	   COLORINDEX.CI_CYAN,   Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("User Types (Interfaces)",  COLORINDEX.CI_CYAN,   Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("User Types (Value types)", COLORINDEX.CI_CYAN,   Color.FromArgb(43, 145, 175)),

			new NemerleColorableItem("Quotation",			 0, COLORINDEX.CI_BROWN),

			new NemerleColorableItem("<[ Text ]>",			0),
			new NemerleColorableItem("<[ Keyword ]>",		 0, COLORINDEX.CI_BLUE),
			new NemerleColorableItem("<[ Comment ]>",		 0, COLORINDEX.CI_DARKGREEN),
			new NemerleColorableItem("<[ Identifier ]>",	  0),
			new NemerleColorableItem("<[ String ]>",		  0, COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("<[ Number ]>",		  0),
			new NemerleColorableItem("<[ Operator ]>",		0),
			new NemerleColorableItem("<[ StringEx ]>",		0, COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),
			new NemerleColorableItem("<[ String (@) ]>",	  1, COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("<[ StringEx (@) ]>",	1, COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),

			new NemerleColorableItem("<[ User Types ]>",			   0, COLORINDEX.CI_CYAN, Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("<[ User Types (Delegates) ]>",   0, COLORINDEX.CI_CYAN, Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("<[ User Types (Enums) ]>",	   0, COLORINDEX.CI_CYAN, Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("<[ User Types (Interfaces) ]>",  0, COLORINDEX.CI_CYAN, Color.FromArgb(43, 145, 175)),
			new NemerleColorableItem("<[ User Types (Value types) ]>", 0, COLORINDEX.CI_CYAN, Color.FromArgb(43, 145, 175)),

			new NemerleColorableItem("Highlight One", COLORINDEX.CI_BLACK, COLORINDEX.CI_YELLOW, Color.Empty, Color.FromArgb(135, 206, 250)),
			new NemerleColorableItem("Highlight Two", COLORINDEX.CI_BLACK, COLORINDEX.CI_GREEN, Color.Empty, Color.FromArgb(255, 182, 193)),

			new NemerleColorableItem("TODO comment", COLORINDEX.CI_BLUE, Color.FromArgb(0,  175, 255)),
			new NemerleColorableItem("BUG comment",  COLORINDEX.CI_RED,  Color.FromArgb(255, 75,  75), FONTFLAGS.FF_BOLD),
			new NemerleColorableItem("HACK comment", COLORINDEX.CI_RED,  Color.FromArgb(145,  0,   0)),

			new NemerleColorableItem("<[ TODO comment ]>", 0, COLORINDEX.CI_BLUE, Color.FromArgb(0,  175, 255)),
			new NemerleColorableItem("<[ BUG comment ]>",  0, COLORINDEX.CI_RED,  Color.FromArgb(255, 75,  75), FONTFLAGS.FF_BOLD),
			new NemerleColorableItem("<[ HACK comment ]>", 0, COLORINDEX.CI_RED,  Color.FromArgb(145,  0,   0)),

			new NemerleColorableItem("String (<# #>)",		 2, COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("StringEx (<# #>)",	   2, COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),
			new NemerleColorableItem("<[ String (<# #>) ]>",   1, COLORINDEX.CI_MAROON, Color.FromArgb(170,  0,   0)),
			new NemerleColorableItem("<[ StringEx (<# #>) ]>", 1, COLORINDEX.CI_MAROON, Color.FromArgb(143, 44, 182)),
			
			new NemerleColorableItem("Field Identifier", COLORINDEX.CI_DARKBLUE, Color.FromArgb(128, 0, 128)),
			new NemerleColorableItem("Event Identifier", COLORINDEX.CI_MAGENTA, Color.FromArgb(255, 0, 255)),
			new NemerleColorableItem("Method Identifier", COLORINDEX.CI_DARKBLUE, Color.FromArgb(0, 139, 139)),
			new NemerleColorableItem("Property Identifier", COLORINDEX.CI_DARKBLUE, Color.FromArgb(128, 0, 128)),

		};

		readonly Dictionary<IVsTextLines, NemerleColorizer> _colorizers = new Dictionary<IVsTextLines, NemerleColorizer>();

		public void DisposeColorizer(IVsTextLines buffer)
		{
			if (_colorizers.ContainsKey(buffer))
				_colorizers.Remove(buffer);
		}

		public override Colorizer GetColorizer(IVsTextLines buffer)
		{
			NemerleColorizer colorizer;

			if (!_colorizers.TryGetValue(buffer, out colorizer))
			{
				colorizer = new NemerleColorizer(this, buffer, (NemerleScanner)GetScanner(buffer));

				_colorizers.Add(buffer, colorizer);
			}

			return colorizer;
		}

		public override IScanner GetScanner(IVsTextLines buffer)
		{
			return new NemerleScanner(this, buffer);
		}

		// Implementation of IVsProvideColorableItems.
		//
		public override int GetItemCount(out int count)
		{
			count = _colorableItems.Length;
			return VSConstants.S_OK;
		}

		public override int GetColorableItem(int index, out IVsColorableItem item)
		{
			if (index < 1)
				throw new ArgumentOutOfRangeException("index");

			item = _colorableItems[index - 1];

			return VSConstants.S_OK;
		}

		#endregion

		#region Source

		public override string Name
		{
			get { return Resources.Nemerle; }
		}

		public override Source CreateSource(IVsTextLines buffer)
		{
			return new NemerleSource(this, buffer, GetColorizer(buffer));
		}

		public override CodeWindowManager CreateCodeWindowManager(IVsCodeWindow codeWindow, Source source)
		{
			CodeWindowManager m = base.CreateCodeWindowManager(codeWindow, source);
			return m;
		}

		#endregion

		#region Snippets

		private int classNameCounter = 0;

		public override ExpansionFunction CreateExpansionFunction(ExpansionProvider provider, string functionName)
		{
			ExpansionFunction function = null;

			if (functionName == "GetName")
			{
				++classNameCounter;
				function = new NemerleGetNameExpansionFunction(provider, classNameCounter);
			}

			return function;
		}

		private List<VsExpansion> _expansionsList;
		private List<VsExpansion> ExpansionsList
		{
			get
			{
				if (_expansionsList != null)
					return _expansionsList;

				GetSnippets();
				return _expansionsList;
			}
		}

		// Disable the "DoNotPassTypesByReference" warning.
		//
		public void AddSnippets(ref NemerleDeclarations declarations)
		{
			if (null == ExpansionsList)
				return;

			foreach (VsExpansion expansionInfo in ExpansionsList)
			{
				//declarations.AddDeclaration(new Declaration(expansionInfo));
				throw new NotImplementedException();
			}
		}

		private void GetSnippets()
		{
			if (null == _expansionsList)
				_expansionsList = new List<VsExpansion>();
			else
				_expansionsList.Clear();

			IVsTextManager2 textManager =
				Microsoft.VisualStudio.Shell.Package.GetGlobalService(
				typeof(SVsTextManager)) as IVsTextManager2;

			if (textManager == null)
				return;

			SnippetsEnumerator enumerator = new SnippetsEnumerator(
				textManager, GetLanguageServiceGuid());

			foreach (VsExpansion expansion in enumerator)
				if (!string.IsNullOrEmpty(expansion.shortcut))
					_expansionsList.Add(expansion);
		}

		internal class NemerleGetNameExpansionFunction : ExpansionFunction
		{
			private int nameCount;

			public NemerleGetNameExpansionFunction(ExpansionProvider provider, int counter)
				: base(provider)
			{
				nameCount = counter;
			}

			public override string GetCurrentValue()
			{
				string name = "MyClass";
				name += nameCount.ToString(CultureInfo.InvariantCulture);
				return name;
			}
		}

		#endregion

		#region Navigation DropDown

		public override TypeAndMemberDropdownBars CreateDropDownHelper(IVsTextView forView)
		{
			if (Preferences.ShowNavigationBar)
				return new NemerleTypeAndMemberDropdownBars(this, forView);
			else
				return null;
		}

		/// <summary>Update current file index, current line & col in Engine</summary>
		/// <param name="line">0 - based</param>
		/// <param name="col">0 - based</param>
		void UpdateViewInfo(IVsTextView textView, int line, int col)
		{
			if (textView != null)
			{
				var source = GetSource(textView) as NemerleSource;

				if (source != null)
				{
					if (line >= 0 && col >= 0 || !ErrorHandler.Failed(textView.GetCaretPos(out line, out col)))
						source.GetEngine().SetTextCursorLocation(source.FileIndex, line + 1, col + 1);
				}
			}
		}

		public override void OnActiveViewChanged(IVsTextView textView)
		{
			UpdateViewInfo(textView, -1, -1);
			base.OnActiveViewChanged(textView);
		}

		public override void SynchronizeDropdowns()
		{
			IVsTextView view = LastActiveTextView;
			if (view != null)
				SynchronizeDropdowns(view);
		}

		public void SynchronizeDropdowns(IVsTextView view)
		{
			var mgr = GetCodeWindowManagerForView(view);
			if (mgr == null || mgr.DropDownHelper == null)
				return;

			var dropDownHelper = (NemerleTypeAndMemberDropdownBars)mgr.DropDownHelper;
			int line = -1, col = -1;
			if (!ErrorHandler.Failed(view.GetCaretPos(out line, out col)))
				dropDownHelper.SynchronizeDropdownsRsdn(view, line, col);
		}

		/// <include file='doc\LanguageService.uex' path='docs/doc[@for="LanguageService.OnCaretMoved"]/*' />
		/// ѕереопредел€ем этот метод, чтобы вызвать SynchronizeDropdowns у 
		/// NemerleTypeAndMemberDropdownBars, а не CodeWindowManager, а так же обновить 
		/// информацию в Engine о активном файле и позиции в нем. Ёто требуетс€ дл€ ускорени€
		/// типизации методов с которыми в данный момент взаимодействует пользователь.
		public override void OnCaretMoved(CodeWindowManager mgr, IVsTextView textView, int line, int col)
		{
			if (mgr.DropDownHelper != null)
			{
				var dropDownHelper = (NemerleTypeAndMemberDropdownBars)mgr.DropDownHelper;
				dropDownHelper.SynchronizeDropdownsRsdn(textView, line, col);
				UpdateViewInfo(textView, line, col);
			}
		}

		#endregion

		#region LanguagePreferences

		LanguagePreferences _preferences;

		public override LanguagePreferences GetLanguagePreferences()
		{
			if (_preferences == null)
			{
				_preferences = new LanguagePreferences(Site, typeof(NemerleLanguageService).GUID, Name);

				// Setup default values.
				_preferences.ShowNavigationBar = true;
				_preferences.EnableFormatSelection = true;
				_preferences.IndentStyle = IndentingStyle.Smart;

				// Load from the registry.
				_preferences.Init();
			}

			return _preferences;
		}

		#endregion

		#region Debugging

		#region IVsLanguageDebugInfo methods

		public override int GetLocationOfName(string name, out string pbstrMkDoc, TextSpan[] spans)
		{
			pbstrMkDoc = null;
			return NativeMethods.E_NOTIMPL;
		}

		public override int GetNameOfLocation(
			IVsTextBuffer buffer,
			int line,
			int col,
			out string name,
			out int lineOffset)
		{
			name = null;
			lineOffset = 0;
			/*
		 TRACE1( "LanguageService(%S)::GetNameOfLocation", m_languageName );
		OUTARG(lineOffset);
		OUTARG(name);
		INARG(textBuffer);

		HRESULT hr;
		IScope* scope = NULL;
		hr = GetScopeFromBuffer( textBuffer, &scope );
		if (FAILED(hr)) return hr;
  
		long realLine = line;
		hr = scope->Narrow( line, idx, name, &realLine );
		RELEASE(scope);
		if (hr != S_OK) return hr;

		*lineOffset = line - realLine;
		return S_OK;
	  */
			return NativeMethods.S_OK;
		}

		public override int GetProximityExpressions(
			IVsTextBuffer buffer,
			int line,
			int col,
			int cLines,
			out IVsEnumBSTR ppEnum)
		{
			ppEnum = null;
			/*
		TRACE2( "LanguageService(%S)::GetProximityExpressions: line %i", m_languageName, line );
		OUTARG(exprs);
		INARG(textBuffer);

		//check the linecount
		if (lineCount <= 0) lineCount = 1;

		//get the source 
		//TODO: this only works for sources that are opened in the environment
		HRESULT hr;
		Source* source = NULL;
		hr = GetSource( textBuffer, &source );
		if (FAILED(hr)) return hr;

		//parse and find the proximity expressions
		StringList* strings = NULL;
		hr = source->GetAutos( line, line + lineCount, &strings );
		RELEASE(source);
		if (FAILED(hr)) return hr;

		hr = strings->QueryInterface( IID_IVsEnumBSTR, reinterpret_cast<void**>(exprs) );
		RELEASE(strings);
		if (FAILED(hr)) return hr;
  
		return S_OK;
	  */
			return NativeMethods.S_FALSE;
		}

		public override int IsMappedLocation(IVsTextBuffer buffer, int line, int col)
		{
			return NativeMethods.S_FALSE;
		}

		public override int ResolveName(string name, uint flags, out IVsEnumDebugName ppNames)
		{
			ppNames = null;
			return NativeMethods.E_NOTIMPL;
		}

		public override int ValidateBreakpointLocation(
			IVsTextBuffer buffer,
			int line,
			int col,
			TextSpan[] pCodeSpan
		)
		{
			if (pCodeSpan != null)
			{
				pCodeSpan[0].iStartLine = line;
				pCodeSpan[0].iStartIndex = col;
				pCodeSpan[0].iEndLine = line;
				pCodeSpan[0].iEndIndex = col;

				if (buffer != null)
				{
					int length;

					buffer.GetLengthOfLine(line, out length);

					pCodeSpan[0].iStartIndex = 0;
					pCodeSpan[0].iEndIndex = length;
				}

				return VSConstants.S_OK;
			}
			else
			{
				return VSConstants.S_FALSE;
			}
		}

		#endregion

		public override ViewFilter CreateViewFilter(CodeWindowManager mgr, IVsTextView newView)
		{
			// This call makes sure debugging events can be received by our view filter.
			//
			GetIVsDebugger();
			return new NemerleViewFilter(mgr, newView);
		}

		#endregion

		#region OnIdle

		public override void OnIdle(bool periodic)
		{
			if (IsDisposed)
				return;

			foreach (var prj in ProjectInfo.Projects)
			{
				prj.Engine.OnIdle();
				prj.ProcessDelayedMethodCompilerMessages();
			}

			if (periodic)
			{
				var maxTime = TimeSpan.FromSeconds(0.05);
				var timer = Stopwatch.StartNew();

				AsyncWorker.DispatchResponses();

				while (timer.Elapsed < maxTime && AsyncWorker.DoSynchronously())
					;
			}
			//if (LastActiveTextView == null)
			//  return;

			//Source src = GetSource(LastActiveTextView);

			//if (src != null && src.LastParseTime == int.MaxValue)
			//  src.LastParseTime = 0;

			SynchronizeDropdowns();

			//base.OnIdle(periodic);
		}

		#endregion

		#region Filter List

		public override string GetFormatFilterList()
		{
			return Resources.NemerleFormatFilter;
		}

		#endregion

		#region ShowLocation event handler

		public void GotoLocation(Location loc)
		{
			GotoLocation(loc, null, false);
		}

		public void GotoLocation(Location loc, string caption, bool asReadonly)
		{
			//TODO: VladD2: –азобратьс€ почему этот код вызывает вылет
			//IVsUIShell uiShell = this.GetService(typeof(SVsUIShell)) as IVsUIShell;
			//if (uiShell != null)
			//{
			//  IVsWindowFrame frame;
			//  string data;
			//  object unknown;
			//  ErrorHandler.ThrowOnFailure(uiShell.GetCurrentBFNavigationItem(out frame, out data, out unknown));
			//  ErrorHandler.ThrowOnFailure(uiShell.AddNewBFNavigationItem(frame, data, unknown, 0));
			//}

			TextSpan span = new TextSpan();

			span.iStartLine = loc.Line - 1;
			span.iStartIndex = loc.Column - 1;
			span.iEndLine = loc.EndLine - 1;
			span.iEndIndex = loc.EndColumn - 1;

			uint itemID;
			IVsUIHierarchy hierarchy;
			IVsWindowFrame docFrame;
			IVsTextView textView;

			if (loc.FileIndex == 0)
				return;
			
			VsShell.OpenDocument(Site, loc.File, VSConstants.LOGVIEWID_Code, 
				out hierarchy, out itemID, out docFrame, out textView);

			if (asReadonly)
			{
				IVsTextLines buffer;
				ErrorHandler.ThrowOnFailure(textView.GetBuffer(out buffer));
				IVsTextStream stream = (IVsTextStream)buffer;
				stream.SetStateFlags((uint)BUFFERSTATEFLAGS.BSF_USER_READONLY);
			}

			if (caption != null)
				ErrorHandler.ThrowOnFailure(docFrame.SetProperty((int)__VSFPROPID.VSFPROPID_OwnerCaption, caption));

			ErrorHandler.ThrowOnFailure(docFrame.Show());

			if (textView != null && loc.Line != 0)
			{
				try
				{
					ErrorHandler.ThrowOnFailure(textView.SetCaretPos(span.iStartLine, span.iStartIndex));
					TextSpanHelper.MakePositive(ref span);
					ErrorHandler.ThrowOnFailure(textView.SetSelection(span.iStartLine, span.iStartIndex, span.iEndLine, span.iEndIndex));
					ErrorHandler.ThrowOnFailure(textView.EnsureSpanVisible(span));
				}
				catch (Exception ex)
				{
					Trace.WriteLine(ex.Message);
				}
			}
		}

		#endregion

		#region StatusBar

		public void SetStatusBarText(string text)
		{
			if (_statusbar == null)
				_statusbar = (IVsStatusbar)GetService(typeof(SVsStatusbar));

			if (_statusbar != null)
				_statusbar.SetText(text);
		}

		#endregion

		#region IVsTipWindow Members

		public Hint Hint { get; private set; }

		public bool ShowHint(IVsTextView view, TextSpan hintSpan, Func<string, string> getHintContent, string hintText)
		{
			if (ContextMenuActive)
				return false;

	    var hWnd = view.GetWindowHandle();

	    int lineHeight;
      ErrorHelper.ThrowOnFailure(view.GetLineHeight(out lineHeight));
      
	    var ptStart = new Microsoft.VisualStudio.OLE.Interop.POINT[1];
	    var ptEnd   = new Microsoft.VisualStudio.OLE.Interop.POINT[1];
	    ErrorHelper.ThrowOnFailure(view.GetPointOfLineColumn(hintSpan.iStartLine, hintSpan.iStartIndex, ptStart));
	    ErrorHelper.ThrowOnFailure(view.GetPointOfLineColumn(hintSpan.iEndLine, hintSpan.iEndIndex, ptEnd));

      NemerleNativeMethods.ClientToScreen(hWnd, ref ptStart[0]);
      NemerleNativeMethods.ClientToScreen(hWnd, ref ptEnd[0]);

      var hintXml = "<hint>" + hintText.Replace("<unknown>", "&lt;unknown&gt;").Replace("\r", "")
        .Replace("\n", "<lb/>") + "</hint>";

      var rect = new Rect(new Point(ptStart[0].x, ptStart[0].y), new Point(ptEnd[0].x, ptEnd[0].y + lineHeight));

      if (Hint.IsOpen)
      {
        if (Hint.PlacementRect == rect)
        {
          Hint.Text = hintXml;
	        return true;
        }

        Hint.Close();
      }

      Hint.Show(hWnd, rect, getHintContent, hintXml);

	    return true;
	  }

    #endregion
  }
}
