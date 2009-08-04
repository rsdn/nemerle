using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio;

using Nemerle.Compiler;
using Nemerle.Completion2;
using Nemerle.Completion2.CodeFormatting;
using Nemerle.VisualStudio.Project;

using System.Collections.Generic;
//using System.Collections;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System;

using VsCommands2K      = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;
using TopDeclaration    = Nemerle.Compiler.Parsetree.TopDeclaration;
using TupleIntInt       = Nemerle.Builtins.Tuple<int, int>;
using TupleStringInt    = Nemerle.Builtins.Tuple<string, int>;
using TupleStringIntInt = Nemerle.Builtins.Tuple<string, int, int>;


namespace Nemerle.VisualStudio.LanguageService
{
	public delegate AuthoringScope ScopeCreatorCallback(ParseRequest request);

	public class NemerleSource : Source, ISource
	{
		#region Init

		public NemerleSource(NemerleLanguageService service, IVsTextLines textLines, Colorizer colorizer)
			: base(service, textLines, colorizer)
		{
			string path = GetFilePath();

			Service = service;
			ProjectInfo = ProjectInfo.FindProject(path);

			if (ProjectInfo != null)
			{
				ProjectInfo.AddSource(this);
				ProjectInfo.MakeCompilerMessagesTextMarkers(textLines, FileIndex);
			}

			Scanner = colorizer.Scanner as NemerleScanner;
			
			if (Scanner != null)
				Scanner._source = this;
			LastDirtyTime = DateTime.Now;
		}

		#endregion

		#region Properties

		public     DateTime               LastDirtyTime { get; private set; }
		public new DateTime               LastParseTime { get; private set; }
		public     NemerleLanguageService Service       { get; private set; }
		public     NemerleScanner         Scanner       { get; private set; }
		public     ScopeCreatorCallback   ScopeCreator  { get;         set; }
		public     ProjectInfo            ProjectInfo   { get; private set; }
		public     MethodData             MethodData    { get; private set; }
		public     int                    TimeStamp     { get; private set; }
		internal   TopDeclaration[]       Declarations  { get;         set; }
    public     bool                   RegionsLoaded { get;         set; }
    public     CompileUnit            CompileUnit   { get;         set; }
		public     int                    FileIndex
		{
			get
			{
				if (_fileIndex <= 0)
				{
					var path = GetFilePath();

					if (path.IsNullOrEmpty())
						return -1;

					_fileIndex = Location.GetFileIndex(path);
				}

				return _fileIndex;
			}
		}
		public     IVsTextLines           TextLines
		{
			get { return GetTextLines(); }
		}

		#endregion

		#region Fields

		MemberBuilder _resetedMember;
		int           _fileIndex = -1;

		#endregion

		#region OnChangeLineText

		string _text;

		public override void OnChangeLineText(TextLineChange[] lineChange, int last)
		{
			var oldLastDirtyTime = LastDirtyTime;
			base.OnChangeLineText(lineChange, last);
			TimeStamp++;
			var timer = Stopwatch.StartNew();
			_text = GetText();

			//Debug.WriteLine("GetText() took: " + timer.Elapsed); timer.Reset(); timer.Start();

			//var engine = GetEngine();
			//var lexer = new LexerString(engine, _text, new Location(FileIndex, 1, 1, 1, 1));
			//var preparser = new PreParser(lexer, engine.CoreEnv);
			//var tokens = preparser.PreParse();
			//_tokens = tokens;

			//var x = tokens;
			//Token tok;
			//do
			//{
			//  tok = lex.GetToken();
			//  _tokens.Add(tok);
			//}
			//while (!(tok is Token.EndOfFile));

			//Debug.WriteLine("PreParse() took: " + timer.Elapsed);
			
			GetEngine().BeginUpdateCompileUnit(this);

			//TODO: Сюда нужно вставить код обновляющий локейшоны в дереве типов если редактирование
			// происходит внутри выражений и обнулять дерево типов в обратоном случае.
			// Если дерево типов нет, то ничего не делаем.

			string      fileName    = GetFilePath();
			ProjectInfo projectInfo = ProjectInfo.FindProject(fileName);

			if (projectInfo == null || !projectInfo.IsProjectAvailable)
				return;

			TextLineChange changes = lineChange[0];
			var resetedMember = projectInfo.AddRelocation(
			  fileName,
			  changes.iNewEndIndex + 1,
			  changes.iNewEndLine  + 1,
			  changes.iOldEndIndex + 1,
			  changes.iOldEndLine  + 1,
			  changes.iStartIndex  + 1,
			  changes.iStartLine   + 1);

			if (resetedMember != null)
			{
				var method = resetedMember as Nemerle.Completion2.Factories.IntelliSenseModeMethodBuilder;
				if (method != null)
					projectInfo.Engine.AddMethodAtFirstCheckQueue(method);
			}
			_resetedMember = resetedMember;

			if (Scanner != null && Scanner.GetLexer().ClearHoverHighlights())
			{
				int lineCount;
				GetTextLines().GetLineCount(out lineCount);
				Recolorize(1, lineCount);
			}
		}

		#endregion

		#region Overrides

		public override bool IsDirty
		{
			get { return base.IsDirty; }
			set
			{
				Debug.WriteLine("IsDirty = " + value);
				base.IsDirty = value;
				if (value)
					LastDirtyTime = System.DateTime.Now;
				//else
				//	LastParseTime = System.DateTime.Now;
			}
		}

		public override void OnIdle(bool periodic)
		{
			//var completionSetIsDisplayed = CompletionSet != null && CompletionSet.IsDisplayed;
			//var methodDataIsDisplayed    = MethodData != null && MethodData.IsDisplayed;
			//var projectInfo              = ProjectInfo.FindProject(GetFilePath());
			//var engine                   = projectInfo == null ? null : projectInfo.Engine;
			//var reparseDelta             = TimeSpan.FromMilliseconds(LanguageService.Preferences.CodeSenseDelay);

			//if (engine != null && !completionSetIsDisplayed && !methodDataIsDisplayed)
			//{
			//  //VladD2: Произсодим парсинг и построение дерева типв если это необходимо и прошло 
			//  //        некоторое время.
			//  if (periodic && !engine.IsProjectAvailable && engine.LastResetAstTime >= LastParseTime)
			//  {
			//    var delta = System.DateTime.Now - engine.LastResetAstTime;

			//    //var isAsyncCompletion = LanguageService.Preferences.EnableAsyncCompletion;
			//    if (delta > reparseDelta)
			//    {
			//      //TryBuildTypesTreeAsync();
			//      return;
			//    }
			//  }
			//}

			//// Kick of a background parse, but only in the periodic intervals
			//if (Service == null || Service.LastActiveTextView == null)
			//  return;

			//if (IsDirty && LastDirtyTime >= LastParseTime)
			//{
			//  var delta = DateTime.Now - LastDirtyTime;

			//  if (delta > reparseDelta) // нам надо перепарсить исходник!
			//  {
			//    // Don't do background parsing while intellisense completion is going on
			//    if (completionSetIsDisplayed || methodDataIsDisplayed)
			//      return;

			//    if (engine == null || !engine.IsProjectAvailable)
			//      return;

			//    // А здесь нам нужно парсить метод? код которого изменился за это время, чтобы
			//    // отображались ошибки внесенные пользователем до прошлого парсинга.
			//    if (!Service.IsParsing)
			//    {
			//      if (_resetedMember != null)
			//      {
			//        LastParseTime = DateTime.Now;
			//        BeginParse(0, 0, new TokenInfo(), (ParseReason)ParseReason2.CheckRelocatedMember,
			//          GetView(), this.HandleParseResponse);
			//        return;
			//      }
			//    }
			//  }
			//}

			//if (engine == null || !engine.IsProjectAvailable)
			//  return;

			//if (!projectInfo.IsMethodsCheckQueueEmpty && !Service.IsParsing)
			//{
			//  Debug.WriteLine("BeginParse(ParseReason.Check)" + DateTime.Now.TimeOfDay);
			//  int line, idx;
			//  IVsTextView view = GetView();
			//  view.GetCaretPos(out line, out idx);
			//  BeginParse(line, idx, new TokenInfo(), ParseReason.Check,
			//    view, new ParseResultHandler(this.HandleParseResponse));
			//  return;
			//}
		}

		public override bool OutliningEnabled
		{
			get { return base.OutliningEnabled; }
			set { base.OutliningEnabled = value; }
		}

		internal void CheckRelocatedMember()
		{
			try
			{
				var methodBuilder = _resetedMember as Nemerle.Completion2.Factories.IntelliSenseModeMethodBuilder;

				if (methodBuilder != null)
					methodBuilder.EnsureCompiled();
				else
				{
					var fieldBuilder = _resetedMember as FieldBuilder;
					if (fieldBuilder != null)
					{
						// здесь нужно разбираться с инициализаторами полей
						fieldBuilder.EnsureCompiled();
					}
				}
				//BeginParse();
			}
			finally { _resetedMember = null; }
		}

		public IVsTextView GetView()
		{
			if (Service == null)
				throw new ArgumentNullException("Service", "The Service property of NemerleSource is null!");

			return Service.GetPrimaryViewForSource(this);
		}

    public override ParseRequest BeginParse(int line, int idx, TokenInfo info, ParseReason reason, IVsTextView view, ParseResultHandler callback)
    {
      //return base.BeginParse(line, idx, info, reason, view, callback);
			switch (reason)
			{
				case ParseReason.Autos:break;
				case ParseReason.Check:break;
				case ParseReason.CodeSpan:break;
				case ParseReason.CompleteWord:break;
				case ParseReason.DisplayMemberList:break;
				case ParseReason.Goto:break;
				case ParseReason.MemberSelect:break;
				case ParseReason.MemberSelectAndHighlightBraces:break;
				case ParseReason.MethodTip:break;
				case ParseReason.None:break;
				case ParseReason.QuickInfo:break;
				case ParseReason.HighlightBraces:
				case ParseReason.MatchBraces:
					if (Service != null)
					{
						string text = this.GetText();
						string fname = this.GetFilePath();
						ParseRequest request = this.LanguageService.CreateParseRequest(this, line, idx, info, text, fname, reason, view);
						request.Callback = callback;
						request.Timestamp = this.ChangeCount;
						request.DirtySpan = this.DirtySpan;
						request.Scope = Service.MatchBraces(request);
						callback(request);
						return request;
					}
					break;
				default:break;
			}
      Debug.WriteLine("Soutce.BeginParse: " + reason.ToString());
      return null;
    }

		public override void OnChangesCommitted(uint reason, TextSpan[] changedArea)
		{
			Debug.WriteLine("OnChangesCommitted");
		}

		public override void Dispose()
		{
			if (ProjectInfo != null)
			{
				ProjectInfo.RemoveSource(this);
				ProjectInfo = null;
			}

			base.Dispose();
		}

		public override string ToString()
		{
			var name = System.IO.Path.GetFileName(GetFilePath());
			if (IsClosed)
				return "NemerleSource: " + name + " (Closed!)";
			else
				return "NemerleSource: " + name;
		}

		public override CommentInfo GetCommentFormat()
		{
			CommentInfo commentInfo = new CommentInfo();

			commentInfo.UseLineComments = true;
			commentInfo.LineStart	   = "//";
			commentInfo.BlockStart	  = "/*";
			commentInfo.BlockEnd		= "*/";

			return commentInfo;
		}

		public override TextSpan CommentLines(TextSpan span, string lineComment)
		{
			// Calculate minimal position of non-space char
			// at lines in selected span.
			var minNonEmptyPosition = 0;
			for (var i = span.iStartLine; i <= span.iEndLine; ++i)
			{
				var line = GetLine(i);
				if (line.Trim().Length <= 0) continue;
				var spaceLen = line.Replace(line.TrimStart(), "").Length;
				if (minNonEmptyPosition == 0 || spaceLen < minNonEmptyPosition)
					minNonEmptyPosition = spaceLen;
			}

			// insert line comment at calculated position.
			var editMgr = new EditArray(this, null, true, "CommentLines");
			for (var i = span.iStartLine; i <= span.iEndLine; ++i)
			{
				var commentSpan = new TextSpan();
				commentSpan.iStartLine = commentSpan.iEndLine = i;
				commentSpan.iStartIndex = commentSpan.iEndIndex = minNonEmptyPosition;
				editMgr.Add(new EditSpan(commentSpan, lineComment));
			}
			editMgr.ApplyEdits();

			// adjust original span to fit comment symbols
			span.iEndIndex += lineComment.Length;
			return span;
		}

		public override AuthoringSink CreateAuthoringSink(ParseReason reason, int line, int col)
		{
			int maxErrors = LanguageService.Preferences.MaxErrorMessages;
			return new NemerleAuthoringSink(this, reason, line, col, maxErrors);
		}

		public override TokenInfo GetTokenInfo(int line, int col)
		{
			//get current line 
			TokenInfo info = new TokenInfo();
			var colorizer = GetColorizer() as NemerleColorizer;

			if (colorizer == null)
				return info;

			colorizer.SetCurrentLine(line);

			//get line info
			TokenInfo[] lineInfo = colorizer.GetLineInfo(this.GetTextLines(), line, this.ColorState);

			if (lineInfo != null)
			{
				//get character info      
				if (col > 0) 
					col--;

				this.GetTokenInfoAt(lineInfo, col, ref info);
			}

			return info;
		}

		public override void ProcessHiddenRegions(System.Collections.ArrayList hiddenRegions)
		{
			// TranslateMe:ss
			//VladD2: Приходится переписывать реализацию от МС, так как она практически не расширяется как нужно нам.
			throw new NotImplementedException();
		}

    class TextSpanEqCmp : IEqualityComparer<TextSpan>
    {
      public bool Equals(TextSpan x, TextSpan y)
      {
        return x.iStartLine == y.iStartLine && x.iEndLine    == y.iEndLine 
            && x.iEndIndex  == y.iEndIndex  && x.iStartIndex == y.iStartIndex;
      }

      public int GetHashCode(TextSpan x)
      {
        return x.iStartLine ^ x.iEndLine ^ x.iEndIndex ^ x.iStartIndex;
      }

      public static TextSpanEqCmp Instance = new TextSpanEqCmp();
    }

		bool _processingOfHiddenRegions;

    public void ProcessHiddenRegions(List<NewHiddenRegion> regions, int sourceVersion)
		{
      if (!this.OutliningEnabled)
				return;

			var timer    = Stopwatch.StartNew();
			var timerAll = Stopwatch.StartNew();

			int added = 0;
			int removed = 0;
			int invalid = 0;

			//Debug.WriteLine("SetRegions: begin               " + timer.Elapsed); timer.Reset(); timer.Start();

			#region Получаем список региотов которые уже есть в редакторе.

			// Регионы в редакторе могут быть
			// по двум причинам:
			// 1. Она билы добавлены предыдущим запуском этого метода.
			// 2. Они были загружены самим редакторм для востановления состояния состояния 
			//    (открыты / закрыты) регионов после открытия файла (обычно случается только 
			//    если файл открывается при открытом Solution).
			//    При этом студия не востанавливает баннеры, так что их приходится обновлять 
			//    (см. коментарий к вызову region.SetBanner()).

			IVsEnumHiddenRegions ppenum = null;
			IVsHiddenTextSession session = GetHiddenTextSession();
			var aspan = new TextSpan[1];
			aspan[0] = GetDocumentSpan();
			ErrorHandler.ThrowOnFailure(session.EnumHiddenRegions((uint)FIND_HIDDEN_REGION_FLAGS.FHR_ALL_REGIONS, HiddenRegionCookie, aspan, out ppenum));
			uint fetched;
			var aregion = new IVsHiddenRegion[1];

			var oldRegionsMap = new Dictionary<TextSpan, IVsHiddenRegion>(TextSpanEqCmp.Instance);

			while (ppenum.Next(1, aregion, out fetched) == NativeMethods.S_OK && fetched == 1)
			{
				var region = aregion[0];
				int regTypeInt;
				ErrorHandler.ThrowOnFailure(region.GetType(out regTypeInt));
				uint dwData;
				region.GetClientData(out dwData);
				HIDDEN_REGION_TYPE regType = (HIDDEN_REGION_TYPE)regTypeInt;
				if (regType != HIDDEN_REGION_TYPE.hrtCollapsible)// || dwData != 0 && dwData != HiddenRegionCookie)
					continue;

				ErrorHandler.ThrowOnFailure(region.GetSpan(aspan));
				TextSpan s = aspan[0];
				var loc = Utils.LocationFromSpan(FileIndex, s);
				oldRegionsMap[s] = region;
			}

			//Debug.WriteLine("SetRegions: old regions fetched " + timer.Elapsed); timer.Reset(); timer.Start();

			#endregion

			// Обновлять регионы нужно на заблокированном для записи редакторе, иначе, если текст
			// изменится во время обновления, то мы получим рассинхронизацию, и как следствие,
			// неопределенное поеведение. По возможности не следует добавлять долгие действия 
			// в блок расположенный ниже, так как обновление идет в GUI-потоке и медленная работа
			// может привести к ощутимому неудобству для пользователя.

			// VS fire ViewFilter.OnChangeScrollInfo event vhen we change regions it lead to many
			// calls of Source.TryHighlightBraces() which can take much time. Prevent it!
			_processingOfHiddenRegions = true;
			LockWrite();
			try
			{
				if (CurrentVersion != sourceVersion)
					return;

				#region Вычисляем регионы которых небыло в редакторе и обновляем баннеры у старых

				var newRegions = new List<NewHiddenRegion>();

				foreach (var rg in regions)
				{
					IVsHiddenRegion region;
					if (oldRegionsMap.TryGetValue(rg.tsHiddenText, out region))
					{
						// Регион с такими же координатами уже был в редактре...

						// Похоже VS, при закрытии файла, запоминает только расположение регионов, 
						// но не их баннеры. Нам нужно обновить баннер если они не совпадают.
						// Кроме того баннеры нужно обновлять когда они физически меняются (пользователем).
						string banner;
						region.GetBanner(out banner);
						if (rg.pszBanner != banner && !(banner == "..." && rg.pszBanner.IsNullOrEmpty()))
							region.SetBanner(rg.pszBanner);
					}
					else
						newRegions.Add(rg); // Регион новый! Запоминаем его...
				}

				//Debug.WriteLine("SetRegions: calc new reg & up b " + timer.Elapsed); timer.Reset(); timer.Start();
				
				#endregion

				#region Вычисляем список устаревших регионов и удаляем их

				var newRegiohsMap = new Dictionary<TextSpan, NewHiddenRegion>(regions.Count);

				// Формируем "мап" новых регионов. .ToDictianary() не применим, так как он 
				// генерирует исключение при попытке добавить элемент с уже существующим ключем.
				foreach (var rg in regions)
					if (newRegiohsMap.ContainsKey(rg.tsHiddenText))
					{ }
					else newRegiohsMap[rg.tsHiddenText] = rg;

				foreach (var rg in oldRegionsMap)
				{
					if (!newRegiohsMap.ContainsKey(rg.Key))
					{ // Чтарый регион не совпадает по местоположению ни с одним из новых. Удаляем его!
						ErrorHandler.ThrowOnFailure(rg.Value.Invalidate((int)CHANGE_HIDDEN_REGION_FLAGS.chrNonUndoable));
						removed++;
					}
				}

				//Debug.WriteLine("SetRegions: bad regions removed " + timer.Elapsed); timer.Reset(); timer.Start();

				#endregion

				#region Добавляем регионы которых не было в редакторе

				int start = Environment.TickCount;

				if (newRegions.Count > 0)
				{
					int count = newRegions.Count;
					// For very large documents this can take a while, so add them in chunks of 
					// 1000 and stop after 5 seconds. 
					int maxTime = this.LanguageService.Preferences.MaxRegionTime;
					int chunkSize = 1000;
					NewHiddenRegion[] chunk = new NewHiddenRegion[chunkSize];
					int i = 0;
					while (i < count && Utils.TimeSince(start) < maxTime)
					{
						int j = 0;
						while (i < count && j < chunkSize)
						{
							NewHiddenRegion r = newRegions[i];
							if (!TextSpanHelper.ValidSpan(this, r.tsHiddenText))
							{
								//Debug.Assert(false, "Invalid span " + r.tsHiddenText.iStartLine + "," + r.tsHiddenText.iStartIndex + "," 
								//                     + r.tsHiddenText.iEndLine + "," + r.tsHiddenText.iEndIndex);
								//break;
								invalid++;
							}
							else
							{
								chunk[j] = r;
								added++;
							}
							i++;
							j++;
						}
						int hr = session.AddHiddenRegions((int)CHANGE_HIDDEN_REGION_FLAGS.chrNonUndoable, j, chunk, null);
						if (ErrorHandler.Failed(hr))
							break; // stop adding if we start getting errors.
					}
				}

				//Debug.WriteLine("SetRegions: new regions added   " + timer.Elapsed); timer.Reset(); timer.Start();

				//Debug.WriteLine("Removed: " + removed + " For add: " + newRegions.Count + " Really added: " + added + " invalid: " + invalid);
				
				#endregion
      }
      finally
      {
        UnlockWrite();
				_processingOfHiddenRegions = false;

        if (ppenum != null)
          Marshal.ReleaseComObject(ppenum);

				//Debug.WriteLine("SetRegions: end (took all)      " + timerAll.Elapsed);
      }

      this.RegionsLoaded = true;
		}
    
		public override void ReformatSpan(EditArray mgr, TextSpan span)
		{
			string filePath = GetFilePath();
			ProjectInfo projectInfo = ProjectInfo.FindProject(filePath);
			Engine engine = projectInfo.Engine;

			ReformatSpan_internal(mgr, span, engine, filePath);
			//ReformatSpan_internal(mgr, span, engine, filePath);
			//ReformatSpan_internal(mgr, span, engine, filePath);
			//base.ReformatSpan(mgr, span);
		}
		private static void ReformatSpan_internal(EditArray _mgr, TextSpan span, Engine engine, string filePath)
		{
			List<FormatterResult> results =
				Formatter.FormatSpan(span.iStartLine + 1,
						   span.iStartIndex + 1,
						   span.iEndLine + 1,
						   span.iEndIndex + 1,
						   engine,
						   filePath);
			EditArray mgr = new EditArray(_mgr.Source, _mgr.TextView, true, "formatting");
			foreach (FormatterResult res in results)
			{
				TextSpan loc = new TextSpan();
				loc.iStartIndex = res.StartCol - 1;
				loc.iStartLine = res.StartLine - 1;
				loc.iEndIndex = res.EndCol - 1;
				loc.iEndLine = res.EndLine - 1;

				mgr.Add(new EditSpan(loc, res.ReplacementString));
			}
			mgr.ApplyEdits();
		}

		public override MethodData CreateMethodData()
		{
			return MethodData = base.CreateMethodData();
		}

		#region Paired chars insertion and deletion

		// TODO: maybe refactor this part so that it will share code with BracketFinder
		private readonly static char[][] _pairedChars = new char[][]
			{
				new char[] {'{', '}'},
				new char[] {'(', ')'},
				new char[] {'\'', '\''},
				new char[] {'[', ']'},
				new char[] {'"', '"'},
				new char[] {'<', '>'}
			};

		private bool IsOpeningPairedChar(char ch)
		{
			foreach (char[] charPair in _pairedChars)
			{
				if(charPair[0] == ch)
					return true;
			}
			return false;
		}

		private bool IsClosingPairedChar(char ch)
		{
			foreach (char[] charPair in _pairedChars)
			{
				if(charPair[1] == ch)
					return true;
			}
			return false;
		}
		
		private char GetClosingChar(char ch)
		{
			foreach (char[] charPair in _pairedChars)
			{
				if(charPair[0] == ch)
					return charPair[1];
			}
			throw new ApplicationException("Paired char not found for '" + ch + "'");
		}

		private char _rememberedChar;

		public void RememberCharBeforeCaret(IVsTextView textView)
		{
			int line;
			int idx;
			textView.GetCaretPos(out line, out idx);
			if(idx > 0)
				_rememberedChar = GetText(line, idx - 1, line, idx)[0];
		}

		public void ClearRememberedChar()
		{
			_rememberedChar = (char) 0;
		}


		#endregion

		private void RemoveCharAt(int line, int idx)
		{
			SetText(line, idx, line, idx + 1, "");
		}

		public override void OnCommand(IVsTextView textView, VsCommands2K command, char ch)
		{
			if (textView == null || Service == null || !Service.Preferences.EnableCodeSense)
				return;

			bool backward = (command == VsCommands2K.BACKSPACE || command == VsCommands2K.BACKTAB ||
				command == VsCommands2K.LEFT || command == VsCommands2K.LEFT_EXT);

			int line, idx;
			textView.GetCaretPos(out line, out idx);

			//ScanLexer sl = ((NemerleScanner) (GetColorizer().Scanner)).GetLexer();
			
			//Tuple<GlobalEnv, TypeBuilder, int, int> ret =
			//	ProjectInfo.Project.GetActiveEnv(FileIndex, line + 1);
			//sl.SetLine(line + 1, source, 0, ret.Field0, ret.Field1);
			TokenInfo tokenBeforeCaret = GetTokenInfo(line, idx);
			TokenInfo tokenAfterCaret = GetTokenInfo(line, idx + 1);

			//HandlePairedSymbols(textView, command, line, idx, ch);

			if ((tokenBeforeCaret.Trigger & TokenTriggers.MemberSelect) != 0 && (command == VsCommands2K.TYPECHAR))
			{
				ParseReason reason = ((tokenBeforeCaret.Trigger & TokenTriggers.MatchBraces) == TokenTriggers.MatchBraces) ?
					ParseReason.MemberSelectAndHighlightBraces :
					ParseReason.MemberSelect;

				Completion(textView, tokenBeforeCaret, reason);
			}
			TryHighlightBraces(textView, command, line, idx, tokenBeforeCaret, tokenAfterCaret);

			if (!MethodData.IsDisplayed &&
				(tokenBeforeCaret.Trigger & TokenTriggers.MethodTip) != 0 &&
				command == VsCommands2K.TYPECHAR &&
				Service.Preferences.ParameterInformation)
			{
				MethodTip(textView, line, idx, tokenBeforeCaret);
			}
		}

//    internal void HandleMatchBracesResponse(ParseRequest req)
//    {
//      try
//      {
//        if (Service == null)
//          return;

//        // Normalize the spans, and weed out empty ones, since there's no point
//        // trying to highlight an empty span.
//        var normalized = new System.Collections.ArrayList();
//        foreach (TextSpan span in req.Sink.Spans)
//        {
//          TextSpan norm = span;
//          TextSpanHelper.Normalize(ref norm, this.TextLines);
//          if (!TextSpanHelper.ValidSpan(this, norm))
//          {
//            Debug.Assert(false, "Invalid text span");
//          }
//          else if (!TextSpanHelper.IsEmpty(norm))
//          {
//            normalized.Add(norm);
//          }
//        }

//        if (normalized.Count == 0)
//          return;

//        //transform spanList into an array of spans
//        TextSpan[] spans = (TextSpan[])normalized.ToArray(typeof(TextSpan));

//        //highlight
//        ErrorHandler.ThrowOnFailure(req.View.HighlightMatchingBrace((uint)this.service.Preferences.HighlightMatchingBraceFlags, (uint)spans.Length, spans));
//        //try to show the matching line in the statusbar
//        if (spans.Length > 0 && Service.Preferences.EnableShowMatchingBrace)
//        {
//          IVsStatusbar statusBar = (IVsStatusbar)service.Site.GetService(typeof(SVsStatusbar));
//          if (statusBar != null)
//          {
//            TextSpan span = spans[0];
//            bool found = false;
//            // Gather up the other side of the brace match so we can 
//            // display the text in the status bar. There could be more than one
//            // if MatchTriple was involved, in which case we merge them.
//            for (int i = 0, n = spans.Length; i < n; i++)
//            {
//              TextSpan brace = spans[i];
//              if (brace.iStartLine != req.Line)
//              {
//                if (brace.iEndLine != brace.iStartLine)
//                {
//                  brace.iEndLine = brace.iStartLine;
//                  brace.iEndIndex = this.GetLineLength(brace.iStartLine);
//                }
//                if (!found)
//                {
//                  span = brace;
//                }
//                else if (brace.iStartLine == span.iStartLine)
//                {
//                  span = TextSpanHelper.Merge(span, brace);
//                }
//                found = true;
//              }
//            }
//            if (found)
//            {
//              Debug.Assert(TextSpanHelper.IsPositive(span));
//              string text = this.GetText(span);

//              int start;
//              int len = text.Length;

//              for (start = 0; start < len && Char.IsWhiteSpace(text[start]); start++) ;

//              if (start < span.iEndIndex)
//              {
//                if (text.Length > 80)
//                {
//                  text = String.Format(CultureInfo.CurrentUICulture, SR.GetString(SR.Truncated), text.Substring(0, 80));
//                }
//                text = String.Format(CultureInfo.CurrentUICulture, SR.GetString(SR.BraceMatchStatus), text);
//                ErrorHandler.ThrowOnFailure(statusBar.SetText(text));
//              }
//            }
//          }
//        }
//#if LANGTRACE
//            } catch (Exception e) {
//                Trace.WriteLine("HandleMatchBracesResponse exception: " + e.Message);
//#endif
//      }
//      catch
//      {
//      }
//    }


		public override void GetPairExtents(IVsTextView view, int line, int col, out TextSpan span)
		{
			var spanAry = GetMatchingBraces(false, line, col);

			if (spanAry.Length == 2)
				{
					if (TextSpanHelper.ContainsInclusive(spanAry[0], line, col))
						span = spanAry[1];
					else
						span = spanAry[0];
				}
				else
					span = new TextSpan();
			
		}

		/// <summary>
		/// Match paired tokens. Run in GUI thread synchronously!
		/// </summary>
		/// <param name="textView">Current view</param>
		/// <param name="line">zero based index of line</param>
		/// <param name="index">zero based index of char</param>
		/// <param name="info"></param>
		public override void MatchBraces(IVsTextView view, int line, int index, TokenInfo info)
		{
			LockWrite();
			try
			{
				var spanAry = GetMatchingBraces(false, line, index);
				if (spanAry.Length > 0)
					ErrorHandler.ThrowOnFailure(view.HighlightMatchingBrace(
						(uint)Service.Preferences.HighlightMatchingBraceFlags, (uint)spanAry.Length, spanAry));
			}
			finally { UnlockWrite(); }
		}

		/// <summary>
		/// Match paired tokens. Run in GUI thread synchronously!
		/// </summary>
		/// <param name="textView">Current view</param>
		/// <param name="isMatchBraces">match or highlight mraces</param>
		/// <param name="line">zero based index of line</param>
		/// <param name="index">zero based index of char</param>
		/// <param name="info">Token information</param>
		public TextSpan[] GetMatchingBraces(bool isMatchBraces, int line, int index)
		{
			var nline = line  + 1; // one based number of line
			var ncol  = index + 1; // one based number of column

			string fname = this.GetFilePath();

			// Steps: 
			// 1. Find token under text caret.
			// 2. Determine that it is a paired token.
			// 3. Determine paired token.
			// 4. Find paired token in the source file.
			// 5. Set info about paired tokens Sink and return it in AuthoringScope.

			#region Init vars

			var source = this;
			IVsTextColorState colorState = source.ColorState;
			Colorizer colorizer = source.GetColorizer();
			var scanner = (NemerleScanner)colorizer.Scanner;
			string lineText = source.GetLine(nline);
			scanner.SetSource(lineText, 0);

			#endregion

			// Steps: 1-3
			BracketFinder bracketFinder = new BracketFinder(source, nline, ncol, scanner, colorState);

			// 4. Find paired token in the source file.
			var matchBraceInfo = bracketFinder.FindMatchBraceInfo();

			if (matchBraceInfo != null)
			{
				// 5. Set info about paired tokens Sink and return it in AuthoringScope.

				// Fix a bug in MPF: Correct location of left token.
				// It need for correct navigation (to left side of token).
				//
				Token matchToken = matchBraceInfo.Token;
				//Location matchLocation = isMatchBraces && !BracketFinder.IsOpenToken(matchToken)
				//	? matchToken.Location.FromEnd() : matchToken.Location;
				Location matchLocation = matchToken.Location;

				// Set tokens position info

				var startSpan = Utils.SpanFromLocation(bracketFinder.StartBraceInfo.Token.Location);
				var endSpan   = Utils.SpanFromLocation(matchLocation);

				return new TextSpan[] { startSpan, endSpan };
			}

			return new TextSpan[0];
		}

		private void TryHighlightBraces(IVsTextView textView, VsCommands2K command, int line, int idx,
										TokenInfo tokenInfo)
		{
			// Highlight brace to the left from the caret
			if ((tokenInfo.Trigger & TokenTriggers.MatchBraces) != 0 && Service.Preferences.EnableMatchBraces)
			{
				if ( (command != VsCommands2K.BACKSPACE) && 
					(/*(command == VsCommands2K.TYPECHAR) ||*/
					Service.Preferences.EnableMatchBracesAtCaret)) 
				{		
					//if (!this.LanguageService.IsParsing)
					MatchBraces(textView, line, idx, tokenInfo);
				}
			}
		}
		private void TryHighlightBraces(IVsTextView textView, VsCommands2K command, int line, int idx,
										TokenInfo tokenBeforeCaret, TokenInfo tokenAfterCaret)
		{
			TryHighlightBraces(textView, command, line, idx + 1, tokenAfterCaret);
			TryHighlightBraces(textView, command, line, idx, tokenBeforeCaret);
		}

		public void TryHighlightBraces(IVsTextView textView)
		{
			if (_processingOfHiddenRegions)
				return;

			var colorizer = GetColorizer() as NemerleColorizer;
			if (colorizer != null && colorizer.IsClosed)
				return;
			
			int line, idx;
			textView.GetCaretPos(out line, out idx);

			TokenInfo tokenBeforeCaret = GetTokenInfo(line, idx);
			TokenInfo tokenAfterCaret = GetTokenInfo(line, idx + 1);

			TryHighlightBraces(textView, VsCommands2K.UP, line, idx, tokenBeforeCaret, tokenAfterCaret);
		}

		private void HandlePairedSymbols(IVsTextView textView, VsCommands2K command, int line, int idx, char ch)
		{
			// insert paired symbols here
			if(command == VsCommands2K.TYPECHAR)
			{
				if(IsOpeningPairedChar(ch))
				{
					SetText(line, idx, line, idx, GetClosingChar(ch).ToString());
					textView.SetCaretPos(line, idx);
				}
				// if we just typed closing char and the char after caret is the same then we just remove 
				// one of them
				if(IsClosingPairedChar(ch))
				{
					char charAfterCaret = GetText(line, idx, line, idx + 1)[0];
					if (ch == charAfterCaret/*IsClosingPairedChar(charAfterCaret)*/)
						RemoveCharAt(line, idx);
				}
			}
			// delete closing char if opened char was just backspaced and closing one is right next
			if(command == VsCommands2K.BACKSPACE)
			{
				char closingChar = GetText(line, idx, line, idx + 1)[0];
				if(IsOpeningPairedChar(_rememberedChar) && IsClosingPairedChar(closingChar))
					RemoveCharAt(line, idx);
			}
		}

		#endregion

		#region Implementation

		public Engine GetEngine()
		{
			var projectInfo = ProjectInfo;
			
			if (projectInfo == null)
				return NemerleLanguageService.DefaultEngine;

			return projectInfo.Engine;
		}

		public void OnProjectReloaded()
		{
		}

		public const uint HiddenRegionCookie = 42;

		internal void HandleParseResponse(ParseRequest req)
		{
      //try
      //{
      //  var reason = (int)req.Reason >= 100 ? ((ParseReason2)req.Reason).ToString() : req.Reason.ToString();
      //  Trace.WriteLine("HandleParseResponse: " + reason + " Timestamp: " + req.Timestamp);
      //  if (this.Service == null)
      //    return;

      //  switch (req.Reason)
      //  {
      //    case (ParseReason)ParseReason2.ParseTopDeclaration:
      //      Service.SynchronizeDropdowns(req.View);
      //      break;
      //    default: break;
      //  }

      //  //if (req.Timestamp == this.ChangeCount)
      //  {
      //    var sink = (NemerleAuthoringSink)req.Sink;
      //    // If the request is out of sync with the buffer, then the error spans
      //    // and hidden regions could be wrong, so we ignore this parse and wait 
      //    // for the next OnIdle parse.
      //    //!!ReportTasks(req.Sink.errors);
      //    if (req.Sink.ProcessHiddenRegions)
      //      ProcessHiddenRegions(sink.HiddenRegionsList);
      //  }
      //  this.Service.OnParseComplete(req);

      //}
      //catch (Exception e)
      //{
      //  Trace.WriteLine("HandleParseResponse exception: " + e.Message);
      //}
		}

		internal void CollapseAllRegions()
		{
			IVsHiddenTextSession session = GetHiddenTextSession();
			IVsEnumHiddenRegions ppenum;
			TextSpan[] aspan = new TextSpan[1];
			aspan[0] = GetDocumentSpan();
			ErrorHandler.ThrowOnFailure(session.EnumHiddenRegions((uint)FIND_HIDDEN_REGION_FLAGS.FHR_ALL_REGIONS, HiddenRegionCookie, aspan, out ppenum));
			IVsHiddenRegion[] aregion = new IVsHiddenRegion[1];
			using (new CompoundAction(this, "ToggleAllRegions"))
			{
				uint fetched;
				while (ppenum.Next(1, aregion, out fetched) == NativeMethods.S_OK && fetched == 1)
				{
					uint dwState;
					aregion[0].GetState(out dwState);
					//dwState &= ~(uint)HIDDEN_REGION_STATE.hrsExpanded;
					dwState = (uint)HIDDEN_REGION_STATE.hrsDefault;
					ErrorHandler.ThrowOnFailure(aregion[0].SetState(dwState,
						(uint)CHANGE_HIDDEN_REGION_FLAGS.chrDefault));
				}
			}
		}

		internal void RenameSymbols(string newName, GotoInfo[] usages)
		{
			RenameSymbols(newName, usages, true);
		}

		internal void RenameSymbols(string newName, GotoInfo[] usages, bool wrapInTransaction)
		{
			if(wrapInTransaction)
			{
				using (var undoTransaction = new LinkedUndoTransaction("Rename refactoring", Service.Site))
				{	
					RenameSymbolsInternal(newName, usages);
					undoTransaction.Commit();
				}
			}
			else 
				RenameSymbolsInternal(newName, usages);
		}

		private void RenameSymbolsInternal(string newName, GotoInfo[] usages)
		{
			var distinctFiles = (from us in usages
								 select us.Location.File).Distinct();

			foreach (string filePath in distinctFiles)
			{
				var tmpFilePath = filePath;
				var mgr = new EditArray(ProjectInfo.GetSource(tmpFilePath), null, true, "Renaming");
				var thisFileUsages = from use in usages
									 where use.Location.File == tmpFilePath
									 select use;

				foreach (var usage in thisFileUsages)
				{
					var span = Utils.SpanFromLocation(usage.Location);
					mgr.Add(new EditSpan(span, newName));
				}
				mgr.ApplyEdits();
			}
		}

		public void DeleteEmptyStatementAt(int lineIndex)
		{
			var txt = GetLine(lineIndex);
			if(txt.Trim() == ";")
			{
				//var len = GetLineLength(lineIndex);
				SetText(lineIndex, 0, lineIndex + 1, 0, "");
			}
		}
		/// <summary>Get text of line frome text bufer of IDE.</summary>
		/// <param name="line">Line position (first line is 1).</param>
		/// <returns>The text of line.</returns>
		public new string GetLine(int line)
		{
			line--; // Convert to zero based index.

#if DEBUG
			//int lineCount = LineCount;

			//if (line >= lineCount) // just for debugging purpose.
			//	Debug.Assert(line < lineCount);
#endif

			return base.GetLine(line);
		}

		/// <summary>Same as GetText but use Nemerle coordinate sisten (with base 1)</summary>
		public string GetRegion(int lineStart, int colStart, int lineEnd, int colEnd)
		{
			return GetText(lineStart - 1, colStart - 1, lineEnd - 1, colEnd - 1);
		}

		public string GetRegion(Location loc)
		{
			return GetRegion(loc.Line, loc.Column, loc.EndLine, loc.EndColumn);
		}

		public new int GetPositionOfLineIndex(int line, int col)
		{
			return base.GetPositionOfLineIndex(line - 1, col - 1);
		}

		public TupleIntInt GetLineIndexOfPosition(int pos)
		{
			int line, col;

			base.GetLineIndexOfPosition(pos, out line, out col);

			return new TupleIntInt(line + 1, col + 1);
		}

		public int LineCount
		{
			get
			{
				int lineCount;
				int hr1 = base.GetTextLines().GetLineCount(out lineCount);
				ErrorHandler.ThrowOnFailure(hr1);
				return lineCount;
			}
		}

		#endregion

		#region ISource Members

    public int CurrentVersion { get { return TimeStamp; } }

		public TupleStringInt GetTextAndCurrentVersion()
    {
			LockWrite();
			try { return new TupleStringInt(GetText(), CurrentVersion); }
			finally { UnlockWrite(); }
    }

		public TupleStringIntInt GetTextCurrentVersionAndFileIndex()
    {
			LockWrite();
			try { return new TupleStringIntInt(GetText(), CurrentVersion, FileIndex); }
			finally { UnlockWrite(); }
    }

    public void LockWrite()       { TextLines.LockBufferEx((uint)BufferLockFlags.BLF_READ); }
    public void UnlockWrite()     { TextLines.UnlockBufferEx((uint)BufferLockFlags.BLF_READ); }
    public void LockReadWrite()   { TextLines.LockBufferEx((uint)BufferLockFlags.BLF_READ_AND_WRITE); }
    public void UnlocReadkWrite() { TextLines.UnlockBufferEx((uint)BufferLockFlags.BLF_READ_AND_WRITE); }

		public void SetRegions(IList<RegionInfo> regions, int sourceVersion)
    {
      var newRegions = regions.Select(ri => 
        {
      	  var secondTime = RegionsLoaded;
          var location   = ri.Location;
          var text       = ri.Banner;
          var isExpanded = ri.Expanded;

          var r = new NewHiddenRegion
          {
            tsHiddenText = Utils.SpanFromLocation(location),
            iType = (int)HIDDEN_REGION_TYPE.hrtCollapsible,
            dwBehavior = (int)HIDDEN_REGION_BEHAVIOR.hrbEditorControlled, //.hrbClientControlled
            pszBanner = string.IsNullOrEmpty(text) ? null : text,
            dwClient = NemerleSource.HiddenRegionCookie,
            dwState = (uint)(secondTime || isExpanded ? HIDDEN_REGION_STATE.hrsExpanded : HIDDEN_REGION_STATE.hrsDefault)
          };

          if (text == "Toplevel typing")
          {
            // VladD2: Debug staff
            var behavior = (HIDDEN_REGION_BEHAVIOR)r.dwBehavior;
            var dwClient = r.dwClient;
            var state = (HIDDEN_REGION_STATE)r.dwState;
            var type = (HIDDEN_REGION_TYPE)r.iType;
            var text1 = r.pszBanner;
            var loc = Utils.LocationFromSpan(location.FileIndex, r.tsHiddenText);
            Debug.Assert(true);
          }

          return r;
        });

      ProcessHiddenRegions(newRegions.ToList(), sourceVersion);
    }

		public void SetTopDeclarations(TopDeclaration[] topDeclarations)
		{
			Declarations = topDeclarations;
		}

		#endregion
	}
}
