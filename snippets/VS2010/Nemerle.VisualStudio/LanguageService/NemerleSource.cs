using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Compiler;
using Nemerle.Compiler.Parsetree;
using Nemerle.Compiler.Utils;
using Nemerle.Compiler.Utils.Async;
using Nemerle.Completion2;
using Nemerle.Completion2.CodeFormatting;
using Nemerle.VisualStudio.GUI;
using Nemerle.VisualStudio.LanguageService.Highlighting.TypeClassifier;
using Nemerle.VisualStudio.Package;
using Nemerle.VisualStudio.Project;

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows;
using System.Windows.Forms;

using TopDeclaration = Nemerle.Compiler.Parsetree.TopDeclaration;
using TupleIntInt = Nemerle.Builtins.Tuple<int, int>;
using TupleStringInt = Nemerle.Builtins.Tuple<string, int>;
using TupleStringIntInt = Nemerle.Builtins.Tuple<string, int, int>;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;
// ReSharper disable LocalizableElement

namespace Nemerle.VisualStudio.LanguageService
{
  public partial class NemerleSource : Source, IIdeSource
  {
    #region Init

    public NemerleSource(NemerleLanguageService service, IVsTextLines textLines)
      : base(service, textLines, null)
    {
      // ReSharper disable DoNotCallOverridableMethodsInConstructor
      string path = GetFilePath();
      // ReSharper restore DoNotCallOverridableMethodsInConstructor
      IsNemerleSybtax = Utils.Eq(Path.GetExtension(path), ".n");

      var vsTextBuffer = textLines as IVsTextBuffer;
      var textBuffer = vsTextBuffer.ToITextBuffer();
      if (!textBuffer.Properties.ContainsProperty(typeof(NemerleSource)))
        textBuffer.Properties.AddProperty(typeof(NemerleSource), this);

      Service = service;
      ProjectInfo projectInfo = ProjectInfo.FindProject(path);
      ProjectInfo = projectInfo;

      if (projectInfo != null)
      {
        projectInfo.AddEditableSource(this);
        try
        {
          projectInfo.MakeCompilerMessagesTextMarkers(textLines, FileIndex);
        }
        catch { }
      }

      LastDirtyTime = DateTime.Now;

      SmartIndent = new NemerleSmartIndentation(this);


      UpdateProjectInfo(projectInfo);
    }

    public void UpdateProjectInfo(ProjectInfo projectInfo)
    {
      if (projectInfo == null)
        GetEngine().BeginUpdateCompileUnit(this);
      else
      {
        ProjectInfo = projectInfo;
        projectInfo.Engine.BeginUpdateCompileUnit(this);
      }
    }

    #endregion

    #region Properties

    public int RefCount { get; private set; }

    public DateTime LastDirtyTime { get; private set; }
    //public new DateTime                LastParseTime { get; private set; }
    public NemerleLanguageService Service { get; private set; }
    public ProjectInfo ProjectInfo { get; private set; }
    public MethodData MethodData { get; private set; }
    public int TimeStamp { get; private set; }
    internal TopDeclaration[] Declarations { get; set; }
    public bool RegionsLoaded { get; set; }
    public CompileUnit CompileUnit { get; set; }

    /// <summary>
    /// A NemerleSource usen for .n files and for extentions like .cs-files inside Nemerle project.
    /// IsNemerleSybtax is true if this file is Nemerle language file (niot extension).
    /// </summary>
    public bool IsNemerleSybtax { get; private set; }

    internal NemerleSmartIndentation SmartIndent { get; private set; }

    public int FileIndex
    {
      get
      {
        if (_fileIndex <= 0)
        {
          var path = base.GetFilePath();

          if (path.IsNullOrEmpty())
            path = GetStubFileForSecondaryBuffer(this.GetTextLines());

          if (path.IsNullOrEmpty())
            return -1;

          _fileIndex = Location.GetFileIndex(path);
        }

        return _fileIndex;
      }
    }

    public bool HasView
    {
      get
      {
        //var vsTextBuffer = (IVsTextBuffer)TextLines;
        //var mgr = (IVsTextManager)LanguageService.GetService(typeof(VsTextManagerClass));
        //IVsTextView vsTextView;
        //var result = mgr.GetActiveView(0, vsTextBuffer, out vsTextView);
        //return result >= 0;
        return true;
      }
    }

    public IVsTextLines TextLines
    {
      get { return GetTextLines(); }
    }

    #endregion

    #region Fields

    public IList<Location> TypeLocations;
    public List<Tuple<Location, List<Location>>> MethodsTypeLocations = new List<Tuple<Location, List<Location>>>();
    readonly List<RelocationRequest> _relocationRequestsQueue = new List<RelocationRequest>();
    int _fileIndex = -1;
    QuickTipInfoAsyncRequest _tipAsyncRequest;

    #endregion

    #region OnChangeLineText

    public override void OnChangeLineText(TextLineChange[] lineChange, int last)
    {
      base.OnChangeLineText(lineChange, last);

      if (!IsNemerleSybtax)
        return;

      TimeStamp++;

      ProjectInfo projectInfo = ProjectInfo;

      if (projectInfo == null)
      {
        GetEngine().BeginUpdateCompileUnit(this);
        return;
      }

      if (projectInfo.IsDocumentOpening)
        return;

      if (projectInfo.IsProjectAvailable)
      {
        TextLineChange changes = lineChange[0];

        var info = new RelocationInfo(FileIndex,
          new TextPoint(changes.iStartLine  + 1, changes.iStartIndex  + 1),
          new TextPoint(changes.iOldEndLine + 1, changes.iOldEndIndex + 1),
          new TextPoint(changes.iNewEndLine + 1, changes.iNewEndIndex + 1)
        );

        RelocateTypeLocations(info);
        // TODO: use info in AddRelocationRequest()
        RelocationQueue.AddRelocationRequest(
          RelocationRequestsQueue,
          FileIndex, CurrentVersion,
          changes.iNewEndLine + 1, changes.iNewEndIndex + 1,
          changes.iOldEndLine + 1, changes.iOldEndIndex + 1,
          changes.iStartLine + 1,  changes.iStartIndex + 1);
      }

      projectInfo.Engine.BeginUpdateCompileUnit(this); // Add request for reparse & update info about CompileUnit
    }

    private void RelocateTypeLocations(RelocationInfo info)
    {
      RelocateList(info, TypeLocations);

      for (int i = 0; i < MethodsTypeLocations.Count; i++)
      {
        var methodsTypeLocation = MethodsTypeLocations[i];
        var loc = methodsTypeLocation.Item1;
        var lst = methodsTypeLocation.Item2;
        var newLoc = Nemerle.Compiler.Completion.Relocate(loc, info);
        if (newLoc != loc)
        {
          RelocateList(info, lst);
          MethodsTypeLocations[i] = Tuple.Create(loc, lst);
        }
      }
    }

    private static void RelocateList(RelocationInfo info, IList<Location> locations)
    {
      if (locations != null)
      {
        for (int i = 0; i < locations.Count; i++)
        {
          var typeLoc = locations[i];
          var newLoc = Nemerle.Compiler.Completion.Relocate(typeLoc, info);
          if (newLoc != typeLoc)
            locations[i] = newLoc;
        }
      }
    }

    #endregion

    #region Overrides

    public override TextSpan UncommentLines(TextSpan span, string lineComment)
    {
      // Remove line comments
      int clen = lineComment.Length;
      var editMgr = new EditArray(this, null, true, "UncommentLines");

      for (int line = span.iStartLine; line <= span.iEndLine; line++)
      {
        int i = this.ScanToNonWhitespaceChar(line);
        string text = base.GetLine(line);

        if ((i + clen) <= text.Length && text.Substring(i, clen) == lineComment)
        {
          var es = new EditSpan(new TextSpan()
          {
            iEndLine = line,
            iStartLine = line,
            iStartIndex = i,
            iEndIndex = i + clen
          }, "");
          editMgr.Add(es); // remove line comment.

          if (line == span.iStartLine && span.iStartIndex != 0)
            span.iStartIndex = i;
        }
      }

      editMgr.ApplyEdits();

      span.iStartIndex = 0;
      return span;
    }

    public override CompletionSet CreateCompletionSet()
    {
      return new NemerleCompletionSet(LanguageService.GetImageList(), this);
    }

    public void Completion(IVsTextView textView, int lintIndex, int columnIndex, bool byTokenTrigger)
    {
      var result = GetEngine().Completion(this, lintIndex + 1, columnIndex + 1, false);
      var decls = new NemerleDeclarations(result, this, result.CompletionResult.IsMemeberComplation);
      CompletionSet.Init(textView, decls, !byTokenTrigger);
    }

    public void ImportCompletion(IVsTextView textView, int lintIndex, int columnIndex)
    {
      var result = GetEngine().Completion(this, lintIndex + 1, columnIndex + 1, true);
      var decls = new NemerleDeclarations(result, this, result.CompletionResult.IsMemeberComplation);
      CompletionSet.Init(textView, decls, true);
    }

    public override string GetFilePath()
    {
      return Location.GetFileName(FileIndex);
    }

    public override bool IsDirty
    {
      get { return base.IsDirty; }
      set
      {
        Debug.WriteLine("IsDirty = " + value);
        base.IsDirty = value;
        if (value)
          LastDirtyTime = DateTime.Now;
        //else
        //	LastParseTime = System.DateTime.Now;
      }
    }

    public override void OnIdle(bool periodic)
    {
    }

    public IVsTextView GetView()
    {
      if (Service == null)
        throw new InvalidOperationException("The Service property of NemerleSource is null!");

      return Service.GetPrimaryViewForSource(this);
    }

    public override void MethodTip(IVsTextView textView, int line, int index, TokenInfo info)
    {
      var result = GetEngine().BeginGetMethodTipInfo(this, line + 1, index + 1);
      result.AsyncWaitHandle.WaitOne();
      if (result.Stop)
        return;
      if (result.MethodTipInfo == null || !result.MethodTipInfo.HasTip)
      {
        MethodData.Dismiss();
        return;
      }

      var methods = new NemerleMethods(result.MethodTipInfo);

      var span = result.MethodTipInfo.StartName.Combine(result.MethodTipInfo.EndParameters).ToTextSpan();
      MethodData.Refresh(textView, methods, result.MethodTipInfo.ParameterIndex, span);
      Debug.WriteLine("MethodTip");
    }

    /// <summary>
    /// Подготавливает результаты Find Usages/Find All References
    /// </summary>
    public GotoInfo[] GetGotoInfo(IVsTextView view, bool gotoDefinition, int lineIndex, int colIndex, out string caption)
    {
      caption = null;

      var engine = GetEngine();
      var line = lineIndex + 1;
      var col = colIndex + 1;

      GotoInfo[] infos = gotoDefinition
        ? engine.GetGotoInfo(this, line, col, GotoKind.Definition)
        : engine.GetGotoInfo(this, line, col, GotoKind.Usages);

      if(infos == null || infos.Length == 0)
        return infos;			

      if(!infos[0].HasLocation && infos[0].Member != null)
      {
        Debug.Assert(infos.Length == 1, "Multiple unknown locations are unexpected");
        var inf = infos[0];
        GotoInfo[] infoFromPdb = TryFindGotoInfoByDebugInfo(engine, inf);

        if(infoFromPdb.Length == 0)
        {
          if(inf.Member != null)
          {
            var res = ProjectInfo.FindProjectByOutput(inf.FilePath);

            if(res != null)
              infos = res.Engine.GetGotoInfoForMember(inf.Member.GetFullName(), false, GotoKind.Definition);

            if(infos.Length <= 0 || res == null)
              infos = NemerleGoto.GenerateSource(infos, engine, out caption);
          } else
            infos = NemerleGoto.GenerateSource(infos, engine, out caption);
        } else
          infos = infoFromPdb;
      }

      return infos;
    }

    /// <summary>
    /// Этот метод заполняет и показывает окошко GotoUsageForm
    /// </summary>
    public void Goto(IVsTextView view, bool gotoDefinition, int lineIndex, int colIndex)
    {	
      string caption;

      var infos = GetGotoInfo(view, gotoDefinition, lineIndex, colIndex, out caption);

      if((infos == null) || (infos.Length == 0))
        return;

      var langSrvc = (NemerleLanguageService)LanguageService;

      if (infos.Length == 1)
        langSrvc.GotoLocation(infos[0].Location, caption, caption != null);
      else if (infos.Length > 0)
      {
        var textEditorWnd = NativeWindow.FromHandle(view.GetWindowHandle());

        using (var popup = new GotoUsageForm(infos))
          if ((textEditorWnd == null ? popup.ShowDialog() : popup.ShowDialog(textEditorWnd)) == DialogResult.OK)
            langSrvc.GotoLocation(popup.Result.Location, caption, caption != null);
      }
    }

    /// <summary>
    /// Этот метод пытается найти позиции перехода на основании имеющехся типов считанных из других сборок,
    /// анализа .pdb-файлов (файлов содежащих отладочную информацию) и парсинга исходников.
    /// </summary>
    /// <param name="engine"></param>
    /// <param name="inf"></param>
    /// <returns></returns>
    /// <remarks>
    /// Описание проблемы:
    /// К сожалению .pdb-файлы не содержат полной информации о местоположениях тиопов. Местоположения содержатся
    /// только для методов (так как именно они подвергаются отладке). Нам же нужно находить не так же типы 
    /// (причем типы могут располагаться более чем в одном файле). Поэтому поступаем следующим образом...
    /// 1. Извлекаем тип для которого нужно получить информацию для перехода. Это может быть или непосредственно
    /// член на котрый осуществляетя переход, или тип в котором объявлен этот член (если перход идет на член 
    /// отличный от типа.
    /// 2. Получаем полный список членов в котором типа полученного на шаге 1.
    /// 3. Получаем местоположения членов (вместе с путем к файлу). Местоположения могут быть не полными. Например, 
    /// может быть задан только путь к файлу, а позиция в файле может быть не верной (при этом, в колонке 
    /// содержится 0). Если мы нашли местопложение для искомого члена и оно корректно, то используем его 
    /// (на этом поиск нужно прекратить). В противном случае переходим к следующему шагу.
    /// 4. Мы получили список файлов в которых может содежаться искомый член. Парсим каждый из файлов 
    /// и ищем в них тип в котором объявлен нужный нам член. Если член и есть тип, то завершаем обработку
    /// и возвращаем местположения типа (тип может быть обявлен более чем в одном файле).
    /// В противном случае переходим к следующему шагу.
    /// 5. Ищем в типе наденом на шаге 5 член с именем совпадающим с искомым (взятым из IMember). Если найдено
    /// более одного члена, то производит отсев наиболее подходящего. Для этого последовательно проверяем список
    /// аргументов, возвращаемое значение и т.п.
    /// </remarks>
    private GotoInfo[] TryFindGotoInfoByDebugInfo(IIdeEngine engine, GotoInfo inf)
    {
      GotoInfo[] infoFromPdb = ProjectInfo.LookupLocationsFromDebugInformation(inf);
      var result = new List<GotoInfo>();

      foreach (GotoInfo item in infoFromPdb)
      {
        var cu = engine.ParseCompileUnit(new FileNemerleSource(item.Location.FileIndex));
        var res = TryGetGotoInfoForMemberFromSource(inf.Member, item.Location, cu);

        if (res.Length > 0)
          result.AddRange(res);
      }

      return result.ToArray();
    }

    private static GotoInfo[] TryGetGotoInfoForMemberFromSource(IMember member, Location loc, CompileUnit cu)
    {
      Trace.Assert(member != null);
      var ty = member as TypeInfo;
      var soughtIsType = ty != null;

      if (ty == null)
        // ReSharper disable PossibleNullReferenceException
        ty = member.DeclaringType;
      // ReSharper restore PossibleNullReferenceException

      var td = FindTopDeclaration(ty, cu);

      if (td == null)
        return new[] { new GotoInfo(loc, UsageType.GeneratedDefinition) };

      if (soughtIsType)
        return new[] { new GotoInfo(Location.GetFileName(cu.FileIndex), td.NameLocation) };

      var name = member.Name;
      var file = Location.GetFileName(cu.FileIndex);
      var members = td.GetMembers().Where(m => string.Equals(m.Name, name, StringComparison.Ordinal)).ToArray();

      if (members.Length == 1)
        return new[] { new GotoInfo(file, members[0].NameLocation) };

      var isProp = member is IProperty;

      members = td.GetMembers().Where(m => string.Equals(m.Name, name, StringComparison.OrdinalIgnoreCase)
        // Макро [Accessor] может изменять имя свойства. Учитываем это...
                         || (isProp && string.Equals(m.Name.Replace("_", ""), name, StringComparison.OrdinalIgnoreCase))).ToArray();

      if (members.Length > 0)
      {
        if (loc.Column > 0)
        {
          var members2 = members.Where(m => m.Location.Contains(loc)).ToArray();

          if (members2.Length > 0)
            return members2.Select(m => new GotoInfo(file, m.NameLocation)).ToArray();

          return new[] { new GotoInfo(file, td.NameLocation) };
        }

        if (members.Length == 1)
          return new[] { new GotoInfo(file, members[0].NameLocation) };

        return FindBastMember(members, member).Select(m => new GotoInfo(file, m.NameLocation)).ToArray();
      }

      // ничего не нашли

      if (td != null) // но у нас есть тип в котором объявлен член...
        return new[] { new GotoInfo(td.NameLocation) }; // возвращаем его имя!

      if (loc.IsEmpty)
        return new GotoInfo[0];

      return new[] { new GotoInfo(loc, UsageType.GeneratedDefinition) }; // если все обломалось, вернем хотя бы что-то (правда, вряд ли это будте корректным результатом)
    }

    // ReSharper disable ParameterTypeCanBeEnumerable.Local
    // ReSharper disable ReturnTypeCanBeEnumerable.Local
    private static ClassMember[] FindBastMember(ClassMember[] members, IMember member)
    {
      var method = member as IMethod;

      if (method != null)
        return FindBastMethod(members, method);

      return members;
    }

    private static ClassMember[] FindBastMethod(ClassMember[] members, IMethod method)
    {
      var parms = method.GetParameters();
      var parmsCount = parms.Length;

      var methods = members.Where(m => m is ClassMember.Function).Cast<ClassMember.Function>().ToArray();

      var methods2 = methods.Where(m => m.header.ParsedParameters.Length == parmsCount).ToArray();

      if (methods2.Length == 1)
        return methods2;

      return methods2;
    }
    // ReSharper restore ReturnTypeCanBeEnumerable.Local
    // ReSharper restore ParameterTypeCanBeEnumerable.Local

    private static TopDeclaration FindTopDeclaration(TypeInfo ty, CompileUnit cu)
    {
      var fullName = ty.FrameworkTypeName.Replace("+", ".");//ty.FullName;

      foreach (var td in cu.TopDeclarations)
      {
        if (td.FullQualifiedName == fullName)
          return td;

        foreach (var td2 in td.GetAllInnerTypes())
        {
          if (td2.FullQualifiedName == fullName)
            return td2;
        }
      }

      return null;
    }

    public override ParseRequest BeginParse(int line, int idx, TokenInfo info, ParseReason reason, IVsTextView view, ParseResultHandler callback)
    {
      //return base.BeginParse(line, idx, info, reason, view, callback);
      switch (reason)
      {
        case ParseReason.Autos: break;
        case ParseReason.Check: break;
        case ParseReason.CodeSpan: break;
        case ParseReason.CompleteWord: break;
        case ParseReason.DisplayMemberList: break;
        case ParseReason.Goto: break;
        case ParseReason.MemberSelect: break;
        case ParseReason.MemberSelectAndHighlightBraces: break;
        case ParseReason.MethodTip: break;
        case ParseReason.None: break;
        case ParseReason.QuickInfo: break;
        case ParseReason.HighlightBraces:
        case ParseReason.MatchBraces:
          Trace.Assert(false);
          break;
        default: break;
      }

      Debug.WriteLine("Soutce.BeginParse: " + reason);
      return null;
    }

    public override void Dispose()
    {
      if (ProjectInfo != null)
      {
        ProjectInfo.RemoveEditableSource(this);

        if (IsSecondarySource)
        {
          var source = ProjectInfo.GetSource(FileIndex);

          if (source != null)
            ProjectInfo.RemoveSource(source);

          RemoveStubFileForSecondaryBuffer(GetTextLines());
        }

        ProjectInfo = null;
      }

      SmartIndent = null;
      MethodData = null;
      _tipAsyncRequest = null;
      CompileUnit = null;
      Declarations = null;
      Service = null;

      if (_relocationRequestsQueue != null)
        _relocationRequestsQueue.Clear();

      if (TypeLocations != null)
        TypeLocations.Clear();

      if (MethodsTypeLocations != null)
        MethodsTypeLocations.Clear();

      base.Dispose();
    }

    public override string ToString()
    {
      var name = Location.GetFileName(FileIndex);

      if (IsClosed)
        return "NemerleSource: " + name + " (Closed!)";

      return "NemerleSource: " + name;
    }

    public override CommentInfo GetCommentFormat()
    {
      return new CommentInfo { UseLineComments = true, LineStart = "//", BlockStart = "/*", BlockEnd = "*/" };
    }

    public override TextSpan CommentLines(TextSpan span, string lineComment)
    {
      // Calculate minimal position of non-space char
      // at lines in selected span.
      var minNonEmptyPosition = 0;

      for (var i = span.iStartLine; i <= span.iEndLine; ++i)
      {
        var line = base.GetLine(i);

        if (line.Trim().Length <= 0)
          continue;

        var spaceLen = line.Replace(line.TrimStart(), "").Length;

        if (minNonEmptyPosition == 0 || spaceLen < minNonEmptyPosition)
          minNonEmptyPosition = spaceLen;
      }

      // insert line comment at calculated position.
      var editMgr = new EditArray(this, null, true, "CommentLines");

      for (var i = span.iStartLine; i <= span.iEndLine; ++i)
      {
        var text = base.GetLine(i);

        if (minNonEmptyPosition <= text.Length && text.Trim().Length > 0)
        {
          var commentSpan = new TextSpan();

          commentSpan.iStartLine = commentSpan.iEndLine = i;
          commentSpan.iStartIndex = commentSpan.iEndIndex = minNonEmptyPosition;

          editMgr.Add(new EditSpan(commentSpan, lineComment));
        }
      }

      editMgr.ApplyEdits();

      // adjust original span to fit comment symbols
      span.iEndIndex += lineComment.Length;
      return span;
    }

    public override AuthoringSink CreateAuthoringSink(ParseReason reason, int line, int col)
    {
      Trace.Assert(false, "We don't using MS infrastructure of background parsing now. This code should not be called!");
      throw new NotImplementedException("This should not be heppen!");
    }

    public override TokenInfo GetTokenInfo(int line, int col)
    {
      TokenInfo tokenInfo;
      if (ParseUtil.TryGetTokenInfo(TextLines.ToITextBuffer(), line, col, out tokenInfo))
        return tokenInfo;
      else
        return new TokenInfo();
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
        return x.iStartLine == y.iStartLine && x.iEndLine == y.iEndLine
          && x.iEndIndex == y.iEndIndex && x.iStartIndex == y.iStartIndex;
      }

      public int GetHashCode(TextSpan x)
      {
        return x.iStartLine ^ x.iEndLine ^ x.iEndIndex ^ x.iStartIndex;
      }

      public static readonly TextSpanEqCmp Instance = new TextSpanEqCmp();
    }

    public void ProcessHiddenRegions(List<NewHiddenRegion> regions, int sourceVersion)
    {
      if (!OutliningEnabled)
        return;

      //var timer    = Stopwatch.StartNew();
      //var timerAll = Stopwatch.StartNew();

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

      IVsHiddenTextSession session = GetHiddenTextSession();
      var aspan = new TextSpan[1];
      aspan[0] = GetDocumentSpan();
      var aregion = new IVsHiddenRegion[1];
      var oldRegionsMap = new Dictionary<TextSpan, IVsHiddenRegion>(TextSpanEqCmp.Instance);
      IVsEnumHiddenRegions ppenum = null;

      ErrorHandler.ThrowOnFailure(session.EnumHiddenRegions((uint)FIND_HIDDEN_REGION_FLAGS.FHR_ALL_REGIONS, HiddenRegionCookie, aspan, out ppenum));

      uint fetched;

      while (ppenum.Next(1, aregion, out fetched) == NativeMethods.S_OK && fetched == 1)
      {
        var region = aregion[0];
        int regTypeInt;
        ErrorHandler.ThrowOnFailure(region.GetType(out regTypeInt));
        uint dwData;
        region.GetClientData(out dwData);
        var regType = (HIDDEN_REGION_TYPE)regTypeInt;
        if (regType != HIDDEN_REGION_TYPE.hrtCollapsible)// || dwData != 0 && dwData != HiddenRegionCookie)
          continue;

        ErrorHandler.ThrowOnFailure(region.GetSpan(aspan));
        TextSpan s = aspan[0];
        //var loc = Utils.LocationFromSpan(FileIndex, s);
        oldRegionsMap[s] = region;
      }

      //Debug.WriteLine("SetRegions: old regions fetched " + timer.Elapsed); timer.Reset(); timer.Start();

      #endregion

      // Обновлять регионы нужно на заблокированном для записи редакторе, иначе, если текст
      // изменится во время обновления, то мы получим рассинхронизацию, и как следствие,
      // неопределенное поеведение. По возможности не следует добавлять долгие действия 
      // в блок расположенный ниже, так как обновление идет в GUI-потоке и медленная работа
      // может привести к ощутимому неудобству для пользователя.

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
            //removed++;
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
          int maxTime = LanguageService.Preferences.MaxRegionTime;
          const int ChunkSize = 1000;
          var chunk = new NewHiddenRegion[ChunkSize];
          int i = 0;
          while (i < count && Utils.TimeSince(start) < maxTime)
          {
            int j = 0;
            while (i < count && j < ChunkSize)
            {
              NewHiddenRegion r = newRegions[i];
              if (!TextSpanHelper.ValidSpan(this, r.tsHiddenText))
              {
                //Debug.Assert(false, "Invalid span " + r.tsHiddenText.iStartLine + "," + r.tsHiddenText.iStartIndex + "," 
                //                     + r.tsHiddenText.iEndLine + "," + r.tsHiddenText.iEndIndex);
                //break;
                //invalid++;
              }
              else
              {
                chunk[j] = r;
                j++;
                //added++;
              }
              i++;
            }

            if (j > 0)
            {
              int hr = session.AddHiddenRegions((int)CHANGE_HIDDEN_REGION_FLAGS.chrNonUndoable, j, chunk, null);
              if (ErrorHandler.Failed(hr))
                break; // stop adding if we start getting errors.
            }
          }
        }

        //Debug.WriteLine("SetRegions: new regions added   " + timer.Elapsed); timer.Reset(); timer.Start();

        //Debug.WriteLine("Removed: " + removed + " For add: " + newRegions.Count + " Really added: " + added + " invalid: " + invalid);

        #endregion
      }
      catch (Exception ex) { Debug.WriteLine("Exception while region processing: " + ex.Message); }
      finally
      {
        UnlockWrite();

        //Debug.WriteLine("SetRegions: end (took all)      " + timerAll.Elapsed);
      }

      RegionsLoaded = true;
    }

    public override void ReformatSpan(EditArray mgr, TextSpan span)
    {
      string filePath = GetFilePath();
      ProjectInfo projectInfo = ProjectInfo.FindProject(filePath);
      IIdeEngine engine = projectInfo != null ? projectInfo.Engine : NemerleLanguageService.DefaultEngine;
            ReformatSpanInternal(mgr, span, engine, filePath, this, LanguageService.GetLanguagePreferences());
    }

    private static void ReformatSpanInternal(EditArray mgr, TextSpan span, IIdeEngine engine, string filePath, IIdeSource src, LanguagePreferences pref)
    {
      var result = Formatter.BeginFormat(span.iStartLine + 1,
               span.iStartIndex + 1,
               span.iEndLine + 1,
               span.iEndIndex + 1,
               engine,
               src, new IndentInfo(pref.InsertTabs, pref.IndentSize, pref.TabSize));
      result.AsyncWaitHandle.WaitOne();
      var results = result.Result;
      using (var editArray = new EditArray(mgr.Source, mgr.TextView, true, "formatting"))
      {
        foreach (FormatterResult res in results)
        {
          var loc = new TextSpan
              {
                iStartIndex = res.StartCol - 1,
                iStartLine = res.StartLine - 1,
                iEndIndex = res.EndCol - 1,
                iEndLine = res.EndLine - 1
              };

          editArray.Add(new EditSpan(loc, res.ReplacementString));
        }

        editArray.ApplyEdits();
      }
    }

    public override MethodData CreateMethodData()
    {
      return MethodData = base.CreateMethodData();
    }

    public override void OnCommand(IVsTextView textView, VsCommands2K command, char ch)
    {
      if (textView == null || Service == null || !Service.Preferences.EnableCodeSense)
        return;

      int line, idx;
      textView.GetCaretPos(out line, out idx);

      if (LanguageService.Preferences.IndentStyle == IndentingStyle.Smart && ch == '}')
        SmartIndent.At(line);

      TokenInfo tokenBeforeCaret = GetTokenInfo(line, idx);

      //VladD2: We do not trigger MethodTip on type because it's very slow!

      // This code open completion list if user enter '.'.
      if ((tokenBeforeCaret.Trigger & TokenTriggers.MemberSelect) != 0 && (command == VsCommands2K.TYPECHAR))
      {
        var spaces                 = new[] { '\t', ' ', '\u000B', '\u000C' };
        var tokenBeforeCaretLength = tokenBeforeCaret.EndIndex - tokenBeforeCaret.StartIndex;
        var str                    = GetText(line, 0, line, idx - tokenBeforeCaretLength).Trim(spaces);

        while (str.Length <= 0 && line > 0) // skip empy lines
        {
          line--;
          str = GetLine(line + 1).Trim(spaces);
        }

        if (str.Length > 0)
        {
          var lastChar = str[str.Length - 1];

          // Don't show completion list if previous char not one of following:
          if (char.IsLetterOrDigit(lastChar) || lastChar == ')' || lastChar == ']')
            Completion(textView, line, idx, true);
        }
      }
    }

    public override void GetPairExtents(IVsTextView view, int line, int col, out TextSpan span)
    {
      span = new TextSpan();
    }

    private CompileUnit TryGetCompileUnit()
    {
      var compileUnit = CompileUnit;
      return compileUnit;
    }

    public void ImplementInterfaces(int line, int column)
    {
      var engine = GetEngine();

      if (!engine.IsDefaultEngine)
        engine.BeginFindUnimplementedMembers(this, line, column);
    }

    public void OverrideMembers(int line, int column)
    {
      var engine = GetEngine();

      if (!engine.IsDefaultEngine)
        engine.BeginFindMethodsToOverride(this, line, column);
    }

    #endregion

    #region Implementation

    public void AddRef()
    {
      RefCount++;
    }

    public void ReleaseRef()
    {
      RefCount--;
    }

    internal void Rename(string newFileName)
    {
      _fileIndex = Location.GetFileIndex(newFileName);
    }

    #region Table formating

    internal bool TryDoTableFormating(NemerleViewFilter view, int line, int col)
    {
      if (!view.GetSelection().IsSpanEmpty())
        return false;

      if (!Service.Package.UseSmartTab)
        return false;

      var text = GetLine(line);

      if (!IsInContent(text, col))
        return false;

      if (line > 1 && TryInsertSpacesToNextToken(GetLine(GetPrevNotEmpryLineIndex(line)), view, col))
        return true;

      if (line < LineCount - 1 && TryInsertSpacesToNextToken(GetLine(GetNextNotEmpryLineIndex(line)), view, col))
        return true;

      return EmulateTabBySpaces(view, line, col);
    }

    private static bool IsInContent(string lileText, int col)
    {
      col--;

      if (col > lileText.Length)
        return false;

      for (int i = 0; i < col; i++)
        if (!char.IsWhiteSpace(lileText[i]))
          return true;

      return false;
    }

    private static int IndexOfNextToken(string lileText, int startIndex)
    {
      if (startIndex >= lileText.Length)
        return -1;

      for (int i = startIndex; i < lileText.Length; i++)
      {
        if (!char.IsWhiteSpace(lileText[i]))
          continue;

        for (; i < lileText.Length; i++)
          if (!char.IsWhiteSpace(lileText[i]))
            break;

        return i;
      }

      return lileText.Length - 1;
    }

    bool TryInsertSpacesToNextToken(string patternLineText, NemerleViewFilter view, int col)
    {
      if (!IsInContent(patternLineText, col))
        return false;

      var nextTokenStartIndex = IndexOfNextToken(patternLineText, col - 1);
      var spacesCount = nextTokenStartIndex - (col - 1);

      if (nextTokenStartIndex < 0 || spacesCount <= 0)
        return false;

      var completor = new Completor(LanguageService, view.TextView, "table formating");
      completor.TypeChars(new string(' ', nextTokenStartIndex - (col - 1)));
      completor.Apply();
      return true;
    }

    bool EmulateTabBySpaces(NemerleViewFilter view, int line, int col)
    {
      var tabSize = LanguageService.Preferences.TabSize;
      var visiblePosition = ColumnToVisiblePosition(line - 1, col - 1);

      var countSpaces = visiblePosition % tabSize == 0 ? tabSize : tabSize - (visiblePosition % tabSize);
      var completor = new Completor(LanguageService, view.TextView, "table formating");
      completor.TypeChars(new string(' ', countSpaces));
      completor.Apply();
      return true;
    }

    bool IsAllWhiteSpace(string text)
    {
      for (int i = 0; i < text.Length; i++)
        if (!char.IsWhiteSpace(text[i]))
          return false;

      return true;
    }

    int GetPrevNotEmpryLineIndex(int line)
    {
      if (--line <= 1)
        return 1;

      while (IsAllWhiteSpace(GetLine(line)))
        if (--line <= 1)
          return 1;

      return line;
    }

    int GetNextNotEmpryLineIndex(int line)
    {
      var len = LineCount;

      if (++line > len)
        return len;

      while (IsAllWhiteSpace(GetLine(line)))
        if (++line > len)
          return len;

      return line;
    }

    #endregion

    public IIdeEngine GetEngine()
    {
      var projectInfo = ProjectInfo;

      if (projectInfo == null)
        return NemerleLanguageService.DefaultEngine;

      return projectInfo.Engine;
    }

    public const uint HiddenRegionCookie = 42;

    internal void CollapseAllRegions()
    {
      IVsHiddenTextSession session = GetHiddenTextSession();
      IVsEnumHiddenRegions ppenum;
      var aspan = new TextSpan[1];
      aspan[0] = GetDocumentSpan();
      ErrorHandler.ThrowOnFailure(session.EnumHiddenRegions((uint)FIND_HIDDEN_REGION_FLAGS.FHR_ALL_REGIONS, HiddenRegionCookie, aspan, out ppenum));
      var aregion = new IVsHiddenRegion[1];
      using (new CompoundAction(this, "ToggleAllRegions"))
      {
        uint fetched;
        while (ppenum.Next(1, aregion, out fetched) == NativeMethods.S_OK && fetched == 1)
        {
          var region = aregion[0];
          int regTypeInt;
          ErrorHandler.ThrowOnFailure(region.GetType(out regTypeInt));
          uint dwData;
          region.GetClientData(out dwData);
          var regType = (HIDDEN_REGION_TYPE)regTypeInt;
          if (regType != HIDDEN_REGION_TYPE.hrtCollapsible)// || dwData != 0 && dwData != HiddenRegionCookie)
            continue;

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
      if (wrapInTransaction)
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

    // ReSharper disable ParameterTypeCanBeEnumerable.Local
    private void RenameSymbolsInternal(string newName, GotoInfo[] usages)
    // ReSharper restore ParameterTypeCanBeEnumerable.Local
    {
      var distinctFilesIndices = (from us in usages select us.Location.FileIndex).Distinct();

      foreach (var fileIndex in distinctFilesIndices)
      {
        var source = ProjectInfo.GetSource(fileIndex) as NemerleSource;
        //VladD2: Этот код рассчитывает на то, что все исходники в которых производятся изменения открыты в редакторах VS!
        //VladD2: Это не верное предположение! 
        //TODO: Нужно переписать этот код так, чтобы он выполнялся без ошибок при любом исходе.
        // Тут есть два пути: 1) открывать все файлы в редакторах, 2) создать еще одну реализацию EditArray которая умела 
        // бы работать с IIdeSource. При этом нужно как-то поддерживать Undo/Redo.
        Trace.Assert(source != null);

        var mgr = new EditArray(source, null, true, "Renaming");
        int index = fileIndex;
        var thisFileUsages = usages.Where(use => use.Location.FileIndex == index);

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
      if (txt.Trim() == ";")
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

    public int LineCount
    {
      get
      {
        int lineCount;
        int hr1 = GetTextLines().GetLineCount(out lineCount);
        ErrorHandler.ThrowOnFailure(hr1);
        return lineCount;
      }
    }

    #endregion

    #region IIdeSource Members

    public int CurrentVersion { get { return TimeStamp; } }

    public TupleStringIntInt GetTextCurrentVersionAndFileIndex()
    {
      LockWrite();
      try { return new TupleStringIntInt(GetText(), CurrentVersion, FileIndex); }
      finally { UnlockWrite(); }
    }

    public void LockWrite() { TextLines.LockBufferEx((uint)BufferLockFlags.BLF_READ); }
    public void UnlockWrite() { TextLines.UnlockBufferEx((uint)BufferLockFlags.BLF_READ); }
    public void LockReadWrite() { TextLines.LockBufferEx((uint)BufferLockFlags.BLF_READ_AND_WRITE); }
    public void UnlocReadkWrite() { TextLines.UnlockBufferEx((uint)BufferLockFlags.BLF_READ_AND_WRITE); }

    public void SetTypeHighlighting(IList<Location> list, int sourceVersion)
    {
      if (CurrentVersion != sourceVersion)
        return;

      TypeLocations = list;

      Redraw();
    }

    public void SetMethodsTypeHighlighting(Location bodyLocation, List<Location> typeLocations, int sourceVersion)
    {
      if (CurrentVersion != sourceVersion)
        return;

      for (int i = 0; i < MethodsTypeLocations.Count; i++)
      {
        var methodsTypeLocation = MethodsTypeLocations[i];
        var loc = methodsTypeLocation.Item1;
        if (bodyLocation.IsIntersect(loc))
        {
          MethodsTypeLocations.RemoveAt(i);
          i--;
        }
      }

      MethodsTypeLocations.Add(Tuple.Create(bodyLocation, typeLocations));

      Redraw();
    }

    private void Redraw()
    {
      TypeClassifier typeClassifier;
      if (TextLines.ToITextBuffer().Properties.TryGetProperty(typeof (TypeClassifier), out typeClassifier))
        typeClassifier.RedrawTypeHighlighting();
    }

    public void SetUsageHighlighting(IEnumerable<GotoInfo> highlightings)
    {
      UsageClassifier usageClassifier;
      if (TextLines.ToITextBuffer().Properties.TryGetProperty(typeof(UsageClassifier), out usageClassifier))
        usageClassifier.UpdateUsageHighlighting(highlightings);
    }

    public void SetRegions(IList<RegionInfo> regions, int sourceVersion)
    {
      var newRegions = regions.Select(ri =>
        {
          var secondTime = RegionsLoaded;
          var location = ri.Location;
          var text = ri.Banner;
          var isExpanded = ri.Expanded;

          var r = new NewHiddenRegion
          {
            tsHiddenText = Utils.SpanFromLocation(location),
            iType = (int)HIDDEN_REGION_TYPE.hrtCollapsible,
            dwBehavior = (int)HIDDEN_REGION_BEHAVIOR.hrbEditorControlled, //.hrbClientControlled
            pszBanner = string.IsNullOrEmpty(text) ? null : text,
            dwClient = HiddenRegionCookie,
            dwState = (uint)(secondTime || isExpanded ? HIDDEN_REGION_STATE.hrsExpanded : HIDDEN_REGION_STATE.hrsDefault)
          };

          //if (text == "Toplevel typing")
          //{
          //  // VladD2: Debug staff
          //  var behavior = (HIDDEN_REGION_BEHAVIOR)r.dwBehavior;
          //  var dwClient = r.dwClient;
          //  var state = (HIDDEN_REGION_STATE)r.dwState;
          //  var type = (HIDDEN_REGION_TYPE)r.iType;
          //  var text1 = r.pszBanner;
          //  var loc = Utils.LocationFromSpan(location.FileIndex, r.tsHiddenText);
          //  Debug.Assert(true);
          //}

          return r;
        });

      ProcessHiddenRegions(newRegions.ToList(), sourceVersion);
    }

    public void SetTopDeclarations(TopDeclaration[] topDeclarations)
    {
      Declarations = topDeclarations;
    }

    public List<RelocationRequest> RelocationRequestsQueue
    {
      get { return _relocationRequestsQueue; }
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

    #endregion

    #region TipText

    private static string TextOfCompilerMessage(CompilerMessage cm)
    {
      var text = new StringBuilder(256);

      switch (cm.Kind)
      {
        case MessageKind.Error: text.Append("<font color=\"Red\"><b>Error:</b></font> "); break;
        case MessageKind.Hint: text.Append("<font color=\"Goldenrod\"><b>Hint:</b></font> "); break;
        case MessageKind.Warning: text.Append("<font color=\"DarkCyan\"><b>Warning:</b></font> "); break;
        default: break;
      }

      TextOfCompilerMessage(cm, text, 0);
      return text.ToString();
    }

    /// <summary>
    /// Return information about token which coordinates intersect with point (line, index)
    /// </summary>
    /// <param name="line">zero based index of line</param>
    /// <param name="index">zero based index of char</param>
    /// <returns>Token coordinate or span initialised with -1, if no token intersect with point</returns>
    public TextSpan GetTokenSpan(int line, int index)
    {
      //VladD2: VS требует от нас TextSpan содержащий координаты участка текста к которому относится hint.
      // В дальнейшем, если курсор мыши не выходит за этот TextSpan, VS не предпринимает попытки показать другой хинт,
      // даже если с нашей точки зрения требуется отображать другую информацию. Чтобы VS не "помогала" нам, вычисляем
      // локешон токена лежащего под запрашиваемой точкой и передаем его студии. Это приведет ктому, что VS будет 
      // повторно запрашивать hint, если курсор покинет пределы текущего токена.
      var token = GetTokenInfo(line, index + 1);  // GetTokenInfo() выдает информацию о предыдущем токене! +1 заставляет ее брать следующий
      if (token == null)
        return new TextSpan { iEndIndex = -1, iStartLine = -1, iStartIndex = -1, iEndLine = -1 };

      var hintSpan = new TextSpan();
      ErrorHelper.ThrowOnFailure(TextLines.GetLineIndexOfPosition(token.StartIndex, out hintSpan.iStartLine, out hintSpan.iStartIndex));
      ErrorHelper.ThrowOnFailure(TextLines.GetLineIndexOfPosition(token.EndIndex, out hintSpan.iEndLine, out hintSpan.iEndIndex));
      return hintSpan;
    }

    private static void TextOfCompilerMessage(CompilerMessage cm, StringBuilder text, int indent)
    {
      const string PosibleOverloadPref = "  Posible overload: ";

      if (cm.Msg.EndsWith("overload defination"))
        return;

      if (indent > 0)
      {
        text.AppendLine();
        text.Append(' ', indent);
      }

      indent += 2;

      string msg = Utils.HtmlMangling(cm.Msg);

      var len = msg.EndsWith("[simple require]", StringComparison.InvariantCulture) && msg.Contains(':') ? msg.LastIndexOf(':') : msg.Length;
      var start = msg.StartsWith(PosibleOverloadPref, StringComparison.InvariantCulture) ? PosibleOverloadPref.Length : 0;

      text.Append(msg.Substring(start, len - start));

      if (cm.IsRelatedMessagesPresent)
        foreach (var related in cm.RelatedMessages)
          TextOfCompilerMessage(related, text, indent);
    }

    private static string NemerleErrorTaskToString(NemerleErrorTask task)
    {
      return TextOfCompilerMessage(task.CompilerMessage);
    }

    internal int GetDataTipText(IVsTextView view, TextSpan[] textSpan, out string hintText)
    {
      hintText = null;

      if (Service.IsSmartTagActive)
        return (int)TipSuccesses2.TIP_S_NODEFAULTTIP;

      var loc = Utils.LocationFromSpan(FileIndex, textSpan[0]);

      if (_tipAsyncRequest == null || _tipAsyncRequest.Line != loc.Line || _tipAsyncRequest.Column != loc.Column)
      {
        //if (_typeNameMarker != null && _typeNameMarker.Location.Contains(loc.Line, loc.Column))
        //  ShowTypeNameSmartTag(view, false);
        _tipAsyncRequest = GetEngine().BeginGetQuickTipInfo(this, loc.Line, loc.Column);
        return VSConstants.E_PENDING;
      }
      if (!_tipAsyncRequest.IsCompleted)
        return VSConstants.E_PENDING;

      var tipInfo = _tipAsyncRequest.QuickTipInfo;
      _tipAsyncRequest = null;

      if (LanguageService.IsDebugging)
      {
        if (NeedDebugDataTip(tipInfo, textSpan))
        {
          hintText = "";
          return (int)TipSuccesses2.TIP_S_NODEFAULTTIP;
        }
      }

      var span = textSpan[0];

      //QuickTipInfo tipInfo = engine.GetQuickTipInfo(FileIndex, loc.Line, loc.Column);

      //Debug.WriteLine(loc.ToVsOutputStringFormat() + "GetDataTipText()");
      var projectInfo = ProjectInfo;

      if (projectInfo == null)
        return (int)TipSuccesses.TIP_S_ONLYIFNOMARKER;

      var taskLocation = IsSecondarySource ? MapSecondaryToPrimaryLocation(loc) : loc;

      var tasks = projectInfo.FindTaks(t => t.CompilerMessage.Location.Contains(taskLocation) && !t.CompilerMessage.IsRelated).ToList();

      if (tasks.Count == 0 && tipInfo == null)
        return (int)TipSuccesses.TIP_S_ONLYIFNOMARKER;

      var hintSpan = GetTokenSpan(span.iStartLine, span.iStartIndex);

      if (tipInfo != null)
      {
        hintText = tipInfo.Text;

        if (TextSpanHelper.IsEmpty(hintSpan))
          hintSpan = Utils.SpanFromLocation(tipInfo.Location);
      }

      if (tasks.Count > 0)
      {
        var locAgg = tasks.Aggregate(Location.Default, (loc1, t) => loc1.Combine(t.CompilerMessage.Location));
        var tasksMsgs = tasks.Select(t => NemerleErrorTaskToString(t));//.ToArray();

        //Debug.WriteLine(token.Type.ToString());
        if (TextSpanHelper.IsEmpty(hintSpan))
          hintSpan = Utils.SpanFromLocation(locAgg);

        if (hintText != null)
          hintText += Environment.NewLine;

        hintText += "<font color=\"Green\"><font face=\"Webdings\" size=\"22\">!</font> <b>Compiler messages:</b></font>"
              + Environment.NewLine + tasksMsgs.Join(Environment.NewLine);
      }

      textSpan[0] = hintSpan; // если не задать не пустой span пересекающийся с исхдным, VS не покажет hint.

      //Debug.WriteLine(Utils.LocationFromSpan(FileIndex, span).ToVsOutputStringFormat() + "result GetDataTipText() text:");
      //Debug.WriteLine(hintText);

      if (hintText != null)
        Service.ShowHint(view, hintSpan, tipInfo == null ? null : tipInfo.GetHintContent, hintText);

      return (int)TipSuccesses2.TIP_S_NODEFAULTTIP;
    }

    private bool NeedDebugDataTip(QuickTipInfo quickTipInfo, TextSpan[] textSpan)
    {
      IVsTextLines textLines = GetTextLines();

      // Now, check if the debugger is running and has anything to offer
      try
      {
        Microsoft.VisualStudio.Shell.Interop.IVsDebugger debugger = LanguageService.GetIVsDebugger();

        if (debugger == null || !LanguageService.IsDebugging || quickTipInfo == null || quickTipInfo.Location.IsEmpty)
          return false;

        string expr = null;
        var loc = quickTipInfo.Location;

        if (quickTipInfo.IsExpr)
        {
          expr = quickTipInfo.TExpr.ToString();
          loc = quickTipInfo.TExpr.Location;
        }

        var debugSpan = new[] { Utils.SpanFromLocation(loc) };

        string debugTextTip;
        int hr = debugger.GetDataTipValue(textLines, debugSpan, expr, out debugTextTip);

        if (hr == (int)TipSuccesses2.TIP_S_NODEFAULTTIP)
        {
          textSpan[0] = debugSpan[0];
          return true;
        }

        //Debug.Assert(false);
      }
      catch (COMException)
      {
      }

      return false;
    }

    #endregion
  }
}
