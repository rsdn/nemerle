//using System;
//using System.Collections.Generic;
//using System.Diagnostics;

//using Microsoft.VisualStudio;
//using Microsoft.VisualStudio.TextManager.Interop;

//using Tuple2 = Nemerle.Builtins.Tuple<int, int>;

//using IIdeSource = Nemerle.Completion2.IIdeSource;

//namespace Nemerle.VisualStudio.LanguageService
//{
//  class SourceTextManager : IIdeSource
//  {
//    public int FileIndex { get { return _source.FileIndex; } }

//    public SourceTextManager(NemerleSource source)
//    {
//      _source  = source;
//    }

//    public CompileUnit CompileUnit 
//    { 
//      get { return _source.CompileUnit; }
//      set { _source.CompileUnit = value; }
//    }
    
//    public  IVsTextLines  TextLines
//    {
//      get { return Source.GetTextLines(); }
//    }

//    private NemerleSource _source;
//    public  NemerleSource  Source
//    {
//      get { return _source; }
//    }

//    public string GetText()
//    {
//      return Source.GetText();
//    }

//    public string GetRegion(int lineStart, int colStart, int lineEnd, int colEnd)
//    {
//      return Source.GetText(lineStart - 1, colStart - 1, lineEnd - 1, colEnd - 1);
//    }

//    public string GetRegion(Location loc)
//    {
//      return GetRegion(loc.Line, loc.Column, loc.EndLine, loc.EndColumn);
//    }

//    /// <summary>
//    /// Get text of line frome text bufer of IDE.
//    /// </summary>
//    /// <param name="line">Line position (first line is 1).</param>
//    /// <returns>The text of line.</returns>
//    public string GetLine(int line)
//    {
//      line--; // Convert to zero based index.

//#if DEBUG
//      //int lineCount = LineCount;

//      //if (line >= lineCount) // just for debugging purpose.
//      //	Debug.Assert(line < lineCount);
//#endif

//      return Source.GetLine(line);
//    }

//    public int GetPositionOfLineIndex(int line, int col)
//    {
//      return Source.GetPositionOfLineIndex(line - 1, col - 1);
//    }

//    public Tuple2 GetLineIndexOfPosition(int pos)
//    {
//      int line, col;
			
//      Source.GetLineIndexOfPosition(pos, out line, out col);
			
//      return new Tuple2(line + 1, col + 1);
//    }

//    public int LineCount
//    {
//      get
//      {
//        int lineCount;
//        int hr1 = Source.GetTextLines().GetLineCount(out lineCount);

//        ErrorHandler.ThrowOnFailure(hr1);

//        return lineCount;
//      }
//    }

//    public void SetRegions(IList<RegionInfo> regions)
//    {
//    }
//  } // and of SourceTextManager class
//} // and of Nemerle.VisualStudio.LanguageService namespace
