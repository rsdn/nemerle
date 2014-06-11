using Microsoft.VisualStudio.Text;
using Nemerle.Compiler;
using System.Diagnostics;
using System.Text.RegularExpressions;

namespace Nemerle.VisualStudio.LanguageService
{
  public static class ParseUtil
  {
    private static readonly Regex _singleLineCommentParser = new Regex(@"^//\s*(TODO\s*:)|(BUG\s*:)|(HACK\s*:)", RegexOptions.Compiled | RegexOptions.CultureInvariant | RegexOptions.IgnoreCase);
    private static readonly Regex _multiLineCommentParser  = new Regex(@"^/\*\s*(TODO\s*:)|(BUG\s*:)|(HACK\s*:)", RegexOptions.Compiled | RegexOptions.CultureInvariant | RegexOptions.IgnoreCase);

    public static bool TryParse(ITextBuffer textBuffer, out ClassificationParseResult parseResult)
    {
      var snapshot = textBuffer.CurrentSnapshot;

      ClassificationParseResult lastParseResult;
      if (textBuffer.Properties.TryGetProperty<ClassificationParseResult>(typeof(ClassificationParseResult), out lastParseResult) && lastParseResult.Snapshot.Version == snapshot.Version)
      {
        parseResult = lastParseResult;
        return true;
      }

      NemerleSource source;
      if (!textBuffer.Properties.TryGetProperty<NemerleSource>(typeof(NemerleSource), out source))
      {
        parseResult = null;
        return false;
      }

      var engine = source.GetEngine();
      if (!engine.RequestOnInitEngine())
      {
        parseResult = null;
        return false;
      }

      var timer = Stopwatch.StartNew();

      var code = snapshot.GetText();
      var lexer = new LexerFile((ManagerClass)engine, 0, code, true);
      var preParser = new PreParser(lexer);
      var tokens = preParser.PreParse();
      var _comments = lexer.GetComments();
      var directives = lexer.GetDirectives();

      var comments = new Comment[_comments.Length];
      for (var i = 0; i < _comments.Length; ++i)
      {
        var c = _comments[i];
        var type = CommentType.Normal;
        var pos = 0;
        var commentParser = c.IsMultiline ? _multiLineCommentParser : _singleLineCommentParser;
        var match = commentParser.Match(code, c.Position, c.Length);
        if (match.Success)
        {
          if (match.Groups[1].Success)
          {
            pos = match.Groups[1].Index;
            type = CommentType.ToDo;
          }
          else if (match.Groups[2].Success)
          {
            pos = match.Groups[2].Index;
            type = CommentType.Bug;
          }
          else if (match.Groups[3].Success)
          {
            pos = match.Groups[3].Index;
            type = CommentType.Hack;
          }
        }
        comments[i] = new Comment(c, type, pos);
      }

      timer.Stop();
      Debug.Print("SyntaxClassifier.TryParse: {0}", timer.Elapsed);

      parseResult = new ClassificationParseResult(snapshot, tokens, comments, directives);
      textBuffer.Properties[typeof(ClassificationParseResult)] = parseResult;
      return true;
    }
  }
}
