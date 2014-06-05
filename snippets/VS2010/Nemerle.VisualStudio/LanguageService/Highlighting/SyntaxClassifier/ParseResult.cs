using Microsoft.VisualStudio.Text;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.LanguageService
{
  partial class SyntaxClassifier
  {
    private sealed class ParseResult
    {
      public ParseResult(ITextSnapshot snapshot, Token.BracesGroup tokens, Comment[] comments, Directive[] directives)
      {
        Snapshot = snapshot;
        Tokens = tokens;
        Comments = comments;
        Directives = directives;
      }

      public readonly ITextSnapshot Snapshot;
      public readonly Token.BracesGroup Tokens;
      public readonly Comment[] Comments;
      public readonly Directive[] Directives;
    }
  }
}
