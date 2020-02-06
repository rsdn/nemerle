using Microsoft.VisualStudio.Text;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.LanguageService
{
  public sealed class ClassificationParseResult
  {
    public ClassificationParseResult(ITextSnapshot snapshot, Token tokens, Comment[] comments, Directive[] directives)
    {
      Snapshot = snapshot;
      Tokens = tokens;
      Comments = comments;
      Directives = directives;
    }

    public readonly ITextSnapshot Snapshot;
    public readonly Token Tokens;
    public readonly Comment[] Comments;
    public readonly Directive[] Directives;
  }
}
