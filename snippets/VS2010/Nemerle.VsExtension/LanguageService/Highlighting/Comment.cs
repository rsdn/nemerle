
namespace Nemerle.VisualStudio.LanguageService
{
  public struct Comment
  {
    public Comment(Nemerle.Compiler.Comment comment, CommentType type, int textPosition)
    {
      Position     = comment.Location.StartPos;
      Length       = comment.Location.Length;
      IsDocument   = comment.IsDocument;
      IsMultiline  = comment.IsMultiline;
      Type         = type;
      TextPosition = textPosition;
    }

    public readonly int Position;
    public readonly int Length;
    public readonly bool IsDocument;
    public readonly bool IsMultiline;
    public readonly CommentType Type;
    public readonly int TextPosition;
    public int TextLength
    {
      get
      {
        var result = Length - (TextPosition - Position);
        return IsMultiline ? result - 2 : result;
      }
    }
  }
}
