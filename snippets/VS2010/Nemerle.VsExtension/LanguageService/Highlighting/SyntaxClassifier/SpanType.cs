
namespace Nemerle.VisualStudio.LanguageService
{
  partial class SyntaxClassifier
  {
    public enum SpanType : byte
    {
      Whitespace = 0,
      Identifier,
      Keyword,
      PreprocessorKeyword,
      Operator,
      Brace,
      Number,
      Character,
      SingleLineComment,
      MultiLineComment,
      String,
      VerbatimString,
      RecursiveString,
      Quotation,
      QuotationBrace,
      ToDoCommentText,
      BugCommentText,
      HackCommentText
    }
  }
}
