
namespace Nemerle.VisualStudio.LanguageService
{
  partial class SyntaxClassifier
  {
    private enum SpanType : byte
    {
      Whitespace = 0,
      Identifier,
      Keyword,
      PreprocessorKeyword,
      Operator,
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
