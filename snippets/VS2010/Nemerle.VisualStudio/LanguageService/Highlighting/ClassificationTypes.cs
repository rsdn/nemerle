using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace Nemerle.VisualStudio.LanguageService
{
  internal static class ClassificationTypes
  {
    public const string StringExName = "Nemerle StringEx";

    [Export, Name(StringExName)]
    public static ClassificationTypeDefinition StringEx = null;

    public const string VerbatimStringName = "Nemerle String (@ Verbatim)";

    [Export, Name(VerbatimStringName), BaseDefinition(PredefinedClassificationTypeNames.String)]
    public static ClassificationTypeDefinition VerbatimString = null;

    public const string VerbatimStringExName = "Nemerle StringEx (@ Verbatim)";

    [Export, Name(VerbatimStringExName), BaseDefinition(StringExName)]
    public static ClassificationTypeDefinition VerbatimStringEx = null;

    public const string RecursiveStringName = "Nemerle String (<# #>)";

    [Export, Name(RecursiveStringName), BaseDefinition(PredefinedClassificationTypeNames.String)]
    public static ClassificationTypeDefinition RecursiveString = null;

    public const string RecursiveStringExName = "Nemerle StringEx (<# #>)";

    [Export, Name(RecursiveStringExName), BaseDefinition(StringExName)]
    public static ClassificationTypeDefinition RecursiveStringEx = null;

    public const string UserTypeName = "Nemerle User Types";

    [Export, Name(UserTypeName)]
    public static ClassificationTypeDefinition UserType = null;

    public const string UserDelegateTypeName = "Nemerle User Types (Delegates)";

    [Export, Name(UserDelegateTypeName)]
    public static ClassificationTypeDefinition UserDelegateType = null;

    public const string UserEnumTypeName = "Nemerle User Types (Enums)";

    [Export, Name(UserEnumTypeName)]
    public static ClassificationTypeDefinition UserEnumType = null;

    public const string UserInterfaceTypeName = "Nemerle User Types (Interfaces)";

    [Export, Name(UserInterfaceTypeName)]
    public static ClassificationTypeDefinition UserInterfaceType = null;

    public const string UserValueTypeName = "Nemerle User Types (Value types)";

    [Export, Name(UserValueTypeName)]
    public static ClassificationTypeDefinition UserValueType = null;

    public const string QuotationName = "Nemerle Quotation";

    [Export, Name(QuotationName)]
    public static ClassificationTypeDefinition Quotation = null;

    public const string QuotationBracesName = "Nemerle Quotation Braces";

    [Export, Name(QuotationBracesName)]
    public static ClassificationTypeDefinition QuotationBraces = null;

    public const string MathingBracesName = "Nemerle Matching Braces";

    [Export, Name(MathingBracesName)]
    public static ClassificationTypeDefinition MathingBraces = null;

    public const string HighlightOneName = "Nemerle Highlight One";

    [Export, Name(HighlightOneName)]
    public static ClassificationTypeDefinition HighlightOne = null;

    public const string HighlightTwoName = "Nemerle Highlight Two";

    [Export, Name(HighlightTwoName)]
    public static ClassificationTypeDefinition HighlightTwo = null;

    public const string ToDoCommentName = "Nemerle TODO Comment";

    [Export, Name(ToDoCommentName), BaseDefinition(PredefinedClassificationTypeNames.Comment)]
    public static ClassificationTypeDefinition ToDoComment = null;

    public const string BugCommentName = "Nemerle BUG Comment";

    [Export, Name(BugCommentName), BaseDefinition(PredefinedClassificationTypeNames.Comment)]
    public static ClassificationTypeDefinition BugComment = null;

    public const string HackCommentName = "Nemerle HACK Comment";

    [Export, Name(HackCommentName), BaseDefinition(PredefinedClassificationTypeNames.Comment)]
    public static ClassificationTypeDefinition HackComment = null;

    public const string FieldIdentifierName = "Nemerle Field Identifier";

    [Export, Name(FieldIdentifierName)]
    public static ClassificationTypeDefinition FieldIdentifier = null;

    public const string EventIdentifierName = "Nemerle Event Identifier";

    [Export, Name(EventIdentifierName)]
    public static ClassificationTypeDefinition EventIdentifier = null;

    public const string MethodIdentifierName = "Nemerle Method Identifier";

    [Export, Name(MethodIdentifierName)]
    public static ClassificationTypeDefinition MethodIdentifier = null;

    public const string PropertyIdentifierName = "Nemerle Property Identifier";

    [Export, Name(PropertyIdentifierName)]
    public static ClassificationTypeDefinition PropertyIdentifier = null;
  }
}
