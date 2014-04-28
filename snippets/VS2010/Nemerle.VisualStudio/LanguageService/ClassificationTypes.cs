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

		// Quotation types

		public const string QuotationTextName = "Nemerle Quotation Text";

		[Export, Name(QuotationTextName)]
		public static ClassificationTypeDefinition QuotationText = null;

		public const string QuotationKeywordName = "Nemerle Quotation Keyword";

		[Export, Name(QuotationKeywordName), BaseDefinition(PredefinedClassificationTypeNames.Keyword)]
		public static ClassificationTypeDefinition QuotationKeyword = null;

		public const string QuotationCommentName = "Nemerle Quotation Comment";

		[Export, Name(QuotationCommentName), BaseDefinition(PredefinedClassificationTypeNames.Comment)]
		public static ClassificationTypeDefinition QuotationComment = null;

		public const string QuotationIdentifierName = "Nemerle Quotation Identifier";

		[Export, Name(QuotationIdentifierName), BaseDefinition(PredefinedClassificationTypeNames.Identifier)]
		public static ClassificationTypeDefinition QuotationIdentifier = null;

		public const string QuotationStringName = "Nemerle Quotation String";

		[Export, Name(QuotationStringName), BaseDefinition(PredefinedClassificationTypeNames.String)]
		public static ClassificationTypeDefinition QuotationString = null;

		public const string QuotationNumberName = "Nemerle Quotation Number";

		[Export, Name(QuotationNumberName), BaseDefinition(PredefinedClassificationTypeNames.Number)]
		public static ClassificationTypeDefinition QuotationNumber = null;

		public const string QuotationOperatorName = "Nemerle Quotation Operator";

		[Export, Name(QuotationOperatorName), BaseDefinition(PredefinedClassificationTypeNames.Operator)]
		public static ClassificationTypeDefinition QuotationOperator = null;

		public const string QuotationStringExName = "Nemerle Quotation StringEx";

		[Export, Name(QuotationStringExName), BaseDefinition(StringExName)]
		public static ClassificationTypeDefinition QuotationStringEx = null;

		public const string QuotationVerbatimStringName = "Nemerle Quotation VerbatimString";

		[Export, Name(QuotationVerbatimStringName), BaseDefinition(VerbatimStringName)]
		public static ClassificationTypeDefinition QuotationVerbatimString = null;

		public const string QuotationVerbatimStringExName = "Nemerle Quotation StringEx (@ Verbatim)";

		[Export, Name(QuotationVerbatimStringExName), BaseDefinition(VerbatimStringExName)]
		public static ClassificationTypeDefinition QuotationVerbatimStringEx = null;

		public const string QuotationRecursiveStringName = "Nemerle Quotation String (<# #>)";

		[Export, Name(QuotationRecursiveStringName), BaseDefinition(RecursiveStringName)]
		public static ClassificationTypeDefinition QuotationRecursiveString = null;

		public const string QuotationRecursiveStringExName = "Nemerle Quotation StringEx (<# #>)";

		[Export, Name(QuotationRecursiveStringExName), BaseDefinition(RecursiveStringExName)]
		public static ClassificationTypeDefinition QuotationRecursiveStringEx = null;

		public const string QuotationUserTypeName = "Nemerle Quotation User Types";

		[Export, Name(QuotationUserTypeName), BaseDefinition(UserTypeName)]
		public static ClassificationTypeDefinition QuotationUserType = null;

		public const string QuotationUserDelegateTypeName = "Nemerle Quotation User Types (Delegates)";

		[Export, Name(QuotationUserDelegateTypeName), BaseDefinition(UserDelegateTypeName)]
		public static ClassificationTypeDefinition QuotationUserDelegateType = null;

		public const string QuotationUserEnumTypeName = "Nemerle Quotation User Types (Enums)";

		[Export, Name(QuotationUserEnumTypeName), BaseDefinition(UserEnumTypeName)]
		public static ClassificationTypeDefinition QuotationUserEnumType = null;

		public const string QuotationUserInterfaceTypeName = "Nemerle Quotation User Types (Interfaces)";

		[Export, Name(QuotationUserInterfaceTypeName), BaseDefinition(UserInterfaceTypeName)]
		public static ClassificationTypeDefinition QuotationUserInterfaceType = null;

		public const string QuotationUserValueTypeName = "Nemerle Quotation User Types (Value types)";

		[Export, Name(QuotationUserValueTypeName), BaseDefinition(UserValueTypeName)]
		public static ClassificationTypeDefinition QuotationUserValueType = null;

		public const string QuotationToDoCommentName = "Nemerle Quotation TODO Comment";

		[Export, Name(QuotationToDoCommentName), BaseDefinition(ToDoCommentName)]
		public static ClassificationTypeDefinition QuotationToDoComment = null;

		public const string QuotationBugCommentName = "Nemerle Quotation BUG Comment";

		[Export, Name(QuotationBugCommentName), BaseDefinition(BugCommentName)]
		public static ClassificationTypeDefinition QuotationBugComment = null;

		public const string QuotationHackCommentName = "Nemerle Quotation HACK Comment";

		[Export, Name(QuotationHackCommentName), BaseDefinition(HackCommentName)]
		public static ClassificationTypeDefinition QuotationHackComment = null;
	}
}
