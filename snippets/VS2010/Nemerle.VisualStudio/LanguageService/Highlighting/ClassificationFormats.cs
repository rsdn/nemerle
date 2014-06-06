using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;
using System.Windows.Media;

namespace Nemerle.VisualStudio.LanguageService
{
  internal static class ClassificationFormats
  {
    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle String Interpolation")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.StringExName)
    , UserVisible(true)]
    public sealed class StringEx : ClassificationFormatDefinition
    {
      public StringEx() { ForegroundColor = Color.FromArgb(0, 143, 44, 182); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle String Interpolation (@ Verbatim)")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.VerbatimStringExName)
    , UserVisible(true)]
    public sealed class VerbatimStringEx : ClassificationFormatDefinition
    {
      public VerbatimStringEx() { BackgroundColor = Color.FromArgb(0, 250, 232, 232); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle String Interpolation (<# #>)")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.RecursiveStringExName)
    , UserVisible(true)]
    public sealed class RecursiveStringEx : ClassificationFormatDefinition
    {
      public RecursiveStringEx() { BackgroundColor = Color.FromArgb(0, 250, 232, 232); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle String (@ Verbatim)")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.VerbatimStringName)
    , UserVisible(true)]
    public sealed class VerbatimString : ClassificationFormatDefinition
    {
      public VerbatimString() { BackgroundColor = Color.FromArgb(0, 250, 232, 232); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle String (<# #>)")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.RecursiveStringName)
    , UserVisible(true)]
    public sealed class RecursiveString : ClassificationFormatDefinition
    {
      public RecursiveString() { BackgroundColor = Color.FromArgb(0, 250, 232, 232); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle User Type")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.UserTypeName)
    , UserVisible(true)]
    public sealed class UserType : ClassificationFormatDefinition
    {
      public UserType() { ForegroundColor = Color.FromArgb(0, 43, 145, 175); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle User Delegate Type")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.UserDelegateTypeName)
    , UserVisible(true)]
    public sealed class UserDelegateType : ClassificationFormatDefinition
    {
      public UserDelegateType() { ForegroundColor = Color.FromArgb(0, 43, 145, 175); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle User Enum Type")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.UserEnumTypeName)
    , UserVisible(true)]
    public sealed class UserEnumType : ClassificationFormatDefinition
    {
      public UserEnumType() { ForegroundColor = Color.FromArgb(0, 43, 145, 175); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle User Interface Type")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.UserInterfaceTypeName)
    , UserVisible(true)]
    public sealed class UserInterfaceType : ClassificationFormatDefinition
    {
      public UserInterfaceType() { ForegroundColor = Color.FromArgb(0, 43, 145, 175); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle User Value Type")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.UserValueTypeName)
    , UserVisible(true)]
    public sealed class UserValueType : ClassificationFormatDefinition
    {
      public UserValueType() { ForegroundColor = Color.FromArgb(0, 43, 145, 175); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle Highlight One")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.HighlightOneName)
    , UserVisible(true)]
    public sealed class HighlightOne : ClassificationFormatDefinition
    {
      public HighlightOne() { BackgroundColor = Color.FromArgb(0, 135, 206, 250); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle Highlight Two")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.HighlightTwoName)
    , UserVisible(true)]
    public sealed class HighlightTwo : ClassificationFormatDefinition
    {
      public HighlightTwo() { BackgroundColor = Color.FromArgb(0, 255, 182, 193); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle Comment (TODO)")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.ToDoCommentName)
    , UserVisible(true)]
    public sealed class ToDoComment : ClassificationFormatDefinition
    {
      public ToDoComment() { ForegroundColor = Color.FromArgb(0, 0, 175, 255); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle Comment (BUG)")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.BugCommentName)
    , UserVisible(true)]
    public sealed class BugComment : ClassificationFormatDefinition
    {
      public BugComment() { ForegroundColor = Color.FromArgb(0, 255, 75, 75); IsBold = true; }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle Comment (HACK)")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.HackCommentName)
    , UserVisible(true)]
    public sealed class HackComment : ClassificationFormatDefinition
    {
      public HackComment() { ForegroundColor = Color.FromArgb(0, 145, 0, 0); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle Field Identifier")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.FieldIdentifierName)
    , UserVisible(true)]
    public sealed class FieldIdentifier : ClassificationFormatDefinition
    {
      public FieldIdentifier() { ForegroundColor = Color.FromArgb(0, 128, 0, 128); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle Event Identifier")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.EventIdentifierName)
    , UserVisible(true)]
    public sealed class EventIdentifier : ClassificationFormatDefinition
    {
      public EventIdentifier() { ForegroundColor = Color.FromArgb(0, 255, 0, 255); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle Method Identifier")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.MethodIdentifierName)
    , UserVisible(true)]
    public sealed class MethodIdentifier : ClassificationFormatDefinition
    {
      public MethodIdentifier() { ForegroundColor = Color.FromArgb(0, 0, 139, 139); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle Property Identifier")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.PropertyIdentifierName)
    , UserVisible(true)]
    public sealed class PropertyIdentifier : ClassificationFormatDefinition
    {
      public PropertyIdentifier() { ForegroundColor = Color.FromArgb(0, 128, 0, 128); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle Quotation Braces")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.QuotationBracesName)
    , UserVisible(true)]
    public sealed class QuotationBraces : ClassificationFormatDefinition
    {
      public QuotationBraces() { ForegroundColor = Color.FromArgb(0, 128, 128, 0); }
    }

    [Export(typeof(EditorFormatDefinition))
    , Name("Nemerle Quotation Content")
    , ClassificationType(ClassificationTypeNames = ClassificationTypes.QuotationName)
    , UserVisible(true)]
    public sealed class QuotationContent : ClassificationFormatDefinition
    {
      public QuotationContent() { BackgroundColor = Color.FromArgb(0, 230, 237, 228); }
    }
  }
}
