using System;
using Microsoft.VisualStudio.Text;

namespace Nemerle.VisualStudio.LanguageService
{
  partial class SyntaxClassifier
  {
    private struct SpanInfo : IComparable<SpanInfo>
    {
      public readonly Span Span;
      public readonly SpanType Type;

      public SpanInfo(Span span, SpanType type)
      {
        Span = span;
        Type = type;
      }

      public int CompareTo(SpanInfo other)
      {
        if (this.Span.Start < other.Span.Start)
          return -1;
        else if (this.Span.Start > other.Span.Start)
          return +1;
        else
          return 0;
      }
    }
  }
}
