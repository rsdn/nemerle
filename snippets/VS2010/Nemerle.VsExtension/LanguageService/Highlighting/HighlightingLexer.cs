using Nemerle.Core;
using Nemerle.Compiler;
using Nemerle.Completion2;

namespace Nemerle.VisualStudio.LanguageService
{
  internal sealed class HighlightingLexer : Lexer
  {
    public HighlightingLexer(IIdeEngine engine, SourceSnapshot sourceSnapshot)
      : base((ManagerClass)engine, sourceSnapshot)
    {
    }

    protected override void OnAfterUsingDirectiveParse(
      Location location,
      list<string> name,
      list<Location> nameLocations,
      string alias,
      Location aliasLocation,
      GlobalEnv beforeEnv,
      GlobalEnv afterEnv
    )
    {
      // do not notify Manager
    }

    protected override void OnBeforeNamespaceParse()
    {
      // do not notify Manager
    }

    protected override void OnAfterNamespaceParse(
      Location location,
      list<string> name,
      list<Location> nameLocations,
      GlobalEnv outsideEnv,
      GlobalEnv insideEnv,
      Location headerLocation,
      Location bodyOpenTokenLocation,
      Location bodyCloseTokenLocation
    )
    {
      // do not notify Manager
    }
  }
}
