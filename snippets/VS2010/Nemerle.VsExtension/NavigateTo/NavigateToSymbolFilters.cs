using Microsoft.VisualStudio.Language.NavigateTo.Interfaces;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Imaging;

namespace Nemerle.VsExtension.NavigateTo
{
  internal class NavigateToSymbolFilters
  {
    [Name("Navigate To Nemerle Symbol"), Order(After = "Navigate To Class"), Export(typeof(FilterShortcutDefinition))]
    public sealed class SymbolFilterShortcut : FilterShortcutDefinition
    {
      public SymbolFilterShortcut()
      {
        base.ActivationSequence = "n";
        base.IsDelimiterRequired = true;
        base.Description = "desc";
        base.IsExclusive = true;
        base.ShouldSearchImmediately = true;
        base.Button = new ButtonDefinition(KnownMonikers.IntellisenseKeyword, "Nemerle Symbols", null);
      }
    }

    [Filter("Navigate To All Nemerle Symbol"), FilterShortcut("Navigate To Nemerle Symbol"), Export]
    internal FilterToShortcutDefinition _symbolFilterMapping;
  }
}
