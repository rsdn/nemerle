using Microsoft.VisualStudio.Language.NavigateTo.Interfaces;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;

namespace Nemerle.VsExtension.NavigateTo
{
  [Name("Navigate To Nemerle Symbol"), Order(After = "Navigate To Class"), Export(typeof(FilterDefinition))]
  internal sealed class NavigateToSymbolFilter : KindFilterDefinition
  {
    public NavigateToSymbolFilter()
    {
      base.Kind = "NemerleSymbol";
    }
  }
}
