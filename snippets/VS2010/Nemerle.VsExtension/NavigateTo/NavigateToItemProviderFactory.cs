
using System;
using Microsoft.VisualStudio.Language.NavigateTo.Interfaces;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;

namespace Nemerle.VsExtension.NavigateTo
{
  [Export(typeof(INavigateToItemProviderFactory))]
  class NavigateToItemProviderFactory : INavigateToItemProviderFactory
  {
    readonly IGlyphService _glyphService;

    [ImportingConstructor]
    public NavigateToItemProviderFactory(IGlyphService glyphService)
    {
      _glyphService = glyphService;
    }

    public bool TryCreateNavigateToItemProvider(IServiceProvider serviceProvider, out INavigateToItemProvider provider)
    {
      provider = new NavigateToItemProvider(serviceProvider, _glyphService);
      return true;
    }
  }
}
