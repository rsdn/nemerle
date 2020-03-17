
using System;
using Microsoft.VisualStudio.Language.NavigateTo.Interfaces;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;

namespace Nemerle.VsExtension.NavigateTo
{
  [Export(typeof(INavigateToItemDisplayFactory))]
  class NavigateToItemDisplayFactory : INavigateToItemDisplayFactory
  {
    readonly IGlyphService    _glyphService;
    readonly IServiceProvider _serviceProvider;

    public NavigateToItemDisplayFactory(IServiceProvider serviceProvider, IGlyphService glyphService)
    {
      _serviceProvider = serviceProvider;
      _glyphService    = glyphService;
    }

    public INavigateToItemDisplay CreateItemDisplay(NavigateToItem item)
    {
      return new NavigateToItemDisplay(_serviceProvider, _glyphService, item);
    }
  }
}
