
using System;
using Microsoft.VisualStudio.Language.NavigateTo.Interfaces;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;
using System.Collections.Generic;

namespace Nemerle.VsExtension.NavigateTo
{
  class NavigateToItemProvider : INavigateToItemProvider2
  {
    public  readonly IServiceProvider                  ServiceProvider;
    public  readonly IGlyphService                     GlyphService;

    public NavigateToItemProvider(IServiceProvider serviceProvider, IGlyphService glyphService)
    {
      ServiceProvider = serviceProvider;
      GlyphService    = glyphService;
    }

    public NavigateToItemDisplayFactory GetFactory()
    {
      return new NavigateToItemDisplayFactory(ServiceProvider, GlyphService);
    }

    public void StartSearch(INavigateToCallback callback, string searchValue)
    {
      StartSearch(callback, searchValue, true, false, new SortedSet<string>());
    }

    public void StopSearch()
    {
    }

    public void Dispose()
    {
    }

    // INavigateToItemProvider2

    public ISet<string> KindsProvided => new HashSet<string>() { "OtherSymbol", "NemerleSymbol" };
    public bool         CanFilter     => true;

    public void StartSearch(INavigateToCallback callback, string searchValue, INavigateToFilterParameters filter)
    {
      var options = (INavigateToOptions2)callback.Options;
      StartSearch(callback, searchValue, options.HideExternalItems, options.SearchCurrentDocument, filter.Kinds);
    }

    private void StartSearch(INavigateToCallback callback, string pattern, bool hideExternalItems, bool searchCurrentDocument, ISet<string> kinds)
    {
            foreach (var projectInfo in VisualStudio.Project.ProjectInfo.Projects)
            {
                projectInfo.Engine.BeginFindAllSymbols(Tuple.Create(this, callback));
            }
    }
  }
}
