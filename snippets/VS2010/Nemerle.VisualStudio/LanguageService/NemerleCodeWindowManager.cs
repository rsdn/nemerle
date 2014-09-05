using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;

namespace Nemerle.VisualStudio.LanguageService
{
  class NemerleCodeWindowManager : CodeWindowManager
  {
    private readonly NemerleSource _source;

    public NemerleCodeWindowManager(Microsoft.VisualStudio.Package.LanguageService service, IVsCodeWindow codeWindow, NemerleSource source)
      : base(service, codeWindow, source)
    {
      _source = source;
    }

    public override int AddAdornments()
    {
      _source.AddRef();
      return base.AddAdornments();
    }

    public override int RemoveAdornments()
    {
      _source.ReleaseRef();
      return base.RemoveAdornments();
    }
  }
}
