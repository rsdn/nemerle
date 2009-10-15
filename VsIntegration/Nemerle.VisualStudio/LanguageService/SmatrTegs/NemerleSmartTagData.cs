using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Nemerle.VisualStudio.GUI.Native;
using Nemerle.Compiler;
using Microsoft.VisualStudio;

namespace Nemerle.VisualStudio.LanguageService
{
  class NemerleSmartTagData : IVsSmartTagData
  {
    readonly NemerleSource _source;
    readonly Location _loc;

    public NemerleSmartTagData(NemerleSource source, Location loc)
    {
      _source = source;
      _loc = loc;
    }

    #region IVsSmartTagData Members

    int IVsSmartTagData.GetContextMenuInfo(out Guid guidID, out int nMenuID, out IOleCommandTarget pCmdTarget)
    {
      guidID = Guid.Empty;
      nMenuID = 0;
      pCmdTarget = null;
      return VSConstants.E_NOTIMPL;
    }

    int IVsSmartTagData.GetContextStream(out int pos, out int length)
    {
      pos = _source.GetPositionOfLineIndex(_loc.Line, _loc.Column);
      var endPos   = _source.GetPositionOfLineIndex(_loc.EndLine, _loc.EndColumn);
      length = endPos - pos;
      return VSConstants.S_OK;
    }

    int IVsSmartTagData.GetImageIndex(out int piIndex)
    {
      piIndex = 2;
      return VSConstants.S_OK;
    }

    int IVsSmartTagData.GetTimerInterval(out int time)
    {
      time = -1;
      return VSConstants.E_NOTIMPL;
    }

    int IVsSmartTagData.GetTipText(out string pbstrTipText)
    {
      pbstrTipText = "pbstrTipText!!!";
      return VSConstants.S_OK;
    }

    int IVsSmartTagData.IsLeftJustified(out int pfIsLeftJustified)
    {
      pfIsLeftJustified = 1;
      return VSConstants.S_OK;
    }

    int IVsSmartTagData.OnDismiss()
    {
      return VSConstants.S_OK;
    }

    int IVsSmartTagData.OnInvocation()
    {
      return VSConstants.E_NOTIMPL;
    }

    int IVsSmartTagData.UpdateView()
    {
      return VSConstants.E_NOTIMPL;
    }

    #endregion
  }
}
