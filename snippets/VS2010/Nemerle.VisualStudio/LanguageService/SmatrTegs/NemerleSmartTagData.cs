using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Nemerle.VisualStudio.GUI.Native;
using Nemerle.Compiler;
using Microsoft.VisualStudio;
using Nemerle.VisualStudio.Project;
using T = Nemerle.VisualStudio.Project.MenuCmd.CmdId;

namespace Nemerle.VisualStudio.LanguageService
{
	class NemerleSmartTagData : IVsSmartTagData, IOleCommandTarget
	{
		readonly Guid          _cmdSet;
		readonly int           _pos;
		readonly int           _length;
		readonly Action<T>     _exec;
		readonly Func<T, uint> _queryStatus;
		readonly Action        _onDismiss;


		public NemerleSmartTagData(
			int           pos,
			int           length,
			Guid          cmdSet,
			Action<T>     exec,
			Func<T, uint> queryStatus,
			Action        onDismiss)
		{
			ErrorHelper.ThrowIsNull(cmdSet, "cmdSet");

			_cmdSet      = cmdSet;
			_pos         = pos;
			_length      = length;
			_exec        = exec;
			_queryStatus = queryStatus;
			_onDismiss   = onDismiss;
		}

		#region IVsSmartTagData Members

		int IVsSmartTagData.GetContextMenuInfo(out Guid guidID, out int nMenuID, out IOleCommandTarget pCmdTarget)
		{
			guidID     = MenuCmd.guidNemerleProjectCmdSet;
			nMenuID    = (int)MenuCmd.CmdId.SmartTagContextMenu;
			pCmdTarget = this;

			return VSConstants.S_OK;
		}

		int IVsSmartTagData.GetContextStream(out int pos, out int length)
		{
			pos    = _pos;
			length = _length;

			return VSConstants.S_OK;
		}

		int IVsSmartTagData.GetImageIndex(out int piIndex)
		{
			piIndex = 2;
			return VSConstants.S_OK;
		}

		int IVsSmartTagData.GetTimerInterval(out int time)
		{
			time = 0;
			return VSConstants.S_OK;
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
			if (_onDismiss != null)
				_onDismiss();
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

		#region IOleCommandTarget Members

		int IOleCommandTarget.Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
		{
			if (_exec != null && pguidCmdGroup == _cmdSet)
				_exec((T)nCmdID);

			return VSConstants.S_OK;
		}

		int IOleCommandTarget.QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
		{
			if (_queryStatus != null && _cmdSet == pguidCmdGroup)
				for (int i = 0; i < prgCmds.Length; i++)
					prgCmds[i].cmdf = _queryStatus((T)prgCmds[i].cmdID);

			return VSConstants.S_OK;
		}

		#endregion
	}
}
