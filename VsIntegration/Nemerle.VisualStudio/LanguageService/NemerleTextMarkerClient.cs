using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio;
using Nemerle.Compiler;
using System.Diagnostics;

namespace Nemerle.VisualStudio.LanguageService
{
	class NemerleTextMarkerClient : IVsTextMarkerClient, IDisposable
	{
    static int markerType = 0;

		public NemerleTextMarkerClient(IVsTextLines buffer, Location loc)
		{
			var markerRef = new IVsTextLineMarker[1];
      if (markerType > (int)MARKERTYPE2.DEF_MARKER_COUNT_NEW)
        markerType = 0;

      if (markerType > (int)MARKERTYPE.DEF_MARKER_COUNT)
        Debug.WriteLine("MARKERTYPE2: " + Enum.Format(typeof(MARKERTYPE2), markerType, "F"));
      else
        Debug.WriteLine("MARKERTYPE: " + Enum.Format(typeof(MARKERTYPE), markerType, "F"));

      var iMarkerType = markerType; // (int)MARKERTYPE2.MARKER_TRACK_NONSAVE;
      markerType++;
      
			var hr = buffer.CreateLineMarker(iMarkerType, loc.Line - 1, loc.Column - 1, 
				         loc.EndLine - 1, loc.EndColumn - 1, this, markerRef);

			Debug.Assert(hr == 0);

			if (hr == 0) // S_OK
			{
				TextLineMarker = markerRef[0];
				string[] ss = new string[1];
				uint i;
				TextLineMarker.GetVisualStyle(out i);
				//TextLineMarker.SetVisualStyle((uint)MARKERVISUAL.MV_SEL_MARGIN_GLYPH);
        Debug.Assert(true);
			}
		}

		public IVsTextLineMarker TextLineMarker { get; internal set; }

		#region IDisposable Members

		public void Dispose()
		{
			DisposeTextLineMarker();
		}

		#endregion


		#region IVsTextMarkerClient Members

		int IVsTextMarkerClient.ExecMarkerCommand(IVsTextMarker pMarker, int iItem)
		{
			return VSConstants.S_OK;
		}

		int IVsTextMarkerClient.GetMarkerCommandInfo(IVsTextMarker pMarker, int iItem, string[] text, uint[] commandFlags)
		{
			// Returning S_OK results in error message appearing in editor's
			// context menu when you right click over the error message.
			if (commandFlags != null && commandFlags.Length > 0)
				commandFlags[0] = 0;
			if (text != null && text.Length > 0)
				text[0] = "aaaaaaaaaa";

			//return VSConstants.E_NOTIMPL;
			return VSConstants.S_OK;
		}

		int IVsTextMarkerClient.GetTipText(IVsTextMarker pMarker, string[] tipText)
		{
			//if (Text != null && Text.Length > 0)
				tipText[0] = "test";//Text;

			return VSConstants.S_OK;
		}

		public void DisposeTextLineMarker()
		{
			if (TextLineMarker != null)
			{
        TextLineMarker.Invalidate();
				TextLineMarker.UnadviseClient();
				TextLineMarker = null;
			}
		}

		void IVsTextMarkerClient.MarkerInvalidated()
    {
      //DisposeTextLineMarker();
    }

		int IVsTextMarkerClient.OnAfterMarkerChange(IVsTextMarker pMarker)
		{
			return VSConstants.S_OK;
		}

		void IVsTextMarkerClient.OnAfterSpanReload() { }
		void IVsTextMarkerClient.OnBeforeBufferClose() { }
		void IVsTextMarkerClient.OnBufferSave(string pszFileName) { }

		#endregion
	}
}
