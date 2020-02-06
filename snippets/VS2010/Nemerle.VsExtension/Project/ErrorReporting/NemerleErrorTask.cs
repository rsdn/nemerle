using System;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Shell;
using Nemerle.VisualStudio.Project;
using Nemerle.Compiler;
using System.Diagnostics;
using Microsoft.VisualStudio;

namespace Nemerle.VisualStudio.Package
{
	public class NemerleErrorTask : ErrorTask, IVsTextMarkerClient, IDisposable
	{
		public NemerleErrorTask(ProjectInfo projectInfo, CompilerMessage compilerMessage, EventHandler navigateHandler)
			: this(projectInfo, compilerMessage)
		{
			Navigate += navigateHandler;
		}

		public NemerleErrorTask(ProjectInfo projectInfo, CompilerMessage compilerMessage)
		{
			ErrorHelper.ThrowIsNull(projectInfo,     "projectInfo");
			ErrorHelper.ThrowIsNull(compilerMessage, "compilerMessage");

			ProjectInfo     = projectInfo;
			CompilerMessage = compilerMessage;

			var loc = compilerMessage.Location;

			if (loc.FileIndex != 0)
			{
				var span = Utils.SpanFromLocation(loc);
				Document = loc.File;
				Line     = span.iStartLine;
				Column   = span.iStartIndex;
			}

			Text = compilerMessage.Msg;

			switch (compilerMessage.Kind)
			{
				case MessageKind.Error:
					Priority = TaskPriority.High;
					ErrorCategory = TaskErrorCategory.Error;
					break;
				case MessageKind.Warning:
					Priority = TaskPriority.Normal;
					ErrorCategory = TaskErrorCategory.Warning;
					break;
				default:
					Priority = TaskPriority.Low;
					ErrorCategory = TaskErrorCategory.Message;
					break;
			}

			Category = TaskCategory.BuildCompile;
			HierarchyItem = projectInfo.ProjectNode.InteropSafeHierarchy;
		}

		public CompilerMessage   CompilerMessage { get; private set; }
		public ProjectInfo       ProjectInfo     { get; private set; }

		IVsTextLineMarker _textLineMarker;
		public IVsTextLineMarker TextLineMarker
		{
			get { return _textLineMarker; }
			//private set { _textLineMarker = value; }
		}

		public void MakeTextMarker(IVsTextLines buffer)
		{
			Trace.Assert(TextLineMarker == null);
			if (TextLineMarker != null)
				DisposeTextLineMarker();

			var span = Utils.SpanFromLocation(CompilerMessage.Location);
			int markerType;
			switch (CompilerMessage.Kind)
			{
				case MessageKind.Error:   markerType = (int)MARKERTYPE.MARKER_CODESENSE_ERROR;  break;
				case MessageKind.Warning: markerType = (int)MARKERTYPE.MARKER_COMPILE_ERROR;    break;
				default:                  markerType = (int)MARKERTYPE2.MARKER_WARNING;         break;
			}

			// create marker so task item navigation works even after file is edited.
			IVsTextLineMarker[] marker = new IVsTextLineMarker[1];
			// bugbug: the following comment in the method CEnumMarkers::Initialize() of
			// ~\env\msenv\textmgr\markers.cpp means that tool tips on empty spans
			// don't work:
			//      "VS7 #23719/#15312 [CFlaat]: exclude adjacent markers when the target span is non-empty"
			// So I wonder if we should debug assert on that or try and modify the span
			// in some way to make it non-empty...
			ErrorHandler.ThrowOnFailure(buffer.CreateLineMarker(markerType, span.iStartLine, span.iStartIndex, 
				span.iEndLine, span.iEndIndex, this, marker));

			_textLineMarker = marker[0];
		}

		public TextSpan Span
		{
			get
			{
				if (TextLineMarker != null)
				{
					TextSpan[] aSpan = new TextSpan[1];
					ErrorHandler.ThrowOnFailure(TextLineMarker.GetCurrentSpan(aSpan));
					return aSpan[0];
				}

				return Utils.SpanFromLocation(CompilerMessage.Location);
			}
		}

		public override string ToString()
		{
			return CompilerMessage.ToString();
		}

		protected override void OnRemoved(EventArgs e)
		{
			if (TextLineMarker != null)
			{
				ErrorHandler.ThrowOnFailure(TextLineMarker.Invalidate());
				DisposeTextLineMarker();
			}

			base.OnRemoved(e);
		}

		public bool IsMarkerValid { get { return TextLineMarker != null; } }

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
			// You can use this code to implement menu for compiler message.
			//if (commandFlags != null && commandFlags.Length > 0)
			//  commandFlags[0] = 0;
			//if (text != null && text.Length > 0)
			//  pMarker.GetTipText(text);
			//
			//return VSConstants.S_OK;

			return VSConstants.E_NOTIMPL;
		}

		int IVsTextMarkerClient.GetTipText(IVsTextMarker pMarker, string[] tipText)
		{
			if (Text != null && Text.Length > 0)
				tipText[0] = Text;

			return VSConstants.S_OK;
		}

		public void DisposeTextLineMarker()
		{
			if (TextLineMarker != null)
			{
				TextLineMarker.UnadviseClient();
				_textLineMarker = null;
			}
		}

		void IVsTextMarkerClient.MarkerInvalidated() { DisposeTextLineMarker(); }

		int IVsTextMarkerClient.OnAfterMarkerChange(IVsTextMarker pMarker)
		{
			//var owner = Owner;
			//Owner = null;
			var span = Span;
			Line     = span.iStartLine;
			Column   = span.iStartIndex;
			//Owner = owner;
			return VSConstants.S_OK;
		}

		void IVsTextMarkerClient.OnAfterSpanReload() { }
		void IVsTextMarkerClient.OnBeforeBufferClose() { }
		void IVsTextMarkerClient.OnBufferSave(string pszFileName) { }

		#endregion
	} // class: NemerleDocumentTask
} // ns: Nemerle.VisualStudio.Package
