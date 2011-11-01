using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Shell.Interop;
using System.Diagnostics;
using Nemerle.VisualStudio.Project;

namespace Nemerle.VisualStudio.LanguageService.TextEditor
{
	[Export(typeof(IVsTextViewCreationListener))]
	[ContentType("code")]
	[TextViewRole(PredefinedTextViewRoles.Document)]
	class NemerleTextViewCreationListener : IVsTextViewCreationListener
	{
    [Import]
    private IEditorOperationsFactoryService editorOperationsFactoryService { get; set; }
		[Import]
		private IVsEditorAdaptersFactoryService editorAdaptersFactoryService { get; set; }

		ProjectInfo GetProjectInfo(string filePath)
		{
			var ext = Path.GetExtension(filePath);

			// TODO: FIXME: VladD2: We must check extension by compiler plagin engine!
			if (!Utils.Eq(ext, ".cs"))
				return null;

			return ProjectInfo.FindProject(filePath);
		}

		#region IVsTextViewCreationListener Members

		public void VsTextViewCreated(IVsTextView textViewAdapter)
		{
			var view = textViewAdapter.ToITextView();
			var filePath = textViewAdapter.GetFilePath();

			var project = GetProjectInfo(filePath);
			if (project == null)
				return;

			//Trace.WriteLine("Opened: " + filePath);

			IVsTextLines vsTextLines = textViewAdapter.GetBuffer();
			var langSrv = NemerlePackage.GetGlobalService(typeof(NemerleLanguageService)) as NemerleLanguageService;
			if (langSrv == null)
				return;
			var source = (NemerleSource)langSrv.GetOrCreateSource(vsTextLines);
			project.AddEditableSource(source);

			view.TextBuffer.Changed += TextBuffer_Changed;
			view.Closed             += view_Closed;
		}

		void TextBuffer_Changed(object sender, TextContentChangedEventArgs args)
		{
			var textBuffer = (ITextBuffer)sender;
			string filePath = textBuffer.GetFilePath();// textBuffer.ToIVsTextBuffer().GetFilePath();

			var project = GetProjectInfo(filePath);
			if (project == null)
				return;

			project.Engine.RequestOnBuildTypesTree();

			//System.Diagnostics.Trace.WriteLine("Changed: (" + args.AfterVersion + ") " + filePath);
		}

		void view_Closed(object sender, EventArgs args)
		{
			var view = (ITextView)sender;
		
			view.TextBuffer.Changed -= TextBuffer_Changed;
			view.Closed -= view_Closed;

			IVsTextView vsTextView = view.ToVsTextView();

			var filePath = vsTextView.GetFilePath();// vsTextView.ToIVsTextBuffer().GetFilePath();
			//Trace.WriteLine("Closed: " + filePath);

			var langSrv = NemerlePackage.GetGlobalService(typeof(NemerleLanguageService)) as NemerleLanguageService;
			if (langSrv == null)
				return;
			var source = (NemerleSource)langSrv.GetSource(filePath);
			langSrv.OnCloseSource(source);

			source.Dispose();
		}

		#endregion
	}
}
