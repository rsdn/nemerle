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
	[ContentType("text")]//code
	[TextViewRole(PredefinedTextViewRoles.Document)]
	class NemerleTextViewCreationListener : IVsTextViewCreationListener
	{
    [Import]
    private IEditorOperationsFactoryService editorOperationsFactoryService { get; set; }
		[Import]
		private IVsEditorAdaptersFactoryService editorAdaptersFactoryService { get; set; }
 	
		#region IVsTextViewCreationListener Members

		public void VsTextViewCreated(IVsTextView textViewAdapter)
		{
			string filePath = textViewAdapter.GetFilePath();
			var    view = textViewAdapter.ToITextView();

			Trace.WriteLine("Opened: " + filePath);
			
			view.TextBuffer.Changed += TextBuffer_Changed;
			view.Closed             += view_Closed;
		}

		void TextBuffer_Changed(object sender, TextContentChangedEventArgs args)
		{
			var textBuffer = (ITextBuffer)sender;
			var vsTextBuffer = textBuffer.ToIVsTextBuffer();
			IPersistFileFormat persistFileFormat = (IPersistFileFormat)vsTextBuffer;
			string filePath = textBuffer.GetFilePath();// textBuffer.ToIVsTextBuffer().GetFilePath();
			var ext = Path.GetExtension(filePath);

			// TODO: FIXME: VladD2: We must check extension by compiler plagin engine!
			if (!Utils.Eq(ext, ".cs"))
				return;

			var project = ProjectInfo.FindProject(filePath);

			if (project == null)
				return;

			project.Engine.RequestOnBuildTypesTree();

			System.Diagnostics.Trace.WriteLine("Changed: (" + args.AfterVersion + ") " + filePath);
		}

		void view_Closed(object sender, EventArgs args)
		{
			var view = (ITextView)sender;
			IVsTextView vsTextView = view.ToVsTextView();
			view.TextBuffer.Changed -= TextBuffer_Changed;
			view.Closed -= view_Closed;

			Trace.WriteLine("Closed: " + vsTextView.GetFilePath());
		}

		#endregion
	}
}
