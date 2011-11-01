using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
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
			IVsTextLines vsTextLines;
			textViewAdapter.GetBuffer(out vsTextLines);
			IPersistFileFormat persistFileFormat = (IPersistFileFormat)vsTextLines;
			string filePath;
			uint formatIndex;
			persistFileFormat.GetCurFile(out filePath, out formatIndex);

			var view = textViewAdapter.ToITextView();
			var doc = view.TextBuffer.Properties.GetProperty<ITextDocument>(typeof(ITextDocument));
			filePath = doc.FilePath;

			Trace.WriteLine("Opened: " + filePath);
			
			view.TextBuffer.Changed += TextBuffer_Changed;
			EventHandler closed = null;
			closed = (sender, args) =>
				{
					Trace.WriteLine("Closed: " + filePath);
					view.TextBuffer.Changed -= TextBuffer_Changed;
					view.Closed -= closed;
				};
			view.Closed += closed; 
		}

		

		void TextBuffer_Changed(object sender, TextContentChangedEventArgs args)
		{
			var textBuffer = sender as ITextBuffer;
			var doc = textBuffer.Properties.GetProperty<ITextDocument>(typeof(ITextDocument));
			//var vsTextBuffer = textBuffer.ToIVsTextBuffer();
			//var vsTextLines = vsTextBuffer as IVsTextLines;
			//var filePath = FilePathUtilities.GetFilePath(vsTextLines);
			var filePath = doc.FilePath;
			System.Diagnostics.Trace.WriteLine("Changed: (" + args.AfterVersion + ") " + filePath);


			//foreach (var p in textBuffer.Properties.PropertyList)
			//{
			//  System.Diagnostics.Trace.WriteLine(p.Key + " = " + p.Value);
			//}


			//System.Diagnostics.Trace.WriteLine(args.AfterVersion);
			//System.Diagnostics.Trace.WriteLine(args.After.GetText());
		}

		void TextBuffer_Changing(object sender, EventArgs args)
		{
		}

		void view_Closed(object sender, EventArgs args)
		{
			var textView = sender as ITextView;
			var wpfTextView = sender as IWpfTextView;
			
			
			//var doc = textView.TextBuffer.Properties.GetProperty<ITextDocument>(typeof(ITextDocument));
			var docData = textView.TextBuffer.Properties.GetProperty<IVsPersistDocData>(typeof(IVsPersistDocData));
			
			//var doc = docData as ITextDocument;
			//var filePath = docData.;
			//System.Diagnostics.Trace.WriteLine("Closed: " + filePath);
		}

		#endregion
	}
}
