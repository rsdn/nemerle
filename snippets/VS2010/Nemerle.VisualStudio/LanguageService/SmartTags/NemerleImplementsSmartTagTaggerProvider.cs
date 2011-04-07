using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text;

namespace SmartTagTest
{
	[Export(typeof(IViewTaggerProvider))]
	[ContentType("Nemerle")]
	[Order(Before = "default")]
	[TagType(typeof(SmartTag))]
	internal class NemerleImplementsSmartTaggerProvider : IViewTaggerProvider
	{
		[Import(typeof(ITextStructureNavigatorSelectorService))]
		internal ITextStructureNavigatorSelectorService NavigatorService { get; set; }

		[Import]
		internal ISmartTagBroker SmartTagBroker { get; set; }

		public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
		{
			if (buffer == null || textView == null)
				return null;

			var propKey = typeof(ISmartTagBroker);

			if (!textView.Properties.ContainsProperty(propKey) && SmartTagBroker != null)
				textView.Properties.AddProperty(propKey, SmartTagBroker);

			//make sure we are tagging only the top buffer
			if (buffer == textView.TextBuffer)
				return new NemerleImplementsSmartTagger(buffer, textView, this) as ITagger<T>;

			else return null;
		}
	}
}
