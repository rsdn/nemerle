using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Text.Operations;
using System.Collections.ObjectModel;
using Microsoft.VisualStudio.Language.Intellisense;

using Nemerle.VisualStudio;
using Nemerle.VisualStudio.LanguageService;
using Nemerle.Compiler.Parsetree;
using System.Diagnostics;
using Nemerle.Compiler;

namespace SmartTagTest
{
	internal class NemerleImplementsSmartTagger : ITagger<NemerleImplementsSmartTag>, IDisposable
	{
		private ITextBuffer             _buffer;
		private ITextView               _view;
		private NemerleImplementsSmartTaggerProvider _provider;
		private bool                    _disposed;
		private bool                    _cursorOnTypeName;
		private Span                    _typeNameSpan;
		private Location                _typeNameLocation;

		public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

		public NemerleImplementsSmartTagger(ITextBuffer buffer, ITextView view, NemerleImplementsSmartTaggerProvider provider)
		{
			_buffer = buffer;
			_view = view;
			_provider = provider;
			//_view.LayoutChanged += OnLayoutChanged;
			_view.Caret.PositionChanged += OnCaretPositionChanged;
		}

		public IEnumerable<ITagSpan<NemerleImplementsSmartTag>> GetTags(NormalizedSnapshotSpanCollection spans)
		{
			if (!_cursorOnTypeName)
				yield break;

			var snapshot = _buffer.CurrentSnapshot;

			if (snapshot.Length == 0)
				yield break; //don't do anything if the buffer is empty

			var span = new SnapshotSpan(snapshot, _typeNameSpan);

			yield return new TagSpan<NemerleImplementsSmartTag>(span, new NemerleImplementsSmartTag(GetSmartTagActions(span)));
		}

		private ReadOnlyCollection<SmartTagActionSet> GetSmartTagActions(SnapshotSpan span)
		{
			List<SmartTagActionSet> actionSetList = new List<SmartTagActionSet>();
			List<ISmartTagAction>   actionList    = new List<ISmartTagAction>();

			ITrackingSpan trackingSpan = span.Snapshot.CreateTrackingSpan(span, SpanTrackingMode.EdgeInclusive);
			var snapShot = trackingSpan.TextBuffer.CurrentSnapshot;

			NemerleSource source = GetNemerleSourceOrNull();
			Trace.Assert(source != null);

			actionList.Add(new SmartTagLambdaAction("Implement interfaces",
				() => source.ImplementInterfaces(_typeNameLocation.Line, _typeNameLocation.Column)));

			actionList.Add(new SmartTagLambdaAction("Override members",
				() => source.OverrideMembers(_typeNameLocation.Line, _typeNameLocation.Column)));

			SmartTagActionSet actionSet = new SmartTagActionSet(actionList.AsReadOnly());

			actionSetList.Add(actionSet);

			return actionSetList.AsReadOnly();
		}

		public void Dispose()
		{
			Dispose(true);
			GC.SuppressFinalize(this);
		}

		private void Dispose(bool disposing)
		{
			if (!this._disposed)
			{
				if (disposing)
				{
					//_view.LayoutChanged         -= OnLayoutChanged;
					_view.Caret.PositionChanged -= OnCaretPositionChanged;
					_view                        = null;
				}

				_disposed = true;
			}
		}

		/*
		private void OnLayoutChanged(object sender, TextViewLayoutChangedEventArgs e)
		{
			ITextSnapshot snapshot = e.NewSnapshot;
			//don't do anything if this is just a change in case
			if (!snapshot.GetText().ToLower().Equals(e.OldSnapshot.GetText().ToLower()))
			{
				SnapshotSpan span = new SnapshotSpan(snapshot, new Span(0, snapshot.Length));
				EventHandler<SnapshotSpanEventArgs> handler = this.TagsChanged;
				if (handler != null)
					handler(this, new SnapshotSpanEventArgs(span));
			}
		}
		*/


		NemerleSource GetNemerleSourceOrNull()
		{
			NemerleSource source;
			_buffer.Properties.TryGetProperty(typeof(NemerleSource), out source);
			return source;
		}

		public void OnCaretPositionChanged(object sender, CaretPositionChangedEventArgs e)
		{
			NemerleSource source = GetNemerleSourceOrNull();
			if (source == null)
				return;

			var compileUnit = source.CompileUnit;

			if (compileUnit != null)
			{
				var point = e.NewPosition.BufferPosition;

				var textLine = point.GetContainingLine();
				var line = textLine.LineNumber + 1;
				var lineStartPos = textLine.Start;
				var col = 1 + point.Position - lineStartPos;

				var member = compileUnit.FindMember(line, col);

				if (member.IsSome && member.Value.NameLocation.Contains(line, col)
					&& (member.Value is TopDeclaration.Class || member.Value is TopDeclaration.Variant))
				{
					_cursorOnTypeName = true;
					_typeNameLocation = member.Value.NameLocation;
					var loc = member.Value.NameLocation;
					var startPos = lineStartPos + loc.Column - 1;
					var endPos = lineStartPos + loc.EndColumn - 1;
					_typeNameSpan = new Span(startPos, endPos - startPos);

					var snapshot = _buffer.CurrentSnapshot;
					//SnapshotSpan span2 = new SnapshotSpan(snapshot, _typeNameSpan);
					//var ttt = span2.GetText();

					SnapshotSpan span = new SnapshotSpan(snapshot, new Span(0, snapshot.Length));
					EventHandler<SnapshotSpanEventArgs> handler = this.TagsChanged;
					if (handler != null)
						handler(this, new SnapshotSpanEventArgs(span));
					return;
				}
			}

			if (_cursorOnTypeName)
			{
				_cursorOnTypeName = false;
				var snapshot = _buffer.CurrentSnapshot;
				SnapshotSpan span = new SnapshotSpan(snapshot, new Span(0, snapshot.Length));
				EventHandler<SnapshotSpanEventArgs> handler = this.TagsChanged;
				if (handler != null)
					handler(this, new SnapshotSpanEventArgs(span));
			}
		}
	}
}
