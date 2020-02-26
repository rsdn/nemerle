using Microsoft.VisualStudio.Text;
using Nemerle.Compiler;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Nemerle.VisualStudio.LanguageService
{
    sealed class VsSourceSnapshot : SourceSnapshot
    {
        private readonly ITextSnapshot _textSnapshot;
        private File _file;

        internal static SourceSnapshot CreateSourceSnapshot(ITextSnapshot textSnapshot) =>
            new VsSourceSnapshot(textSnapshot);

        public static SourceSnapshot GetSourceSnapshot(ITextBuffer textBuffer) => GetSourceSnapshot(textBuffer.CurrentSnapshot);

        public static SourceSnapshot GetSourceSnapshot(ITextSnapshot textSnapshot)
        {
            var ideSource = textSnapshot.TextBuffer.TryGetNemerleSource();
            return ideSource?.GetSourceSnapshot(textSnapshot);
        }

        private VsSourceSnapshot(ITextSnapshot textSnapshot) : base(0, textSnapshot.GetHashCode())
        {
            _file = FileUtils.GetFile(textSnapshot.TextBuffer.GetFilePath());
            Debug.Assert(_file != null);
            _textSnapshot = textSnapshot;
        }

        public override string OriginalText => _textSnapshot.GetText();
        public override string Text => OriginalText;
        public override File File => _file;
        public override int Version => _textSnapshot.Version.VersionNumber;
        public override bool IsGenerated => false;
        public override bool IsFake => false;

        public override bool Equals(object other)
        {
            if (other is VsSourceSnapshot otherSnapshot)
                return otherSnapshot._hashCode == _hashCode && otherSnapshot._textSnapshot.Equals(_textSnapshot);
            return false;
        }

        public override int GetHashCode() => _textSnapshot.GetHashCode();
    }
}
