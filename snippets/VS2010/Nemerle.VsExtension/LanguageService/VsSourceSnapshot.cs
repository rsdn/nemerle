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
        public readonly int BaseVersion;

        private readonly ITextSnapshot _textSnapshot;
        private File _file;
        private string _text;

        internal static SourceSnapshot CreateSourceSnapshot(VsNemerleSource nemerleSource, ITextSnapshot textSnapshot) =>
            new VsSourceSnapshot(nemerleSource, textSnapshot);

        public static SourceSnapshot GetSourceSnapshot(ITextBuffer textBuffer) => GetSourceSnapshot(textBuffer.CurrentSnapshot);

        public static SourceSnapshot GetSourceSnapshot(ITextSnapshot textSnapshot)
        {
            var ideSource = textSnapshot.TextBuffer.TryGetNemerleSource();
            return ideSource?.GetSourceSnapshot(textSnapshot);
        }

        private VsSourceSnapshot(VsNemerleSource nemerleSource, ITextSnapshot textSnapshot) : base(0, 0) // hash-code calculated dynamically
        {
            BaseVersion = nemerleSource.BaseVersion;
            var indexArrayLength = FileUtils.GetIndexArrayLength();
            _file = FileUtils.GetFile(textSnapshot.TextBuffer.GetFilePath());
            Debug.Assert(_file != null);
            if (_file.Id >= indexArrayLength)
                nemerleSource.ProjectInfo?.Engine?.BeginBuildTypesTree();
            _textSnapshot = textSnapshot;

            Debug.WriteLine($"VsSourceSnapshot created Version={Version} {nemerleSource.GetFilePath()}");
        }

        public override string OriginalText => _text ?? (_text =_textSnapshot.GetText());
        public override string Text => OriginalText;
        public override File File => _file;
        public override int Version => _textSnapshot.Version.VersionNumber + BaseVersion;
        public override bool IsGenerated => false;
        public override bool IsFake => false;

        public override bool Equals(object other)
        {
            //if (other is VsSourceSnapshot otherSnapshot)
            //    return otherSnapshot._hashCode == _hashCode && otherSnapshot._textSnapshot.Equals(_textSnapshot);
            //return false;
            return base.Equals(other);
        }

        public override int GetHashCode()
        {
            if (_hashCode == 0)
                _hashCode = OriginalText.GetHashCode();

            return _hashCode;
        }
    }
}
