using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Compiler;
using Nemerle.Compiler.Parsetree;
using Nemerle.Compiler.Utils;
using Nemerle.Compiler.Utils.Async;
using Nemerle.Completion2;
using Nemerle.Completion2.CodeFormatting;
using Nemerle.VisualStudio.GUI;
using Nemerle.VisualStudio.LanguageService.Highlighting.TypeClassifier;
using Nemerle.VisualStudio.Package;
using Nemerle.VisualStudio.Project;

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows;
using System.Windows.Forms;

using TopDeclaration = Nemerle.Compiler.Parsetree.TopDeclaration;
using TupleIntInt = Nemerle.Builtins.Tuple<int, int>;
using TupleStringInt = Nemerle.Builtins.Tuple<string, int>;
using TupleStringIntInt = Nemerle.Builtins.Tuple<string, int, int>;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;
using NC = Nemerle.Compiler;
using Nemerle.VsExtension;

namespace Nemerle.VisualStudio.LanguageService
{
    static class Factories
    {
        public static void Init() { }

        static Factories()
        {
            Nemerle.Compiler.Completion.RelocateImpl = RelocateImpl2;
            RelocationFactory.CreateRelocationInfo = CreateRelocationInfo;
        }

        class RelocationInfo2 : RelocationInfo
        {
            public RelocationInfo2(IChanges changes) : base(changes) { }

            public override bool IsVisited(object obj) => base.IsVisited(obj);

            public override void SetVisited(object obj)
            {
                base.SetVisited(obj);
            }
        }

        private static RelocationInfo CreateRelocationInfo(IChanges changes)
        {
            return new RelocationInfo2(changes);
        }

        private static Location RelocateImpl2(Location loc, RelocationInfo info)
        {
            if (loc.FileIndex != info.FileIndex)
                return loc;

            var st = new System.Diagnostics.StackTrace();
            //var frames = st.GetFrames();
            var result = RelocateImpl(loc, info);
            //if (loc.Line == loc.EndLine)
            //    Debug.WriteLine($"{loc.ToVsOutputStringFormat()} RelocateImpl2 {loc} -> {result} ThreadId=" + AsyncWorker.ThreadId);
            return result;
        }

        private static Location RelocateImpl(Location loc, RelocationInfo info)
        {
            if (loc.FileIndex != info.FileIndex)
                return loc;

            var version = loc.Source.Version;
            var changes = info.Changes;
            var afterVersion = changes.AfterVersion;
            var beforeVersion = changes.BeforeVersion;
            var afterSource = changes.After;

            if (afterSource.IsGenerated)

                if (version == afterVersion)
                    return loc;

            if (version != beforeVersion)
            {
                if (version == afterVersion)
                    return loc;
                if (loc.IsGenerated)
                    return Location.Default;
                Debug.WriteLine($"{loc.ToVsOutputStringFormat()} Location not relocated due 'version != beforeVersion' beforeVersion={beforeVersion} loc='{loc}'");
                throw new RelocationException();
            }

            if (loc.Source.IsGenerated)
                afterSource = FileUtils.MakeGeneratedSource(afterSource);

            if (changes is ISingleChanges single)
                RelocateImpl(ref loc, changes, single.Change, afterSource);
            else
            {
                foreach (Change change in ((IMultipleChanges)changes).ReversedChanges)
                    RelocateImpl(ref loc, changes, change, afterSource);
            }

            //Debug.WriteLine(loc.ToVsOutputStringFormat() + " relocated");
            return loc;
        }

        private static void RelocateImpl(ref Location loc, IChanges changes, in Change change, in SourceSnapshot afterSource)
        {
            var oldSpan = change.OldSpan;
            var startPos = loc.StartPos;
            var endPos = loc.EndPos;
            var oldStartPos = oldSpan.StartPos;
            var oldEndPos = oldSpan.EndPos;
            var newLen = change.NewSpan.Length;
            var oldLen = oldSpan.Length;

            // ----- startPos
            // ----- endPos
            // ***** oldStartPos
            // ***** oldStartPos
            if (oldStartPos >= endPos)
            {
                loc = new Location(afterSource, startPos, endPos);
                return;
            }

            var delta = newLen - oldLen;
            // ----- startPos
            // ***** oldStartPos
            // ***** oldStartPos |
            // ----- endPos      V
            if (startPos < oldStartPos && endPos > oldEndPos)
            {
                loc = new Location(afterSource, startPos, endPos + delta);
                return;
            }

            // ***** oldStartPos |
            // ***** oldStartPos V
            // ----- startPos
            // ----- endPos
            if (startPos >= oldEndPos)
            {
                loc = new Location(afterSource, startPos + delta, endPos + delta);
                return;
            }

            loc = new Location(afterSource, 0, 0); // broken location
        }

    }
}
