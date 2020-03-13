using Nemerle.Compiler;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Nemerle.VsExtension
{
    class SingleChanges : ISingleChanges
    {
        public SingleChanges(SourceSnapshot before, SourceSnapshot after, Change change)
        {
            Before        = before ?? throw new ArgumentNullException(nameof(before));
            After         = after  ?? throw new ArgumentNullException(nameof(after));
            BeforeVersion = before.Version;
            AfterVersion  = after.Version;
            Change        = change;
        }

        public SourceSnapshot Before { get; }

        public SourceSnapshot After { get; }

        public int BeforeVersion { get; }

        public int AfterVersion { get; }

        public Change Change { get; }

        public override string ToString()
        {
            var before = new Location(Before, Change.OldSpan.StartPos, Change.OldSpan.EndPos);
            var after  = new Location(After,  Change.NewSpan.StartPos, Change.NewSpan.EndPos);
            return $"{before} => {after}";
        }
    }
}
