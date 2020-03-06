using Nemerle.Compiler;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Nemerle.VsExtension
{
    class MultipleChanges : IMultipleChanges
    {
        public MultipleChanges(SourceSnapshot before, SourceSnapshot after, Change[] reversedChanges)
        {
            Before          = before ?? throw new ArgumentNullException(nameof(before));
            After           = after ?? throw new ArgumentNullException(nameof(after));
            BeforeVersion   = before.Version;
            AfterVersion    = after.Version;
            ReversedChanges = reversedChanges ?? throw new ArgumentNullException(nameof(reversedChanges));
        }

        public SourceSnapshot Before { get; }

        public SourceSnapshot After { get; }

        public int BeforeVersion { get; }

        public int AfterVersion { get; }

        public Change[] ReversedChanges { get; }

        public override string ToString()
        {
            var list = new List<string>();
            foreach (var change in ReversedChanges)
            {
                var before = new Location(Before, change.OldSpan.StartPos, change.OldSpan.EndPos);
                var after = new Location(Before, change.NewSpan.StartPos, change.NewSpan.EndPos);
                list.Add($"{before} => {after}");
            }
            return string.Join("; ", list);
        }
    }
}
