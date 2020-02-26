using Nemerle.Compiler;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Nemerle.VsExtension
{
    class Changes : IChanges
    {
        public Changes(SourceSnapshot before, SourceSnapshot after, Change[] reversedChanges)
        {
            Before          = before          ?? throw new ArgumentNullException(nameof(before));
            After           = after           ?? throw new ArgumentNullException(nameof(after));
            ReversedChanges = reversedChanges ?? throw new ArgumentNullException(nameof(reversedChanges));
        }

        public SourceSnapshot Before { get; }

        public SourceSnapshot After { get; }

        public int BeforeVersion { get; }

        public int AfterVersion { get; }

        public Change[] ReversedChanges { get; }
    }
}
