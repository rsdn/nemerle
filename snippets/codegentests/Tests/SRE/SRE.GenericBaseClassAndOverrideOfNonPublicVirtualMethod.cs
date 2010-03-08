using System;
using System.Collections.Generic;
using System.Text;

namespace Test.CodeGeneration.SRE
{
    class GenericBaseClassAndOverrideOfNonPublicVirtualMethod : ICodeGenerator
    {
        public string Run(string tempFolder)
        {
            throw new NotImplementedException();
        }
    }
}
