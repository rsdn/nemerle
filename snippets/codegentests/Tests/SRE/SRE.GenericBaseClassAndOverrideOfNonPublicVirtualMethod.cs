using System;
using System.Collections.Generic;
using System.Reflection.Emit;
using System.Text;

namespace Test.CodeGeneration.SRE
{
    class GenericBaseClassAndOverrideOfNonPublicVirtualMethod : SREGenerator
    {
        public GenericBaseClassAndOverrideOfNonPublicVirtualMethod() 
            : base(TestNames.GenericBaseTypeAndNonPublicOverride)
        {
        }

        protected override AssemblyBuilder Generate()
        {
            throw new NotImplementedException();
        }
    }
}
