using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection.Emit;
using System.Text;

namespace Test.CodeGeneration.SRE
{
    public abstract class SREGenerator : CodeGenerator
    {
        protected SREGenerator(string assemblyName) 
            : base("sre." + assemblyName)
        {
        }

        public override string Run(string tempFolder)
        {
            var assembly = Generate();
            var resultPath = Path.Combine(tempFolder, assembly.EntryPoint != null ? ExeName : DllName);
            assembly.Save(resultPath);
            return resultPath;
        }

        protected abstract AssemblyBuilder Generate();
    }
}
