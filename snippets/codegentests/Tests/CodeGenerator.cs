using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;

namespace Test.CodeGeneration
{
    public abstract class CodeGenerator
    {
        protected CodeGenerator(string assemblyName)
        {
            AssemblyName = assemblyName;
        }

        protected string AssemblyName { get; private set; }
        protected string DllName { get { return AssemblyName + ".dll"; } }
        protected string ExeName { get { return AssemblyName + ".exe"; } }

        public abstract string Run(string tempFolder);
    }
}
