using System.IO;
using Mono.Cecil;

namespace Test.CodeGeneration.Cecil
{
    abstract class CecilGenerator : CodeGenerator
    {
        protected CecilGenerator(string assemblyName) 
            : base("cecil." + assemblyName)
        {
        }

        public override string Run(string tempFolder)
        {
            var assembly = Generate();
            var resultPath = Path.Combine(tempFolder, assembly.Kind == AssemblyKind.Dll ? DllName : ExeName);
            AssemblyFactory.SaveAssembly(assembly, resultPath);
            return resultPath;
        }

        protected abstract AssemblyDefinition Generate();
    }
}
