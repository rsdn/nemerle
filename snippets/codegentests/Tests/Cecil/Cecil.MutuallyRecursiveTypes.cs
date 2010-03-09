using System.IO;
using Mono.Cecil;

namespace Test.CodeGeneration.Cecil
{
    class MutuallyRecursiveTypes : CecilGenerator
    {
        public MutuallyRecursiveTypes() 
            : base(TestNames.MutuallyRecursiveTypes)
        {
        }

        protected override AssemblyDefinition  Generate()
        {
            var assembly = AssemblyFactory.DefineAssembly(DllName, TargetRuntime.NET_2_0, AssemblyKind.Dll);

            var objectTypeRef = assembly.MainModule.Import(typeof (object));
            var module = assembly.MainModule;

            var xType = new TypeDefinition("X", "", TypeAttributes.Class, objectTypeRef);
            xType.GenericParameters.Add(new GenericParameter("T", xType));
            module.Types.Add(xType);
            xType.Module = module;

            var aType = new TypeDefinition("A", "", TypeAttributes.Class, null);
            var bType = new TypeDefinition("B", "", TypeAttributes.Class, null);

            var aBaseType = new GenericInstanceType(xType);
            aBaseType.GenericArguments.Add(bType);
            aType.BaseType = aBaseType;

            var bBaseType = new GenericInstanceType(xType);
            bBaseType.GenericArguments.Add(aType);
            bType.BaseType = bBaseType;
            
            module.Types.Add(aType);
            aType.Module = module;

            module.Types.Add(bType);
            bType.Module = module;

            return assembly;
        }
    }
}
