using System.IO;
using Microsoft.Cci;
using Microsoft.Cci.MutableCodeModel;

namespace Test.CodeGeneration.CCI
{
    public class MutuallyRecursiveTypes : CciGenerator
    {
        public MutuallyRecursiveTypes() : base(TestNames.MutuallyRecursiveTypes)
        {
        }

        protected override Assembly  Generate()
        {
            var nt = Host.NameTable;
            var mscorlib = Host.LoadAssembly(Host.CoreAssemblySymbolicIdentity);
            var assembly = new Assembly
                               {
                                   Name = nt.GetNameFor(AssemblyName),
                                   ModuleName = nt.GetNameFor(DllName),
                                   Kind = ModuleKind.DynamicallyLinkedLibrary,
                                   TargetRuntimeVersion = mscorlib.TargetRuntimeVersion,
                                   MetadataFormatMajorVersion = 2
                               };
            assembly.AssemblyReferences.Add(mscorlib);

            var rootNamespace = new RootUnitNamespace();
            assembly.UnitNamespaceRoot = rootNamespace;
            rootNamespace.Unit = assembly;

            // define module
            var module = new NamespaceTypeDefinition
                             {
                                 ContainingUnitNamespace = rootNamespace,
                                 Name = nt.GetNameFor("<Module>"),
                                 InternFactory = Host.InternFactory,
                                 IsClass = true
                             };
            assembly.AllTypes.Add(module);

            // define X<T>
            var xType = new NamespaceTypeDefinition
                            {
                                ContainingUnitNamespace = rootNamespace,
                                Name = nt.GetNameFor("X"),
                                InternFactory = Host.InternFactory,
                                IsClass = true
                            };
            var typeParameter = new GenericTypeParameter
                                    {
                                        Name = nt.GetNameFor("T"),
                                        InternFactory = Host.InternFactory,
                                        IsClass = true,
                                        DefiningType = xType
                                    };
            xType.GenericParameters.Add(typeParameter);
            assembly.AllTypes.Add(xType);

            var aType = new NamespaceTypeDefinition
                            {
                                ContainingUnitNamespace = rootNamespace,
                                Name = nt.GetNameFor("A"),
                                InternFactory = Host.InternFactory,
                                IsClass = true
                            };
            assembly.AllTypes.Add(aType);

            var bType = new NamespaceTypeDefinition
                            {
                                ContainingUnitNamespace = rootNamespace,
                                Name = nt.GetNameFor("B"),
                                InternFactory = Host.InternFactory,
                                IsClass = true
                            };
            assembly.AllTypes.Add(bType);

            aType.BaseClasses.Add(new GenericTypeInstanceReference { GenericType = xType, GenericArguments = { bType }, InternFactory = Host.InternFactory });
            bType.BaseClasses.Add(new GenericTypeInstanceReference { GenericType = xType, GenericArguments = { aType }, InternFactory = Host.InternFactory });

            return assembly;
        }
    }
}
