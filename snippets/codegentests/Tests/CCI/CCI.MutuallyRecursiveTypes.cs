using System.IO;
using Microsoft.Cci;
using Microsoft.Cci.MutableCodeModel;

namespace Test.CodeGeneration.CCI
{
    public class MutuallyRecursiveTypes : ICodeGenerator
    {
        public string Run(string tempFolder)
        {
            var host = new PeReader.DefaultHost();
            var nt = host.NameTable;
            var mscorlib = host.LoadAssembly(host.CoreAssemblySymbolicIdentity);
            const string fileName = "cci.mutuallyrecursivetypes.dll";
            var assembly = new Assembly
                               {
                                   Name = nt.GetNameFor("cci.mutuallyrecursivetypes"),
                                   ModuleName = nt.GetNameFor(fileName),
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
                                 InternFactory = host.InternFactory,
                                 IsClass = true
                             };
            assembly.AllTypes.Add(module);

            // define X<T>
            var xType = new NamespaceTypeDefinition
                            {
                                ContainingUnitNamespace = rootNamespace,
                                Name = nt.GetNameFor("X"),
                                InternFactory = host.InternFactory,
                                IsClass = true
                            };
            var typeParameter = new GenericTypeParameter
                                    {
                                        Name = nt.GetNameFor("T"),
                                        InternFactory = host.InternFactory,
                                        IsClass = true,
                                        DefiningType = xType
                                    };
            xType.GenericParameters.Add(typeParameter);
            assembly.AllTypes.Add(xType);

            var aType = new NamespaceTypeDefinition
                            {
                                ContainingUnitNamespace = rootNamespace,
                                Name = nt.GetNameFor("A"),
                                InternFactory = host.InternFactory,
                                IsClass = true
                            };
            assembly.AllTypes.Add(aType);

            var bType = new NamespaceTypeDefinition
                            {
                                ContainingUnitNamespace = rootNamespace,
                                Name = nt.GetNameFor("B"),
                                InternFactory = host.InternFactory,
                                IsClass = true
                            };
            assembly.AllTypes.Add(bType);

            aType.BaseClasses.Add(new GenericTypeInstanceReference { GenericType = xType, GenericArguments = { bType }, InternFactory = host.InternFactory });
            bType.BaseClasses.Add(new GenericTypeInstanceReference { GenericType = xType, GenericArguments = { aType }, InternFactory = host.InternFactory });

            var resultPath = Path.Combine(tempFolder, fileName);
            using (var fs = File.Create(resultPath))
                PeWriter.WritePeToStream(assembly, host, fs);
            return resultPath;
        }

    }
}
