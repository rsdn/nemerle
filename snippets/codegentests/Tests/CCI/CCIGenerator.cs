using System;
using System.Collections.Generic;
using System.IO;
using Microsoft.Cci;
using Microsoft.Cci.MutableCodeModel;

namespace Test.CodeGeneration.CCI
{
    public abstract class CciGenerator : CodeGenerator
    {
        protected CciGenerator(string testName)
            :base("cci." + testName)
        {
            Host = new PeReader.DefaultHost();
        }

        protected IMetadataReaderHost Host { get; private set; }

        public override string Run(string tempFolder)
        {
            var assembly = Generate();
            var fileName = assembly.Kind == ModuleKind.DynamicallyLinkedLibrary ? DllName : ExeName;
            var resultPath = Path.Combine(tempFolder, fileName);
            using (var fs = File.Create(resultPath))
                PeWriter.WritePeToStream(assembly, Host, fs);
            return resultPath;
        }

        protected abstract Assembly Generate();

        protected void DefineModule(Assembly assembly, RootUnitNamespace rootNamespace)
        {
            var nt = Host.NameTable;
            var module = new NamespaceTypeDefinition
                             {
                                 ContainingUnitNamespace = rootNamespace,
                                 Name = nt.GetNameFor("<Module>"),
                                 InternFactory = Host.InternFactory,
                                 IsClass = true
                             };
            assembly.AllTypes.Add(module);
        }
    }
}
