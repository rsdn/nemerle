using System;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;

namespace Test.CodeGeneration.SRE
{
    class MutuallyRecursiveTypes : ICodeGenerator
    {
        public string Run(string tempFolder)
        {
            throw new NotSupportedException("Not supported yet");
            const string name = "sre.mutuallyrecusivetypes";
            const string fileName = name + ".dll";

            var asmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(
                new AssemblyName(name),
                AssemblyBuilderAccess.RunAndSave
                );

            ResolveEventHandler handler = (_, ea) => asmBuilder;

            AppDomain.CurrentDomain.TypeResolve += handler;

            try
            {
                var moduleBuilder = asmBuilder.DefineDynamicModule(name, fileName);
                var xType = moduleBuilder.DefineType("X", TypeAttributes.Class);
                xType.DefineGenericParameters("T");
                xType.CreateType();

                var aType = moduleBuilder.DefineType("A", TypeAttributes.Class);
                var bType = moduleBuilder.DefineType("B", TypeAttributes.Class);
                aType.SetParent(xType.MakeGenericType(bType));
                bType.SetParent(xType.MakeGenericType(aType));

                aType.CreateType();
                bType.CreateType();

                var resultPath = Path.Combine(tempFolder, fileName);
                asmBuilder.Save(fileName);
                return resultPath;
            }
            finally
            {
                AppDomain.CurrentDomain.TypeResolve -= handler;
            }
        }
    }
}
