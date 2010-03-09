using System;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;

namespace Test.CodeGeneration.SRE
{
    class MutuallyRecursiveTypes : SREGenerator
    {
        public MutuallyRecursiveTypes() 
            : base(TestNames.MutuallyRecursiveTypes)
        {
        }

        protected override AssemblyBuilder  Generate()
        {
            throw new NotSupportedException("Not supported yet");

            var asmBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(
                new AssemblyName(AssemblyName),
                AssemblyBuilderAccess.RunAndSave
                );

            ResolveEventHandler handler = (_, ea) => asmBuilder;

            AppDomain.CurrentDomain.TypeResolve += handler;

            try
            {
                var moduleBuilder = asmBuilder.DefineDynamicModule(AssemblyName, DllName);
                var xType = moduleBuilder.DefineType("X", TypeAttributes.Class);
                xType.DefineGenericParameters("T");
                xType.CreateType();

                var aType = moduleBuilder.DefineType("A", TypeAttributes.Class);
                var bType = moduleBuilder.DefineType("B", TypeAttributes.Class);
                aType.SetParent(xType.MakeGenericType(bType));
                bType.SetParent(xType.MakeGenericType(aType));

                aType.CreateType();
                bType.CreateType();
                return asmBuilder;
            }
            finally
            {
                AppDomain.CurrentDomain.TypeResolve -= handler;
            }
        }
    }
}
