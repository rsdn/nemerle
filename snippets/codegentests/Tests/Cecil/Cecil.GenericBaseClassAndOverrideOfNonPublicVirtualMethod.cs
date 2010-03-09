using System;
using System.Collections.Generic;
using System.Text;
using Mono.Cecil;
using Mono.Cecil.Cil;

namespace Test.CodeGeneration.Cecil
{
    class GenericBaseClassAndOverrideOfNonPublicVirtualMethod : CecilGenerator
    {
        public GenericBaseClassAndOverrideOfNonPublicVirtualMethod() 
            : base(TestNames.GenericBaseTypeAndNonPublicOverride)
        {
        }

        protected override AssemblyDefinition Generate()
        {
            var assembly = AssemblyFactory.DefineAssembly(AssemblyName, DllName, TargetRuntime.NET_2_0, AssemblyKind.Dll);
            GenerateTypeB(assembly, GenerateTypeA(assembly));
            return assembly;
        }

        private void GenerateTypeB(AssemblyDefinition assembly, TypeDefinition aType)
        {
            var stringType = assembly.MainModule.Import(typeof (string));
            var bType = new TypeDefinition(
                "B",
                "",
                TypeAttributes.Class,
                new GenericInstanceType(aType) {GenericArguments = {stringType}});

            assembly.MainModule.Types.Add(bType);
            var overrideMethod = new MethodDefinition("Get", MethodAttributes.Assem | MethodAttributes.Virtual, stringType);
            bType.Methods.Add(overrideMethod);
            overrideMethod.DeclaringType = bType;
            var cil = overrideMethod.Body.CilWorker;
            cil.Emit(OpCodes.Ldstr, "!");
            cil.Emit(OpCodes.Ret);
        }

        private TypeDefinition GenerateTypeA(AssemblyDefinition assembly)
        {
            var objType = assembly.MainModule.Import(typeof (object));
            
            // define base class
            var aType = new TypeDefinition("A", "", TypeAttributes.Class, objType);
            assembly.MainModule.Types.Add(aType);

            var typeParameter = new GenericParameter("T", aType);
            aType.GenericParameters.Add(typeParameter);

            var baseMethod = new MethodDefinition("Get",
                                                  MethodAttributes.Assem | MethodAttributes.Virtual |
                                                  MethodAttributes.NewSlot, typeParameter);
            aType.Methods.Add(baseMethod);
            baseMethod.DeclaringType = aType;
            
            var local = new VariableDefinition(typeParameter);
            baseMethod.Body.Variables.Add(local);

            var cil = baseMethod.Body.CilWorker;
            cil.Emit(OpCodes.Ldloca, local);
            cil.Emit(OpCodes.Initobj, typeParameter);
            cil.Emit(OpCodes.Ldloc, local);
            cil.Emit(OpCodes.Ret);
            return aType;
        }
    }
}
