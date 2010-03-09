using Microsoft.Cci;
using Microsoft.Cci.MutableCodeModel;

namespace Test.CodeGeneration.CCI
{
    class GenericBaseClassAndOverrideOfNonPublicVirtualMethod : CciGenerator
    {
        public GenericBaseClassAndOverrideOfNonPublicVirtualMethod() 
            : base(TestNames.GenericBaseTypeAndNonPublicOverride)
        {
        }

        protected override Assembly Generate()
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
            DefineModule(assembly, rootNamespace);

            var typeA = GenerateTypeA(rootNamespace);
            assembly.AllTypes.Add(typeA);

            var baseType = GenericTypeInstance.GetGenericTypeInstance(typeA, new[] { Host.PlatformType.SystemString }, Host.InternFactory);
            var typeB = GenerateTypeB(rootNamespace, baseType);
            assembly.AllTypes.Add(typeB);

            return assembly;
        }

        private NamespaceTypeDefinition GenerateTypeB(IUnitNamespace rootNamespace, ITypeReference baseType)
        {
            var nt = Host.NameTable;
            var typeB = new NamespaceTypeDefinition
                            {
                                ContainingUnitNamespace = rootNamespace,
                                Name = nt.GetNameFor("B"),
                                InternFactory = Host.InternFactory,
                                IsClass = true,
                                BaseClasses = { baseType }
                            };

            var overrideGetMethod = new MethodDefinition
                                        {
                                            Name = nt.GetNameFor("Get"),
                                            IsCil = true,
                                            IsVirtual = true,
                                            Visibility = TypeMemberVisibility.Assembly,
                                            Type = Host.PlatformType.SystemString,
                                            ContainingTypeDefinition = typeB,
                                            InternFactory = Host.InternFactory
                                        };
            typeB.Methods.Add(overrideGetMethod);
            var il = new ILGenerator(Host);
            var body = new ILGeneratorMethodBody(il, true, 1) {MethodDefinition = overrideGetMethod};
            overrideGetMethod.Body = body;
            
            il.Emit(OperationCode.Ldstr, "1");
            il.Emit(OperationCode.Ret);
            return typeB;
        }

        private INamedTypeDefinition GenerateTypeA(IUnitNamespace rootNamespace)
        {
            var nt = Host.NameTable;
            var typeA = new NamespaceTypeDefinition
                            {
                                ContainingUnitNamespace = rootNamespace,
                                Name = nt.GetNameFor("A"),
                                InternFactory = Host.InternFactory,
                                IsClass = true
                            };
            var typeParameter = new GenericTypeParameter
                                    {
                                        Name = nt.GetNameFor("T"),
                                        InternFactory = Host.InternFactory,
                                        DefiningType = typeA,
                                    };
            typeA.GenericParameters.Add(typeParameter);

            var baseGetMethod = new MethodDefinition
                                    {
                                        Name = nt.GetNameFor("Get"),
                                        IsCil = true,
                                        IsVirtual = true,
                                        Visibility = TypeMemberVisibility.Assembly,
                                        Type = typeParameter,
                                        ContainingTypeDefinition = typeA,
                                        InternFactory = Host.InternFactory,
                                        IsNewSlot = true
                                    };
            typeA.Methods.Add(baseGetMethod);

            var il = new ILGenerator(Host);
            var localVar = new LocalDefinition
            {
                Type = typeParameter,
                Name = nt.GetNameFor("local1"),
            };

            // tricky moment, ILGeneratorMethodBody fills internal collection of local vars only in constructor.
            // lines below should not be swapped 

            il.AddVariableToCurrentScope(localVar);     
            var body = new ILGeneratorMethodBody(il, true, 1) {MethodDefinition = baseGetMethod};
            baseGetMethod.Body = body;


            il.Emit(OperationCode.Ldloca, localVar);
            il.Emit(OperationCode.Initobj, typeParameter);
            il.Emit(OperationCode.Ldloc_0);
            il.Emit(OperationCode.Ret);

            return typeA;
        }
    }
}
