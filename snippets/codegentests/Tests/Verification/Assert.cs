using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Cci;

namespace Test.CodeGeneration.Verification
{
    public static class Assert
    {
        public static void Body(IMethodBody body, params IOperation[] operations)
        {
            if (body.Operations.Count() != operations.Count())
                throw new VerificationException("Incorrect number of instructions");

            var ok = body.Operations.Zip(operations, (x, y) => new {x, y}).All(o => o.x.Equals(o.y));
            if (!ok)
                throw new VerificationException("Incorrect method body content");
        }

        static IEnumerable<T3> Zip<T1, T2, T3>(this IEnumerable<T1> first, IEnumerable<T2> second, Func<T1, T2, T3> f)
        {
            using(var e1 = first.GetEnumerator())
            using (var e2 = second.GetEnumerator())
                if (e1.MoveNext() && e2.MoveNext())
                    yield return f(e1.Current, e2.Current);
        }

        public static AssemblyVerifier Assembly(string assemblyPath)
        {
            var env = new TestHostEnvironment();
            var assembly = (IAssembly)env.LoadUnitFrom(assemblyPath);
            if (assembly == Dummy.Assembly)
                throw new VerificationException("Assembly not found on path '{0}'", assemblyPath);
            return new AssemblyVerifier(assembly, env);
        }

        public static void Equals(IName name, string expectedName)
        {
            if (name.Value != expectedName)
                throw new VerificationException("Name {0} was not equal to expected name {1}", name.Value, expectedName);
        }

        public static void True(bool condition)
        {
            if (!condition)
                throw new VerificationException("Assertion failed.");
        }
    }


    public class AssemblyVerifier
    {
        private readonly IAssembly assembly;
        private readonly IMetadataReaderHost env;

        internal AssemblyVerifier(IAssembly assembly, IMetadataReaderHost env)
        {
            this.assembly = assembly;
            this.env = env;
        }

        public TypeVerifier Type(string typeName)
        {
            return Type(typeName, 0, delegate { });
        }

        public TypeVerifier Type(string typeName, Action<ITypeDefinition> typeVerifier)
        {
            return Type(typeName, 0, typeVerifier);
        }

        public TypeVerifier Type(string typeName, int genericParamsCount, Action<ITypeDefinition> typeVerifier)
        {
            var types = assembly.GetAllTypes().Where(t => t.Name.Value == typeName);
            var targetType = types.FirstOrDefault(t => t.GenericParameterCount == genericParamsCount);
            if (targetType == null)
                throw new ArgumentException(string.Format("Type {0} not found", typeName));
            typeVerifier(targetType);
            return new TypeVerifier(this, targetType, env);
        }
    }
}
