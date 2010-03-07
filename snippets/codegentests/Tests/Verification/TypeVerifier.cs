using CGTest;
using Microsoft.Cci;
using System.Linq;

namespace Test.CodeGeneration.Verification
{
    public class TypeVerifier
    {
        private readonly AssemblyVerifier assemblyVerifier;
        private readonly INamedTypeDefinition type;
        private readonly IMetadataReaderHost env;

        public TypeVerifier(AssemblyVerifier assemblyVerifier, INamedTypeDefinition type, IMetadataReaderHost env)
        {
            this.assemblyVerifier = assemblyVerifier;
            this.type = type;
            this.env = env;
        }

        public MemberVerifier<IMethodDefinition> Method(string name)
        {
            var member = GetMember<IMethodDefinition>(name);
            return new MemberVerifier<IMethodDefinition>(this, member);
        }

        public MemberVerifier<IPropertyDefinition> Property(string name)
        {
            var member = GetMember<IPropertyDefinition>(name);
            return new MemberVerifier<IPropertyDefinition>(this, member);
        }

        private T GetMember<T>(string name)
            where T : class, ITypeDefinitionMember
        {
            var member = type.GetMembersNamed(env.NameTable.GetNameFor(name), false).FirstOrDefault() as T;
            if (member == null)
                throw new VerificationException("{1} {0} not found", name, typeof(T).Name);
            return member;
        }


        public AssemblyVerifier Complete()
        {
            return assemblyVerifier;
        }
    }
}