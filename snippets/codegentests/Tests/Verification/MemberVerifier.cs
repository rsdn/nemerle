using System;
using Microsoft.Cci;

namespace Test.CodeGeneration.Verification
{
    public class MemberVerifier<T>
        where T : ITypeDefinitionMember
    {
        private readonly TypeVerifier typeVerifier;
        private readonly T member;

        public MemberVerifier(TypeVerifier typeVerifier, T member)
        {
            this.typeVerifier = typeVerifier;
            this.member = member;
        }

        public TypeVerifier Definition(Action<T> validator)
        {
            validator(member);
            return typeVerifier;
        }
    }
}