using System;

namespace Test.CodeGeneration.Verification
{
    public class VerificationException : Exception
    {
        public VerificationException(string format, params object [] args) 
            : base(string.Format(format, args))
        {
        }
    }
}