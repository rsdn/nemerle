using System;
using System.Linq;
using CGTest;
using Test.CodeGeneration.Verification;

namespace Test.CodeGeneration
{
    public class TestCase
    {
        private readonly Action<string> verifier;
        private readonly string tempFolder;

        public TestCase(string description, string tempFolder, Action<string> verifier)
        {
            this.Description = description;
            this.tempFolder = tempFolder;
            this.verifier = verifier;
        }

        public string Description { get; private set; }

        public void Run(CodeGenerator codeGenerator)
        {
            try
            {
                var path = codeGenerator.Run(tempFolder);
                var peVerifyResult = PeVerify.VerifyAssembly(path);
                if (peVerifyResult.Errors.Any())
                    throw new VerificationException(string.Join(Environment.NewLine, peVerifyResult.Errors.ToArray()));

                verifier(path);
                Console.WriteLine("Successful");
            }
            catch(Exception e)
            {
                Console.WriteLine(e);
                Console.WriteLine("Failed");
            }
        }
    }
}
