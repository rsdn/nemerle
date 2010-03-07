using Microsoft.Cci;

namespace Test.CodeGeneration.Verification
{
    public class TestHostEnvironment : MetadataReaderHost
    {
        private readonly PeReader reader;

        public TestHostEnvironment()
        {
            reader = new PeReader(this);
        }

        public override IUnit LoadUnitFrom(string location)
        {
            var result = reader.OpenModule(BinaryDocument.GetBinaryDocumentForFile(location, this));
            RegisterAsLatest(result);
            return result;
        }
    }
}