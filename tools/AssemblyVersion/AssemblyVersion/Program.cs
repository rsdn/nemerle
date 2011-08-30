using System;
using System.IO;
using System.Reflection;

namespace AssemblyVersion
{
    class Program
    {
        static void Main(string[] args)
        {
            var assemblyName = AssemblyName.GetAssemblyName(args[0]);
            string line;
            using (StreamReader inFile = new System.IO.StreamReader(args[1]))
                using (StreamWriter outFile = new System.IO.StreamWriter(args[2]))
                    while ((line = inFile.ReadLine()) != null)
                        outFile.WriteLine(line.Replace("$AssemblyVersion$", assemblyName.Version.ToString()));
        }
    }
}
