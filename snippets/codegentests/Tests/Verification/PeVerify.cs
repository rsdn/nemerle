//-----------------------------------------------------------------------------
//
// Copyright (c) Microsoft Corporation.  All Rights Reserved.
// This code is licensed under the Microsoft Public License.
// THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
// IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
// PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
//-----------------------------------------------------------------------------
using System.Diagnostics;
using System.IO;
using System.Collections.Generic;
using System.Text;
using System;

namespace CGTest
{
    public class PeVerifyResult
    {

        public int ExitCode;
        public string AssemblyName;
        public List<string> Errors;

        public string NormalizeErrorString(string error)
        {
            // Lets remove any path information.
            string path = Path.GetDirectoryName(AssemblyName);

            StringBuilder b = new StringBuilder(error);
            if (path.Length > 0)
            {
                b.Replace(path, "<path>");
            }
            b.Replace("\r\n", " ");

            return b.ToString();
        }

    } // class

    public class PeVerify
    {

        const int PeVerifyExpectedExitCode = 0;

        static string _peVerify;
        public static string PeVerifyPath
        {
            get
            {
                if (_peVerify == null)
                {
                    var sdk = new DirectoryInfo(Environment.ExpandEnvironmentVariables(@"%ProgramFiles%\Microsoft SDKs\Windows"));
                    if (sdk.Exists)
                    {
                        foreach (var sdkVersion in sdk.GetDirectories())
                        {
                            var peverify = Path.Combine(Path.Combine(sdkVersion.FullName, "bin"), "peverify.exe");
                            if (File.Exists(peverify))
                            {
                                _peVerify = peverify;
                                break;
                            }
                        }
                    }
                    if (_peVerify == null)
                    {
                        sdk = new DirectoryInfo(sdk.FullName.Replace(" (x86)", ""));
                        if (sdk.Exists)
                        {
                            foreach (var sdkVersion in sdk.GetDirectories())
                            {
                                var peverify = Path.Combine(Path.Combine(sdkVersion.FullName, "bin"), "peverify.exe");
                                if (File.Exists(peverify))
                                {
                                    _peVerify = peverify;
                                    break;
                                }
                            }
                        }
                    }
                    if (_peVerify == null)
                        throw new FileNotFoundException(@"could not find peverify.exe under %programfiles%\Microsoft SDKs\Windows\...");
                }

                return _peVerify;
            }
        }

        public static PeVerifyResult VerifyAssembly(string assemblyName)
        {

            PeVerifyResult result = new PeVerifyResult();
            result.AssemblyName = assemblyName;

            string stdOut, stdErr;
            result.ExitCode = StartAndWaitForResult(PeVerifyPath, assemblyName + " /UNIQUE /IL /NOLOGO", out stdOut, out stdErr);
            ParseErrors(result, stdOut);

            return result;
        }

        static void ParseErrors(PeVerifyResult result, string stdOut)
        {

            result.Errors = new List<string>();

            int startIndex = 0;
            while (startIndex < stdOut.Length)
            {
                startIndex = stdOut.IndexOf("[IL]:", startIndex);
                if (startIndex == -1) break;

                int endIndex = stdOut.IndexOf("[IL]:", startIndex + 1);
                if (endIndex == -1)
                {
                    // Look for the last line...
                    endIndex = stdOut.IndexOf("\r\n", startIndex + 1);
                }

                result.Errors.Add(result.NormalizeErrorString(stdOut.Substring(startIndex, endIndex - startIndex)));
                startIndex = endIndex;
            }
        }

        static int StartAndWaitForResult(string fileName, string arguments, out string stdOut, out string stdErr)
        {
            ProcessStartInfo info = new ProcessStartInfo(fileName, arguments);
            info.UseShellExecute = false;
            info.ErrorDialog = false;
            info.CreateNoWindow = true;
            info.RedirectStandardOutput = true;
            info.RedirectStandardError = true;

            using (Process p = Process.Start(info))
            {
                stdOut = p.StandardOutput.ReadToEnd();
                stdErr = p.StandardError.ReadToEnd();
                return p.ExitCode;
            }
        }

        public static void Assert(PeVerifyResult expectedResult, PeVerifyResult actualResult)
        {
            foreach (var e in expectedResult.Errors)
            {
                actualResult.Errors.Remove(e);
            }

            // TODO: Ideally this will be with peverify v4.0, and this won't be needed.
            if (actualResult.Errors.Count > expectedResult.Errors.Count)
            {
                Console.WriteLine("Expected:");
                expectedResult.Errors.ForEach(Console.WriteLine);
                Console.WriteLine();

                Console.WriteLine("Actual:");
                actualResult.Errors.ForEach(Console.WriteLine);
                Console.WriteLine("PeVerify: {0}", expectedResult.AssemblyName);

                throw new Exception("PeVerify Failed with " + actualResult.Errors.Count + " different errors.");
            }
        }

    } // class

}
