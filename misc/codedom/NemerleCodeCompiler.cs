//
// NemerleCodeCompiler.cs
//
// Author:
//	Atsushi Enomoto <atsushi@ximian.com>
//
// Original: CSharpCodeCompiler.cs
// Original Authors:
//	Sean Kasun (seank@users.sf.net)
//	Gonzalo Paniagua Javier (gonzalo@ximian.com)
//
// Copyright (c) Novell, Inc. (http://www.novell.com)
//

//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Configuration;
using System.IO;
using System.Text;
using System.Reflection;
using System.Collections;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Text.RegularExpressions;

namespace Nemerle.Contrib
{

	internal class NemerleCodeCompiler : NemerleCodeGenerator, ICodeCompiler
	{
		public NemerleCodeCompiler ()
		{
		}

		public CompilerResults CompileAssemblyFromDom (
			CompilerParameters options, CodeCompileUnit e)
		{
			return CompileAssemblyFromDomBatch (options,
				new CodeCompileUnit [] {e});
		}
		public CompilerResults CompileAssemblyFromDomBatch (
			CompilerParameters options, CodeCompileUnit [] ea)
		{
			string[] fileNames = new string [ea.Length];
			int i = 0;
			if (options == null)
				options = new CompilerParameters ();
			
			StringCollection assemblies = options.ReferencedAssemblies;

			foreach (CodeCompileUnit e in ea) {
				fileNames [i] = GetTempFileNameWithExtension (options.TempFiles, i.ToString () + ".n");
				FileStream f = new FileStream (fileNames [i],FileMode.OpenOrCreate);
				StreamWriter s = new StreamWriter (f, Encoding.UTF8);
				if (e.ReferencedAssemblies != null) {
					foreach (string str in e.ReferencedAssemblies) {
						if (!assemblies.Contains (str))
							assemblies.Add (str);
					}
				}

				((ICodeGenerator) this).GenerateCodeFromCompileUnit (e, s, new CodeGeneratorOptions());
				s.Close();
				f.Close();
				i++;
			}
			return CompileAssemblyFromFileBatch (options, fileNames);
		}
		
		public CompilerResults CompileAssemblyFromFile (
			CompilerParameters options, string fileName)
		{
			return CompileAssemblyFromFileBatch (options, new string [] {fileName});
		}

		public CompilerResults CompileAssemblyFromFileBatch (CompilerParameters options, string [] fileNames)
		{
            Console.WriteLine("entering batch");
            if (null == options)
				throw new ArgumentNullException("options");
			if (null == fileNames)
				throw new ArgumentNullException("fileNames");

			CompilerResults results = new CompilerResults (options.TempFiles);
			Process compiler = new Process ();

			string compiler_output;
			string [] compiler_output_lines;
			string envncc = Environment.GetEnvironmentVariable ("NCC_PROVIDER_COMMAND");
			if (envncc == null)
				envncc = ConfigurationSettings.AppSettings ["ncc-provider-command"];
			if (envncc != null)
				compiler.StartInfo.FileName = envncc;
			else
				compiler.StartInfo.FileName = "ncc";
			compiler.StartInfo.Arguments = BuildArgs (options,fileNames);
			compiler.StartInfo.CreateNoWindow = true;
			compiler.StartInfo.UseShellExecute = false;
            compiler.StartInfo.RedirectStandardOutput=true;
//            Console.WriteLine(envncc);
//            Console.WriteLine(BuildArgs(options, fileNames));
            try {
                compiler.Start ();
                Console.WriteLine("compiler started");
                compiler_output = compiler.StandardOutput.ReadToEnd ();
				compiler.WaitForExit ();
                Console.WriteLine("compiler ended");
            } finally {
				results.NativeCompilerReturnValue = compiler.ExitCode;
				compiler.Close ();
            }
            compiler_output_lines = compiler_output.Split (
				System.Environment.NewLine.ToCharArray ());
			bool loadIt = true;
			foreach (string error_line in compiler_output_lines)
			{
				CompilerError error = CreateErrorFromString(error_line);
				if (null != error)
				{
					results.Errors.Add (error);
					if (!error.IsWarning)
						loadIt = false;
				}
			}
			if (loadIt)
				results.CompiledAssembly = Assembly.LoadFrom (options.OutputAssembly);
			else
				results.CompiledAssembly = null;

			return results;
		}

		public CompilerResults CompileAssemblyFromSource (
			CompilerParameters options, string source)
		{
			return CompileAssemblyFromSourceBatch (options,
				new string [] {source});
		}

		public CompilerResults CompileAssemblyFromSourceBatch (
			CompilerParameters options, string [] sources)
		{
			string [] fileNames = new string [sources.Length];
			int i = 0;
			foreach (string source in sources) {
				fileNames [i] = GetTempFileNameWithExtension (
					options.TempFiles, i.ToString () + ".n");
				FileStream f = new FileStream (fileNames [i],
					FileMode.OpenOrCreate);
				StreamWriter s = new StreamWriter (f);
				s.Write (source);
				s.Close ();
				f.Close ();
				i++;
			}
			return CompileAssemblyFromFileBatch (options, fileNames);
		}

		private static string BuildArgs (CompilerParameters options,
			string [] fileNames)
		{
			StringBuilder args = new StringBuilder();
			if (options.GenerateExecutable)
				args.AppendFormat ("-target-exe ");
			else
				args.AppendFormat ("-target-library ");
//			if (options.IncludeDebugInformation)
//				args.AppendFormat ("/debug ");
//			if (options.TreatWarningsAsErrors)
//				args.AppendFormat ("/warnaserror ");

//			if (options.WarningLevel != -1)
//				args.AppendFormat ("/warn:{0} ", options.WarningLevel);

			if (options.OutputAssembly == null)
				options.OutputAssembly = GetTempFileNameWithExtension (options.TempFiles, "dll");
			args.AppendFormat ("-out:\"{0}\" ",options.OutputAssembly);
			if (null != options.ReferencedAssemblies) {
				foreach (string import in options.ReferencedAssemblies)
					args.AppendFormat ("-r:\"{0}\" ",import);
			}

//			args.Append (" -- ");
			foreach (string source in fileNames)
				args.AppendFormat ("\"{0}\" ",source);
			return args.ToString ();
		}

		private static CompilerError CreateErrorFromString (
			string error_string)
		{
			// When IncludeDebugInformation is true, prevents the debug symbols stats from braeking this.
			if (error_string.StartsWith ("WROTE SYMFILE") || error_string.StartsWith ("OffsetTable"))
				return null;

			CompilerError error = new CompilerError();
			Regex reg = new Regex (@"^(\s*(?<file>.*)\((?<line>\d*)(,(?<column>\d*))?\)\s+)*(?<level>\w+)\s*(?<number>.*):\s(?<message>.*)",
				RegexOptions.Compiled | RegexOptions.ExplicitCapture);
			Match match = reg.Match (error_string);
			if (!match.Success)
				return null;
			if (String.Empty != match.Result ("${file}"))
				error.FileName = match.Result ("${file}");
			if (String.Empty != match.Result ("${line}"))
				error.Line = int.Parse (match.Result ("${line}"));
			if (String.Empty != match.Result ("${column}"))
				error.Column = int.Parse (match.Result ("${column}"));
			if (match.Result ("${level}") == "warning")
				error.IsWarning = true;
			error.ErrorNumber = match.Result ("${number}");
			error.ErrorText = match.Result ("${message}");
			return error;
		}

		static string GetTempFileNameWithExtension (
			TempFileCollection temp_files, string extension)
		{
			return temp_files.AddExtension (extension);
		}
	}
}
