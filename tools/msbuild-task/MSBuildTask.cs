/*
 * Copyright (c) 2010 Stanislav Matveev
 * Originaly created by Kamil Skalski <nazgul at nemerle.org>
 *
 * Copyright (c) 2005-2008 The University of Wroclaw.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *    3. The name of the University may not be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 * NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

using System;
using System.IO;
using System.Collections;
using System.Reflection;
using System.Text;
using System.Diagnostics;

using Microsoft.Build.Framework;
using Microsoft.Build.Tasks;
using Microsoft.Build.Utilities;
using Microsoft.Win32;

namespace Nemerle.Tools.MSBuildTask
{

	public class Ncc : ManagedCompiler
	{

		public Ncc()
		{
			WarningLevel = 4;
		}

		public string CompilerPath { get; set; }
		public int WarningLevel { get; set; }
		public string[] DisabledWarnings { get; set; }
		public string[] EnabledWarnings { get; set; }
		public string DocumentationFile { get; set; }
		public bool NoStdLib { get; set; }
		public bool NoStdMacros { get; set; }
		public bool IndentationSyntax { get; set; }
		public bool GreedyReferences { get; set; }
		public string Platform { get; set; }
		public bool RunDebugger { get; set; }
		public string ProjectPath { get; set; }
		public string RootNamespace { get; set; }
		public int CompilerStackSize { get; set; }
		public string CustomArguments { get; set; }
		public ITaskItem[] MacroReferences
		{
			get { return (ITaskItem[])Bag["MacroReferences"]; }
			set { Bag["MacroReferences"] = value; }
		}
		public bool CheckIntegerOverflow
		{
			get { return (bool)Bag["CheckIntegerOverflow"]; }
			set { Bag["CheckIntegerOverflow"] = value; }
		}

#if MONO
				protected override string ToolName
				{
						get { 
								if(Environment.OSVersion.VersionString.Contains("Windows"))
										return "ncc.bat";
								else
										return "ncc";
						}
				}
#else
		protected override string ToolName
		{
			get { return "ncc.exe"; }
		}
#endif

		private string FindExecutable(string toolName)
		{
			if (!string.IsNullOrEmpty(CompilerPath))
			{
				var path = Path.Combine(CompilerPath, toolName);

				if (File.Exists(path))
					return path;
			}

			var my_file = new Uri(typeof(Ncc).Assembly.CodeBase).LocalPath;
			var ncc_file = Path.Combine(Path.GetDirectoryName(my_file), toolName);

			if (File.Exists(ncc_file))
			{
				// The tool found in the same folder.
				//
				return ncc_file;
			}
			else
			{
				// Query the shell association
				//
				var regKeyName = @"SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\" + toolName;
				var regKey = Registry.LocalMachine.OpenSubKey(regKeyName);

				if (null != regKey)
				{
					// The tool is registered with the Shell API.
					//
					return (string)regKey.GetValue(null);
				}
				else
				{
					// Return the tool name itself.
					// The environment will search common paths for the tool.
					//
					return toolName;
				}
			}
		}

		protected override string GenerateFullPathToTool()
		{
			return FindExecutable(ToolName);
		}

		protected override void AddResponseFileCommands(CommandLineBuilderExtension commandLine)
		{
			try
			{
				AddResponseFileCommandsImpl(commandLine);
			}
			catch (Exception ex)
			{
				Trace.Assert(false, ex.ToString());
				throw;
			}
		}

		protected void AddResponseFileCommandsImpl(CommandLineBuilderExtension commandLine)
		{
			if (OutputAssembly == null && Sources != null && Sources.Length > 0 && ResponseFiles == null)
			{
				try
				{
					OutputAssembly = new TaskItem(Path.GetFileNameWithoutExtension(Sources[0].ItemSpec));
				}
				catch (ArgumentException exception)
				{
					throw new ArgumentException(exception.Message, "Sources", exception);
				}

				var outputAssembly = OutputAssembly;
				switch (TargetType.ToLowerInvariant())
				{
					case "library":
						outputAssembly.ItemSpec = outputAssembly.ItemSpec + ".dll";
						break;

					case "module":
						outputAssembly.ItemSpec = outputAssembly.ItemSpec + ".netmodule";
						break;

					default:
						outputAssembly.ItemSpec = outputAssembly.ItemSpec + ".exe";
						break;
				}
			}

			// Don't call base.AddResponseFileCommands()!
			//base.AddResponseFileCommands(commandLine);

			//System.Diagnostics.Debug.Assert(false);
			if (RunDebugger)
				commandLine.AppendSwitch("\n/debugger");
			if (Optimize)
				commandLine.AppendSwitch("\n/optimize");
			commandLine.AppendPlusOrMinusSwitch("\n/checked", base.Bag, "CheckIntegerOverflow");

			commandLine.AppendSwitch("\n/no-color");
			commandLine.AppendSwitchIfNotNull("\n/lib:", base.AdditionalLibPaths, ",");
			commandLine.AppendSwitchIfNotNull("\n/nowarn:", this.DisabledWarnings, ",");
			commandLine.AppendSwitchIfNotNull("\n/dowarn:", this.EnabledWarnings, ",");
			if (NoStdLib)
				commandLine.AppendSwitch("\n/no-stdlib");
			if (NoStdMacros)
				commandLine.AppendSwitch("\n/no-stdmacros");
			if (!GreedyReferences)
				commandLine.AppendSwitch("\n/greedy-references:-");
			if (WarningLevel != 4)
				commandLine.AppendSwitchIfNotNull("\n/warn:", this.WarningLevel.ToString());
			if (IndentationSyntax)
				commandLine.AppendSwitch("\n/indentation-syntax");
			commandLine.AppendSwitchIfNotNull("\n/doc:", this.DocumentationFile);
			if (!string.IsNullOrEmpty(base.DefineConstants))
			{
				var defines = base.DefineConstants
					.Split(new char[] { ';', ' ', '\r', '\n', '\t' }, StringSplitOptions.RemoveEmptyEntries);
				commandLine.AppendSwitchUnquotedIfNotNull("\n/define:", String.Join(";", defines));
			}
			commandLine.AppendSwitchIfNotNull("\n/win32res:", base.Win32Resource);
			commandLine.AppendSwitchIfNotNull("\n/platform:", this.Platform);

			// Switchs from base.AddResponseFileCommands()
			commandLine.AppendSwitchIfNotNull("\n/addmodule:", this.AddModules, ",");
			commandLine.AppendPlusOrMinusSwitch("\n/delaysign", base.Bag, "DelaySign");
			commandLine.AppendSwitchIfNotNull("\n/keycontainer:", this.KeyContainer);
			commandLine.AppendSwitchIfNotNull("\n/keyfile:", this.KeyFile);
			commandLine.AppendSwitchIfNotNull("\n/linkresource:", this.LinkResources, new[] { "LogicalName", "Access" });
			if (NoLogo)
				commandLine.AppendSwitch("\n/nologo");
			commandLine.AppendSwitchIfNotNull("\n/resource:", this.Resources, new[] { "LogicalName", "Access" });
			commandLine.AppendSwitchIfNotNull("\n/target:", this.TargetType);
			commandLine.AppendPlusOrMinusSwitch("\n/warnaserror", base.Bag, "TreatWarningsAsErrors");
			commandLine.AppendSwitchIfNotNull("\n/win32icon:", this.Win32Icon);
			commandLine.AppendPlusOrMinusSwitch("\n/debug", base.Bag, "EmitDebugInformation");
			commandLine.AppendSwitchIfNotNull("\n/project-path:", this.ProjectPath);
			commandLine.AppendSwitchIfNotNull("\n/root-namespace:", this.RootNamespace);
			commandLine.AppendSwitchIfNotNull("\n/main:", this.MainEntryPoint);
			if (CompilerStackSize > 0)
				commandLine.AppendSwitchIfNotNull("\n/stack-size:", this.CompilerStackSize.ToString());

			// Not supported options:
			//commandLine.AppendSwitchWithInteger("\n/codepage:", base.Bag, "CodePage");
			//commandLine.AppendSwitchIfNotNull("/debug:", this.DebugType);
			//commandLine.AppendSwitchWithInteger("\n/filealign:", base.Bag, "FileAlignment");
			//commandLine.AppendWhenTrue("\n/utf8output", base.Bag, "Utf8Output");

			// Add sources
			if (this.Sources != null)
			{
				commandLine.Append("\n\n");
				commandLine.AppendFileNamesIfNotNull(this.Sources, "\n");
				commandLine.Append("\n");
			}

			if (null != base.ResponseFiles)
			{
				foreach (var it in base.ResponseFiles)
					commandLine.AppendSwitchIfNotNull("\n/fromfile:", it.ItemSpec);
			}

			if (null != base.References)
			{
				foreach (var it in base.References)
					commandLine.AppendSwitchIfNotNull("\n/ref:", it.ItemSpec);
			}

			if (null != this.MacroReferences)
			{
				foreach (var it in this.MacroReferences)
					commandLine.AppendSwitchIfNotNull("\n/macros:", it.ItemSpec);
			}

			if (!string.IsNullOrEmpty(CustomArguments))
				commandLine.AppendSwitch(CustomArguments);

			commandLine.AppendSwitchIfNotNull("\n\n/out:", OutputAssembly);
		}

		private class Location
		{
			public string File;
			public int StartLine;
			public int StartPos;
			public int EndLine;
			public int EndPos;
		}

		protected override void LogEventsFromTextOutput(string singleLine, MessageImportance importance)
		{
			// System.Diagnostics.Trace.Assert(false);

			var get_location = new Func<string, Location>(
					(before) =>
					{
						var str = singleLine.Substring(0, singleLine.IndexOf(before));
						if (string.IsNullOrEmpty(str))
							return new Location();
						// Path can contain ':'. We should skip it...
						var dir = str.StartsWith(":") ? "" : Path.GetDirectoryName(str);
						// Find first location separator (it's a end of path)
						var locIndex = str.IndexOf(':', dir.Length);
						var path = (locIndex <= 0) ? dir : str.Substring(0, locIndex);
						var locStr = str.Substring(locIndex);
						var parts = locStr.Trim().Trim(':').Split(':');
						switch (parts.Length)
						{
							case 2:
								var line = int.Parse(parts[0]);
								var pos = int.Parse(parts[1]);
								return new Location
								{
									File = path,
									StartLine = line,
									StartPos = pos,
									EndLine = line,
									EndPos = pos + 1
								};
							case 4:
								return new Location
								{
									File = path,
									StartLine = int.Parse(parts[0]),
									StartPos = int.Parse(parts[1]),
									EndLine = int.Parse(parts[2]),
									EndPos = int.Parse(parts[3])
								};
							default:
								return new Location
								{
									File = path
								};
						}
					});

			var try_report = new Func<string, Action<Location, string>, bool>(
					(tag, action) =>
					{
						var index = singleLine.IndexOf(tag);
						if (index < 0)
							return false;
						var loc = get_location(tag);
						action(loc, singleLine.Substring((tag == "hint:") ? index : index + tag.Length + 1));
						return true;
					});

			var log_error = new Action<Location, string>(
					(loc, msg) =>
					{
						Log.LogError(null, null, null, loc.File, loc.StartLine, loc.StartPos, loc.EndLine, loc.EndPos, msg);
					});

			var log_warning = new Action<Location, string>(
					(loc, msg) =>
					{
						Log.LogWarning(null, null, null, loc.File, loc.StartLine, loc.StartPos, loc.EndLine, loc.EndPos, msg);
					});

			var _ = try_report("error:", log_error) || try_report("warning:", log_warning)
					|| try_report("debug:", log_error) || try_report("hint:", log_warning)
					|| Log.LogMessageFromText(singleLine, MessageImportance.High);
			if (_) return; // -- warning CS0219... 
		}

		protected override string GetResponseFileSwitch(string responseFilePath)
		{
			return "/from-file:\"" + responseFilePath + "\"";
		}

		/*protected override void LogToolCommand(string message)
		{
				Log.LogMessageFromText("Command:", MessageImportance.Low);
				Log.LogMessageFromText(message, MessageImportance.Normal);
		}*/

	}

	public static class CommandLineBuilderNemerleExtension
	{

		public static void Append(this CommandLineBuilder commandLine, string text)
		{
			GetCommandLine(commandLine).Append(text);
		}

		public static void AppendPlusOrMinusSwitch(this CommandLineBuilder commandLine, string switchName, Hashtable bag, string parameterName)
		{
			var flag = bag[parameterName];
			if (flag != null)
				commandLine.AppendSwitchIfNotNull(switchName, (bool)flag ? "+" : "-");
		}

		public static void AppendSwitchIfNotNull(this CommandLineBuilder commandLine, string switchName, ITaskItem[] parameters, string[] metadataNames)
		{
			if (parameters == null)
				return;
			foreach (var item in parameters)
			{
				commandLine.AppendSwitchIfNotNull(switchName, item.ItemSpec);
				if (metadataNames == null)
					continue;
				foreach (var metadataName in metadataNames)
				{
					var metadata = item.GetMetadata(metadataName);
					if (metadata != null && metadata.Length > 0)
					{
						GetCommandLine(commandLine).Append(',');
						AppendTextWithQuoting(commandLine, metadata);
					}
				}
			}
		}

		#region Accessors to protected members of CommandLineBuilder

		private static StringBuilder GetCommandLine(CommandLineBuilder commandLine)
		{
			return (StringBuilder)commandLine.GetType()
					.GetProperty("CommandLine", BindingFlags.Instance | BindingFlags.NonPublic)
					.GetValue(commandLine, null);
		}

		private static void AppendTextWithQuoting(CommandLineBuilder commandLine, string text)
		{
			commandLine.GetType()
					.GetMethod("AppendTextWithQuoting", BindingFlags.Instance | BindingFlags.NonPublic)
					.Invoke(commandLine, new[] { text });
		}

		#endregion

	}

}
