using System;
using System.IO;
using System.Collections;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
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
        
        #if !NET_4_5
        public string Platform { get; set; }
        #endif
        
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

        private sealed class Location
        {
            private static Regex _msgRx = new Regex(@"(?<path>.*?)\(\s*(?<start_line>\d+)\s*,\s*(?<start_char>\d+)\s*(?:,\s*(?<end_line>\d+)\s*,\s*(?<end_char>\d+)\s*)?\):\s*(hint|warning|error)\s*:\s*(?<msg>.*)", RegexOptions.Compiled);
            public string File;
            public int StartLine;
            public int StartPos;
            public int EndLine;
            public int EndPos;

            public static Location Parse(int tagPos, string text)
            {
                if (text == null || tagPos >= text.Length)
                    return new Location();

                var m = _msgRx.Match(text, tagPos); // Try new (MS) format.

                if (m.Success)
                {
                  var path       = m.Groups["path"].Value;
                  var startLine  = int.Parse(m.Groups["start_line"].Value);
                  var startPos   = int.Parse(m.Groups["start_char"].Value);
                  var endLineOpt = m.Groups["end_line"];
                  var endPosOpt  = m.Groups["end_char"];
                  var endLine    = endLineOpt.Success ? int.Parse(endLineOpt.Value) : startLine;
                  var endPos     = endPosOpt.Success  ? int.Parse(endPosOpt.Value)  : startPos;

                  return new Location
                  {
                      File = path,
                      StartLine = startLine,
                      StartPos = startPos,
                      EndLine = endLine,
                      EndPos = endPos
                  };
                }
                else
                {
                  // Try old nemerle format...
                  var str = text.Substring(0, tagPos);
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
                }
            }
        }

        protected override void LogEventsFromTextOutput(string singleLine, MessageImportance importance)
        {
            int tagPos;
            string tag;

            if ((tagPos = singleLine.IndexOf(tag = "error:")) >= 0)
            {
                var loc = Location.Parse(tagPos, singleLine);
                var msg = singleLine.Substring(tagPos + tag.Length + 1);
                Log.LogError(null, null, null, loc.File, loc.StartLine, loc.StartPos, loc.EndLine, loc.EndPos, msg);
            }
            else if ((tagPos = singleLine.IndexOf(tag = "warning:")) >= 0)
            {
                var loc = Location.Parse(tagPos, singleLine);
                var msg = singleLine.Substring(tagPos + tag.Length + 1);
                Log.LogWarning(null, null, null, loc.File, loc.StartLine, loc.StartPos, loc.EndLine, loc.EndPos, msg);
            }
            else if ((tagPos = singleLine.IndexOf(tag = "debug:")) >= 0)
            {
                var loc = Location.Parse(tagPos, singleLine);
                var msg = singleLine.Substring(tagPos + tag.Length + 1);
                Log.LogError(null, null, null, loc.File, loc.StartLine, loc.StartPos, loc.EndLine, loc.EndPos, msg);
            }
            else if ((tagPos = singleLine.IndexOf(tag = "hint:")) >= 0)
            {
                var loc = Location.Parse(tagPos, singleLine);
                var msg = singleLine.Substring(tagPos);
                Log.LogWarning(null, null, null, loc.File, loc.StartLine, loc.StartPos, loc.EndLine, loc.EndPos, msg);
            }
            else
            {
                Log.LogMessageFromText(singleLine, MessageImportance.High);
            }
        }

        protected override string GetResponseFileSwitch(string responseFilePath)
        {
            return "/from-file:\"" + responseFilePath + "\"";
        }
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
