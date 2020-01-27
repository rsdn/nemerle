using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Diagnostics;

using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using Microsoft.Win32;

namespace Nemerle.Tools.MSBuildTask
{
    public class Ncc : ToolTask
    {
        private bool? _checkIntegerOverflow, _delaySign, _treatWarningsAsErrors, _emitDebugInformation;

        public Ncc()
        {
            WarningLevel = 4;
        }

        public string CompilerPath { get; set; }

        public int CompilerStackSize { get; set; }

        public int WarningLevel { get; set; }

        public string[] DisabledWarnings { get; set; }

        public string[] EnabledWarnings { get; set; }

        public bool GreedyReferences { get; set; }

        public bool IndentationSyntax { get; set; }

        public bool NoLogo { get; set; }

        public bool NoStdLib { get; set; }

        public bool NoStdMacros { get; set; }

        public bool Optimize { get; set; }

        public bool RunDebugger { get; set; }

        public bool CheckIntegerOverflow { get { return _checkIntegerOverflow.Value; } set { _checkIntegerOverflow = value; } }

        public bool DelaySign { get { return _delaySign.Value; } set { _delaySign = value; } }

        public bool TreatWarningsAsErrors { get { return _treatWarningsAsErrors.Value; } set { _treatWarningsAsErrors = value; } }

        public bool EmitDebugInformation { get { return _emitDebugInformation.Value; } set { _emitDebugInformation = value; } }

        public string CustomArguments { get; set; }

        public string DefineConstants { get; set; }

        public string DocumentationFile { get; set; }

        public string KeyContainer { get; set; }

        public string KeyFile { get; set; }

        public string MainEntryPoint { get; set; }

        public string Platform { get; set; }

        public string ProjectPath { get; set; }

        public string RootNamespace { get; set; }

        public string TargetType { get; set; }

        public string Win32Icon { get; set; }

        public string Win32Resource { get; set; }

        public string[] AddModules { get; set; }

        public string[] AdditionalLibPaths { get; set; }

        public ITaskItem[] Sources { get; set; }

        public ITaskItem[] References { get; set; }

        public ITaskItem[] MacroReferences { get; set; }

        public ITaskItem[] ResponseFiles { get; set; }

        public ITaskItem[] LinkResources { get; set; }

        public ITaskItem[] Resources { get; set; }

        [Output]
        public ITaskItem OutputAssembly { get; set; }

        protected override string ToolName
        {
            get
            {
#if MONO
                return Environment.OSVersion.VersionString.Contains("Windows") ? "ncc.bat" : "ncc";
#else
                return "ncc.exe";
#endif
            }
        }

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

            // Return the tool name itself.
            // The environment will search common paths for the tool.
            //
            return toolName;
        }

        protected override string GenerateFullPathToTool()
        {
            return FindExecutable(ToolName);
        }

        protected override string GenerateResponseFileCommands()
        {
            try
            {
                var commandLine = new NccCommandLineBuilder();
                AddResponseFileCommandsImpl(commandLine);
                return commandLine.ToString();
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex);
                throw;
            }
        }

        private void AddResponseFileCommandsImpl(NccCommandLineBuilder commandLine)
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

            //System.Diagnostics.Debug.Assert(false);
            if (RunDebugger)
                commandLine.AppendSwitch("\n/debugger");
            if (Optimize)
                commandLine.AppendSwitch("\n/optimize");
            commandLine.AppendPlusOrMinusSwitch("\n/checked", _checkIntegerOverflow);

            commandLine.AppendSwitch("\n/no-color");
            commandLine.AppendSwitchIfNotNull("\n/lib:", AdditionalLibPaths, ",");
            commandLine.AppendSwitchIfNotNull("\n/nowarn:", DisabledWarnings, ",");
            commandLine.AppendSwitchIfNotNull("\n/dowarn:", EnabledWarnings, ",");
            if (NoStdLib)
                commandLine.AppendSwitch("\n/no-stdlib");
            if (NoStdMacros)
                commandLine.AppendSwitch("\n/no-stdmacros");
            if (!GreedyReferences)
                commandLine.AppendSwitch("\n/greedy-references:-");
            if (WarningLevel != 4)
                commandLine.AppendSwitchIfNotNull("\n/warn:", WarningLevel.ToString());
            if (IndentationSyntax)
                commandLine.AppendSwitch("\n/indentation-syntax");
            commandLine.AppendSwitchIfNotNull("\n/doc:", DocumentationFile);
            if (!string.IsNullOrEmpty(DefineConstants))
            {
                var defines = DefineConstants
                    .Split(new char[] { ';', ' ', '\r', '\n', '\t' }, StringSplitOptions.RemoveEmptyEntries);
                commandLine.AppendSwitchUnquotedIfNotNull("\n/define:", String.Join(";", defines));
            }
            commandLine.AppendSwitchIfNotNull("\n/win32res:", Win32Resource);
            commandLine.AppendSwitchIfNotNull("\n/platform:", Platform);

            commandLine.AppendSwitchIfNotNull("\n/addmodule:", AddModules, ",");
            commandLine.AppendPlusOrMinusSwitch("\n/delaysign", _delaySign);
            commandLine.AppendSwitchIfNotNull("\n/keycontainer:", KeyContainer);
            commandLine.AppendSwitchIfNotNull("\n/keyfile:", KeyFile);
            commandLine.AppendSwitchIfNotNull("\n/linkresource:", LinkResources, new[] { "LogicalName", "Access" });
            if (NoLogo)
                commandLine.AppendSwitch("\n/nologo");
            commandLine.AppendSwitchIfNotNull("\n/resource:", Resources, new[] { "LogicalName", "Access" });
            commandLine.AppendSwitchIfNotNull("\n/target:", TargetType);
            commandLine.AppendPlusOrMinusSwitch("\n/warnaserror", _treatWarningsAsErrors);
            commandLine.AppendSwitchIfNotNull("\n/win32icon:", Win32Icon);
            commandLine.AppendPlusOrMinusSwitch("\n/debug", _emitDebugInformation);
            commandLine.AppendSwitchIfNotNull("\n/project-path:", ProjectPath);
            commandLine.AppendSwitchIfNotNull("\n/root-namespace:", RootNamespace);
            commandLine.AppendSwitchIfNotNull("\n/main:", MainEntryPoint);
            if (CompilerStackSize > 0)
                commandLine.AppendSwitchIfNotNull("\n/stack-size:", CompilerStackSize.ToString());

            // Not supported options:
            //commandLine.AppendSwitchWithInteger("\n/codepage:", base.Bag, "CodePage");
            //commandLine.AppendSwitchIfNotNull("/debug:", this.DebugType);
            //commandLine.AppendSwitchWithInteger("\n/filealign:", base.Bag, "FileAlignment");
            //commandLine.AppendWhenTrue("\n/utf8output", base.Bag, "Utf8Output");

            // Add sources
            if (Sources != null)
            {
                commandLine.AppendTextUnquoted("\n\n");
                commandLine.AppendFileNamesIfNotNull(Sources, "\n");
                commandLine.AppendTextUnquoted("\n");
            }

            if (null != ResponseFiles)
            {
                foreach (var it in ResponseFiles)
                    commandLine.AppendSwitchIfNotNull("\n/fromfile:", it.ItemSpec);
            }

            if (null != References)
            {
                foreach (var it in References)
                    commandLine.AppendSwitchIfNotNull("\n/ref:", it.ItemSpec);
            }

            if (null != MacroReferences)
            {
                foreach (var it in MacroReferences)
                    commandLine.AppendSwitchIfNotNull("\n/macros:", it.ItemSpec);
            }

            if (!string.IsNullOrEmpty(CustomArguments))
                commandLine.AppendSwitch(CustomArguments);

            commandLine.AppendSwitchIfNotNull("\n\n/out:", OutputAssembly);
        }

        private sealed class Location
        {
            private static Regex _msgRx = new Regex(@"(?<path>.*?)\(\s*(?<start_line>\d+)\s*,\s*(?<start_char>\d+)\s*(?:,\s*(?<end_line>\d+)\s*,\s*(?<end_char>\d+)\s*)?\):\s*(hint|warning|error)\s*:\s*(?<msg>.*)", RegexOptions.Compiled);
            private static char[] _invalidPathChars = Path.GetInvalidPathChars();
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
                    var path = m.Groups["path"].Value;
                    var startLine = int.Parse(m.Groups["start_line"].Value);
                    var startPos = int.Parse(m.Groups["start_char"].Value);
                    var endLineOpt = m.Groups["end_line"];
                    var endPosOpt = m.Groups["end_char"];
                    var endLine = endLineOpt.Success ? int.Parse(endLineOpt.Value) : startLine;
                    var endPos = endPosOpt.Success ? int.Parse(endPosOpt.Value) : startPos;

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
                    if (str.IndexOfAny(_invalidPathChars) >= 0)
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
                }
            }
        }

        protected override void LogEventsFromTextOutput(string singleLine, MessageImportance importance)
        {
            try
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
            catch (Exception ex)
            {
                Log.LogMessageFromText(singleLine, MessageImportance.High);
                Log.LogMessageFromText(ex.ToString(), MessageImportance.High);
                Console.WriteLine(ex);
            }
        }

        public class NccCommandLineBuilder : CommandLineBuilder
        {
            public void AppendPlusOrMinusSwitch(string switchName, bool? flag)
            {
                if (flag.HasValue)
                    AppendSwitchIfNotNull(switchName, flag.GetValueOrDefault() ? "+" : "-");
            }

            public void AppendSwitchIfNotNull(string switchName, ITaskItem[] parameters, string[] metadataNames)
            {
                if (parameters == null)
                    return;
                foreach (var item in parameters)
                {
                    AppendSwitchIfNotNull(switchName, item.ItemSpec);
                    if (metadataNames == null)
                        continue;
                    foreach (var metadataName in metadataNames)
                    {
                        var metadata = item.GetMetadata(metadataName);
                        if (metadata != null && metadata.Length > 0)
                        {
                            CommandLine.Append(',');
                            AppendTextWithQuoting(metadata);
                        }
                    }
                }
            }
        }
    }
}
