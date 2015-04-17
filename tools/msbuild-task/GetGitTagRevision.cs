using System;
using System.Diagnostics;
using System.IO;
using System.Text.RegularExpressions;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;

namespace Nemerle.Tools.MSBuildTask
{
  public class GetGitTagRevision : Task
  {
    [Required]
    public string WorkingDirectory { get; set; }

    [Output]
    public string GitTag { get; set; }

    [Output]
    public string GitTagAsVersion { get; set; }

    [Output]
    public string GitRevision { get; set; }

    [Output]
    public string GitCommitHash { get; set; }

    public override bool Execute()
    {
      if (string.IsNullOrEmpty(WorkingDirectory))
      {
        Log.LogError("Working directory must be specified");
        return false;
      }
      if (!Directory.Exists(WorkingDirectory))
      {
        Log.LogError("Working directory '{0}' not found", WorkingDirectory);
        return false;
      }
      return executeGit(commonStartInfoConfigurator, false) || executeGit(cmdStartInfoConfigurator, true);
    }

    private static void commonStartInfoConfigurator(ProcessStartInfo startInfo)
    {
      // mono git or msysgit with git.exe in PATH
      startInfo.FileName = Environment.GetEnvironmentVariable("GIT_PATH") ?? "git";
      startInfo.Arguments = "describe --tags --long";
    }

    private static void cmdStartInfoConfigurator(ProcessStartInfo startInfo)
    {
      // PATH conatains git.cmd only workaround
      startInfo.FileName = "cmd";
      startInfo.Arguments = "/C git describe --tags --long";
    }

    private bool executeGit(Action<ProcessStartInfo> startInfoConfigurator, bool reportErrors)
    {
      var startInfo = new ProcessStartInfo
      {
        UseShellExecute = false,
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        WorkingDirectory = WorkingDirectory,
        CreateNoWindow = true
      };
      startInfoConfigurator(startInfo);
      try
      {
        Log.LogCommandLine(MessageImportance.Low, startInfo.FileName + " " + startInfo.Arguments);

        using (var process = Process.Start(startInfo))
        {
          for (var line = process.StandardOutput.ReadLine(); line != null; line = process.StandardOutput.ReadLine())
          {
            var lineMatch = _mainPattern.Match(line);
            if (lineMatch.Success)
            {
              GitTag = lineMatch.Groups["tag"].Value;
              var versionMatch = _versionPattern.Match(GitTag);
              if (versionMatch.Success)
                GitTagAsVersion = versionMatch.Groups["version"].Value;
              GitRevision = lineMatch.Groups["revision"].Value;
              GitCommitHash = lineMatch.Groups["commit"].Value;
              return true;
            }
          }
          // Wait for git client process to terminate
          if (!process.WaitForExit (2000))
            process.Kill ();

          if (reportErrors)
          {
            Log.LogWarning("Tag, revision and commit hash not found in git output.");
            GitTagAsVersion = "1.0";
            GitRevision     = "0";
            GitCommitHash   = "00000000";
            return true;
          }
        }
      }
      catch(Exception ex)
      {
        if (reportErrors) Log.LogErrorFromException(ex);
      }
      return false;
    }

    private static readonly Regex _mainPattern = new Regex(@"^(?<tag>.+)\-(?<revision>.+)\-(?<commit>.+)$", RegexOptions.Compiled);

    private static readonly Regex _versionPattern = new Regex(@"v(?<version>\d+(\.\d+){1,3})", RegexOptions.Compiled);
  }
}
