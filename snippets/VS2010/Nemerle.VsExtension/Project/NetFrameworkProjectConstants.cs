using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Nemerle.VisualStudio.Project
{
  static class NetFrameworkProjectConstants
  {
    private const string FrameworkVersion48 = "4.8";
    private const string FrameworkVersion472 = "4.7.2";
    private const string FrameworkVersion471 = "4.7.1";
    private const string FrameworkVersion47 = "4.7";
    private const string FrameworkVersion462 = "4.6.2";
    private const string FrameworkVersion461 = "4.6.1";
    private const string FrameworkVersion46 = "4.6";
    private const string FrameworkVersion452 = "4.5.2";
    private const string FrameworkVersion451 = "4.5.1";
    private const string FrameworkVersion45 = "4.5";
    private const string FrameworkVersion40 = "4.0";

    public const string ToolsVersion = "4.0";
    public const string NemerleVersion = "Net-" + ToolsVersion;

    public static readonly HashSet<string> ValidNemerleVersions = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase)
    {
        "Net-" + FrameworkVersion40
    };

    public const string NemerleBinPathRoot = @"$(ProgramFiles)\Nemerle";

    public static readonly HashSet<string> OldNemerlePropertyValues = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase)
    {
      @"$(ProgramFiles)\Nemerle",
      @"$(ProgramFiles)\Nemerle\Net-3.5",
      @"$(ProgramFiles)\Nemerle\Net-4.0"
    };

    public const string NemerleProperty = @"$(NemerleBinPathRoot)\$(NemerleVersion)";

    public static string GetDefaultTargetFrameworkVersion()
    {
      switch (VisualStudioVersion)
      {
        case 10: return "v" + FrameworkVersion40;
        case 11: return "v" + FrameworkVersion45;
        case 12: return "v" + FrameworkVersion451;
        case 14: return "v" + FrameworkVersion46;
        case 15: return "v" + FrameworkVersion462;
        case 16: return "v" + FrameworkVersion472;
        default: return "v" + FrameworkVersion40;
      }
    }

    public static readonly HashSet<string> ValidTargetFrameworkVersions = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase)
    {
      "v" + FrameworkVersion40,
      "v" + FrameworkVersion45,
      "v" + FrameworkVersion451,
      "v" + FrameworkVersion452,
      "v" + FrameworkVersion46,
      "v" + FrameworkVersion461,
      "v" + FrameworkVersion462,
      "v" + FrameworkVersion47,
      "v" + FrameworkVersion471,
      "v" + FrameworkVersion472,
      "v" + FrameworkVersion48,
    };

    public static int VisualStudioVersion = Process.GetCurrentProcess().MainModule.FileVersionInfo.FileMajorPart;
  }
}
