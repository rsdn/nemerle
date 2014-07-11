using System;
using System.Collections.Generic;

namespace Nemerle.VisualStudio.Project
{
  static class NetFrameworkProjectConstants
  {
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

    public const string DefaultTargetFrameworkVersion = "v" + FrameworkVersion40;

    public static readonly HashSet<string> ValidTargetFrameworkVersions = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase)
    {
      "v" + FrameworkVersion40,
      "v" + FrameworkVersion45,
      "v" + FrameworkVersion451,
    };

    public static Version VisualStudioVersion = typeof(Microsoft.VisualStudio.Package.LanguageService).Assembly.GetName().Version;
  }
}
