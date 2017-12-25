using System;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;

namespace Nemerle.Tools.MSBuildTask
{
  public class NSetEnvironmentVariable : Task
  {
    [Required]
    public string Variable { get; set; }

    [Required]
    public string Value { get; set; }

    public override bool Execute()
    {
      try
      {
        Environment.SetEnvironmentVariable(Variable, Value);
        return true;
      }
      catch (Exception ex)
      {
        Log.LogErrorFromException(ex);
        return false;
      }
    }
  }
}
