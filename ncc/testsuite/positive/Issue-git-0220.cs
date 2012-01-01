using System.Diagnostics;

public class Program
{
  public int MappingSchema
  {
    [DebuggerStepThrough]
    get { return 0; }
    set { var _ = value; }
  }

  static void Main() { }
}