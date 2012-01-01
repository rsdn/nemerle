public struct NameOrIndexParameter
{
  public NameOrIndexParameter(string _name) { }
  public NameOrIndexParameter(int _index) { }

  public static implicit operator NameOrIndexParameter(string name)
  {
    return new NameOrIndexParameter(name);
  }

  public static implicit operator NameOrIndexParameter(int index)
  {
    return new NameOrIndexParameter(index);
  }
}

public class Program
{

  static void Test(NameOrIndexParameter _x) { }
  static void Main() { Test("a"); }
}
