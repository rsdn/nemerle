using System;
using System.Console;
using SCG = System.Collections.Generic;
using LIST = System.Collections.Generic.List<int>;

namespace CSharpToNemerle.Test
{
  public delegate T X<T>(int a, T b) where T : class;

  public enum A {
    A1 = 10,
    A2,
    A3
  }

  public class Foo<T> where T : new()
  {
    public void DoSomething(int x, global::System.String p = "class: ")
    {
      if(x > 0)
        System.Console.WriteLine("x is greater than 0.");
      delegate(string p) {
        WriteLine("Generic " + p + typeof(T).FullName);
      }(p);
    }
  }

  static partial class Program 
  {
    void TestPartial()
    {
      Console.WriteLine("Partial works!");
    }

    void TestAlias()
    {
      Console.WriteLine(typeof(LIST));
    }
  }
}
