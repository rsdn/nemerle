using System;
using System.Console;

namespace CSharpToNemerle.Test
{
  public class Foo<T> where T : new()
  {
    public void DoSomething(int x, string p = "class: ")
    {
      if(x > 0)
        System.Console.WriteLine("x is greater than 0.");
      WriteLine("Generic " + p + typeof(T).FullName);
    }
  }

  partial static class Program 
  {
    void TestPartial()
    {
      Console.WriteLine("Partial works!");
    }
  }
}
