using System;

namespace CSharpToNemerle.Test
{
  public class Foo<T> where T : new()
  {
    public void DoSomething()
    {
      Console.WriteLine("Foo!!!");
    }
  }
}
