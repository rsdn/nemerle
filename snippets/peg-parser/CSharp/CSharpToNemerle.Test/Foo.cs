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
      if(x > 0) {
        System.Console.WriteLine("x is greater than 0.");
        ++x;
      }
      delegate(string p) {
        Console.WriteLine("p Length is {0}", p.Length);
        WriteLine("Generic " + p + typeof(T).FullName);
      }(p);
      const int a = 10;
      var b = x + a,
          c = x * a;
      b += 44;
      Console.WriteLine("b is {0}, c is {1}", b, c);

      var list = new SCG.List<int>(10) { a, b, c * 2 };
      foreach(var x in list)
        Console.WriteLine("collection element {0}", x);

      object obj1 = new {};
      var obj2 = new { a, B = b, C = c + 1, int.MaxValue };
      Console.WriteLine("Anonymous type test: obj1 {0}, obj2 {1}", obj1, obj2);
    }
  }

  // some magic!
  public class Magic {
    [Nemerle.Utility.Accessor(flags = WantSetter)] int a;
    [Nemerle.Utility.Accessor(flags = WantSetter)] string b;
  }
  
  static partial class Program 
  {
    void TestAlias()
    {
      Console.WriteLine("Alias LIST is {0}", typeof(LIST));
    }

    void TestArray()
    {
      var x = new byte[] { 1, 2, 3, 4 };
      Console.WriteLine("x is {0}: {1}, {2}, {3}, {4}", x, x[0], x[1], x[2], x[3]);

      var y = new long[,] { { 1, 2 }, { 3, 4 } };
      Console.WriteLine("y is {0}: {1}, {2}, {3}, {4}", y, y[0, 0], y[0, 1], y[1, 0], y[1, 1]);

      var z = new short[][] { new[]{ (short)1, 2 }, new short[]{ 3, 4 } }; // "(short)" is issue :(
      Console.WriteLine("z is {0}: {1}, {2}, {3}, {4}", z, z[0][0], z[0][1], z[1][0], z[1][1]);
    }

    void TestMagic() 
    {
      Magic m = new Magic { A = 10, B = "s" };
      Console.WriteLine("magic: a is {0}, b is {1}", m.A, m.B);
    }
  }
}
