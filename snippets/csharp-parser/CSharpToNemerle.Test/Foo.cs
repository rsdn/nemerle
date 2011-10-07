#line 2 //does not alter locations :)

using System;
using System.ComponentModel;
using System.Console;
using System.Linq;
using SCG = System.Collections.Generic;
using LIST = System.Collections.Generic.List<int>;
using SCG;

namespace CSharpToNemerle.Test
{
    // test case for issue #93
    public class TestA
    {
        public static IEnumerable<T> B<T>(IEnumerable<T> source)
        {
            return source.Select(i => i);
        }

        public static IEnumerable<T> C<T>(IEnumerable<T> source)
        {
            return source.Select(delegate {return default(T);});
        }

    }
  /// docs for delegate

  [Description("this is delegate")]
  [return:Description("this is delegate result")]
  public delegate T X<T>(int a, T b) where T : class;

  /// <summary>
  /// docs
  /// for
  /// interface
  /// </summary>
  public interface IVarianceTest<out T> {
    /// method Bar
    T Bar();
  }

  /**
    docs for enum
   */
  public enum A {
    A1 = 10,
    A2,
    A3
  }

  #define X

  #if X
  public static class AExtensions
  {
    public static void TestExtension(this A a)
    {
      Console.WriteLine(a);
    }
  }
  #endif

  public class Foo<T> where T : new()
  {
    public Foo() : base() {  }
    
    /// this is destructor
    ~Foo() {}

    public event EventHandler Bar;

    private EventHandler bla;
    public event EventHandler Bla {
      add { bla = (EventHandler) Delegate.Combine(bla, value); }
      remove { bla = (EventHandler) Delegate.Remove(bla, value); }
    }

    const string F = "Generic ";

    
    public void DoSomething(int x, global::System.String p = "class: ")
    {
      if(x > 0) {
        System.Console.WriteLine("x is greater than 0.");
        checked { ++x; }
      }
      delegate(string p) {
        Console.WriteLine("p Length is {0}", p.Length);
        WriteLine(F + p + typeof(T).FullName);
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

    [property:Description("only property")]
    [Description("only property too")]
    [method:Description("both accessors")]
    [return:Description("shuold be on return value")]
    public string B {
        [Description("getter")]
        get;

        [method:Description("setter")]
        [param:Description("value parameter")]
        set;
    }

    [Description("just on method")]
    [return:Description("shuold be on return value")]
    public override string ToString()
    {
      return string.Format(@"a is {0}, b is ""{1}""", a, B);
    }
  }

  public class Disposable1 : System.IDisposable {
    void IDisposable.Dispose() {}
  }

  struct __<T>
  {
      public T _;

      public __(T _)
      {
          this._ = _;
      }
  }

  class X { public int A { get; set; } public int B { get; set; } }

  public class Container<T1, T2>
  {
    public Container(Func<T1, T2> _f)
    {
    }
  }

  public class Data<T>
  {
  }

  static partial class Program
  {
    static Program() { }

    int[] data = { };

    string[] data2 = { "a", "b" };

    string[,] data3 = { { "a" }, { "b" } };

      int[] data4 = { 1, 2, 3};

    void TestGenericLambda()
    {
        var res = TestA.B(data2);
        foreach (var i in res)
        {
            Console.WriteLine("Element in enumerable is {0}", i);
        }
        var res2 = TestA.C(data4);
        foreach (var i in res2)
        {
            Console.WriteLine("Element in enumerable is {0}", i);
        }
    }

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

      string[] k = { "a", "b" };
      Console.WriteLine("z is [ {0}, {1} ]", k[0], k[1]);
    }

    void TestMagic()
    {
      Magic m = new Magic { A = 10, B = "s" };
      Console.WriteLine("magic: {0}", m);
    }

    void TestFor(int count)
    {
      for(int i = 0, j = 5; i < count /* ... */; ++i, ++j)
        Console.WriteLine("i = {0}, j = {1}", i, j);
    }

    void TestLinq()
    {
        //magic!!
        var Scores = new SCG.List() {
            new { name = "Оля",  score = 97 },
            new { name = "Петя", score = 60 },
            new { name = "Вася", score = 92 },
            new { name = "Маша", score = 81 }
        };

        // Create the query.
        var queryHighScores =
            from rec in Scores
            where rec.score > 80
            orderby rec.score descending, rec.name
            select rec;

        // Execute the query.
        foreach (var rec in queryHighScores)
        {
          Console.WriteLine(rec);
        }
        Console.WriteLine();
    }

    void TestNullCheckOperator(string str)
    {
      Console.WriteLine("Null-checkin operator: {0}", str ?? "was null :)");
    }

    void TestCrazy()
    {
      var x = new __<X>(new X { A = 1,  B = 2 }) { _ = { A = 3, B = 4 } }._;
      Console.WriteLine("A: {0}, B: {1}", x.A, x.B);
    }

    void TestNullable()
    {
      int? nullableX = 10;
      Console.WriteLine(nullableX.HasValue ? "nullableX has value" : "no value in nullableX");
      global::System.Object objX = nullableX;
      Console.WriteLine(objX is int ? "objX is int" : "objX is not int");
      Console.WriteLine(objX is int? ? "objX is int?" : "objX is not int?");
    }

    void TestUnicodeSeq()
    {
      Console.WriteLine((int)'\uD800' == 0xD800);
      Console.WriteLine((int)'\uE000' == 0xE000);
      Console.WriteLine("\U0010FFFF");
    }

    void TestGenericMethod<D>(D data)
    {
      Console.WriteLine(data);
    }

    void TestPrefixInfixChain()
    {
      var i = 10;
      int j = i += 20;
      Console.WriteLine(j == 30);
      j = ++i;
      Console.WriteLine(j == 31);
      j = i--;
      Console.WriteLine(j == 31);
      Console.WriteLine(i == 30);
    }

    void TestLambda()
    {
      var c1 = new Container<string, Data<int>>(_x => new Data<int>());
      var c2 = x => x == 10;
      Console.WriteLine(c1.GetType());
      Console.WriteLine(c2.GetType());
      Func<int, string, string> f0 = delegate { return "OK"; };
      Console.WriteLine(f0(42, "42"));

      Func<int, string> f = delegate { return "OK"; };
      Console.WriteLine(f(42));
   
      Func<string> f2 = delegate { return "OK"; };
      Console.WriteLine(f2());    

      Func<int, string> f3 = x => { return x.ToString(); };
      Console.WriteLine(f3(42));
    }

    void TestDictionaryInitializer()
    {
      var d = new System.Collections.Generic.Dictionary<string, bool>()
      {
        { "a", false },
        { "b", true }
      };
      Console.WriteLine(d["b"]);
    }

    void TestGoto()
    {
      for(int i = 0; i < 100; ++i)
        if(i == 10)
          goto ENDLOOP;
    ENDLOOP:
      Console.WriteLine("GOTO :)");
    }

    void TestVoid1()
    {
      var x = new System.Text.StringBuilder();
      if(true) {
        Console.WriteLine("OK");
        x.Append("OK");
      } else {}
    }

    void TestVoid2()
    {
      var f = true;
      for(;;) { Console.WriteLine("OK"); if(f) break; }
    }

    void TestSwitch()
    {
#line hidden
      var i = 14;
#line default
      switch(i)
      {
        case 0:
        case 1:
          Console.WriteLine("Fail");
          break;

        case 5:
          goto default;

        case 10:
        case 11:
          Console.WriteLine("OK");

        case 14:
          goto case 5;

        default:
          goto case 11;
      }
    }

    void TestConditionals()
    {
#if DEBUG
      Console.WriteLine("DEBUG");
#else
      Console.WriteLine("RELEASE");
#endif
    }

    void TestEscapes()
    {
      var a\u0041 = "AA";
      Console.WriteLine(aA);
    }
  }
}

