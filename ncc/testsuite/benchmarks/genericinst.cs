class C1 <T> { }
class C2 <T> { }
class C3 <T> { }
class C4 <T> { }

class C5 <T> { }
class C6 <T> { }
class C7 <T> { }
class C8 <T> { }

class M {
  static int dupl<T> (int cnt, T unused)
  {
    if (cnt < 0)
      return 1;
    else return
      dupl (cnt - 1, new C1<T> ()) +
      dupl (cnt - 1, new C2<T> ()) +
      dupl (cnt - 1, new C3<T> ()) +
      dupl (cnt - 1, new C4<T> ()) +
      dupl (cnt - 1, new C5<T> ()) +
      dupl (cnt - 1, new C6<T> ()) +
      dupl (cnt - 1, new C7<T> ()) +
      dupl (cnt - 1, new C8<T> ());
  }

  public static void Main ()
  {
    string a = System.Environment.GetCommandLineArgs () [1];
    System.Console.WriteLine (dupl (int.Parse (a), "foo"));
  }
}
