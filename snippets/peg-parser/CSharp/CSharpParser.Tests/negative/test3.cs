class X {
  void F() {
   a // E: statement
  }
  void G() {
    f();
  }
}
/*
BEGIN-OUTPUT
  Expr.Call(Expr.Ref(Id("f")), [])
END-OUTPUT
*/