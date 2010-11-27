class A
{
    static void Foo()
    {
        a: //E: statement
    }
}
/*
BEGIN-OUTPUT
  TypeMember.Method where (name = Expr.Ref(Id("Foo")))
END-OUTPUT
*/