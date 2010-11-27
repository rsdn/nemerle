class A
{
    static void Main()
    {
        object[] p = null;
        var q = from x in p
                where x is int?
                select x;
    }
}

/*
BEGIN-OUTPUT
Expr.BinaryOperator(
  left  = Expr.Ref(Id("x")),
  op    = Id("is"),
  right = Expr.NullableType(Expr.BuiltInType("int"))
)
END-OUTPUT
*/