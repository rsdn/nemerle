class P
{
    static void Main()
    {
        using (C? x = new C() + from y in new[] { 1 }
                                join z in new[] { 1 } on y equals z
                                let s = y + z
                                select s is int ? "" : null) { }
    }
}

/*
BEGIN-OUTPUT
  Expr.BinaryOperator(
    left = Expr.NewObject,
    op = Id("+"),
    right = Expr.Query
  )
END-OUTPUT
*/