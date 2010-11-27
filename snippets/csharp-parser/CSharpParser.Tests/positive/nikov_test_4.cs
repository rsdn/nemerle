using System;

class C : IDisposable
{
    public void Dispose() {}

    public static C operator *(C x, C y)
    {
        using (x * y * y)
        {
            return x;
        }
    }
}
/*
BEGIN-OUTPUT
  Statement.UsingExpr where (
    expr = Expr.BinaryOperator(
      left = Expr.BinaryOperator,
      op   = Id("*"),
      right = Expr.Ref(Id("y"))
    )
  )
END-OUTPUT
*/