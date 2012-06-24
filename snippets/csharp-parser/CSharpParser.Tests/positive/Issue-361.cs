class Foo
{
  int x = 1 - 2 * 3 + 4;
}

/*
BEGIN-OUTPUT
  Expr.BinaryOperator where (
    op = Id("+"),
    left = Expr.BinaryOperator where (
      op = Id("-"),
      left = Expr.Literal where (
        literal = Literal.Integer where ( val = 1 )
      ),
      right = Expr.BinaryOperator where (
        op = Id("*"),
        left = Expr.Literal where (
          literal = Literal.Integer where ( val = 2 )
        ),
        right = Expr.Literal where (
          literal = Literal.Integer where ( val = 3 )
        )
      )
    ),
    right = Expr.Literal where (
      literal = Literal.Integer where ( val = 4 )
    )
  )
END-OUTPUT
*/