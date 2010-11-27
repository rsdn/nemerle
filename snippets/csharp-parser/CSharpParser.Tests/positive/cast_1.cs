class Foo
{
  object x = (X)null;
}
/*
BEGIN-OUTPUT
Expr.CastOperator(
  Expr.Literal(Literal.Null),
  Expr.Ref(Id("X"))
)
END-OUTPUT
*/