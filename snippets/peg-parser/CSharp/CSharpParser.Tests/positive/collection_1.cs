class Foo
{
  string[] array = { "abc", "bca" };
}
/*
BEGIN-OUTPUT
VariableInitializer.Complex(
  [
    VariableInitializer.Expr(Expr.Literal(Literal.String("abc", false))),
    VariableInitializer.Expr(Expr.Literal(Literal.String("bca", false)))
  ]
)
END-OUTPUT
*/