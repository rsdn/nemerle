using System;

unsafe class C
{
    int* x = (int*) -1;
}

/*
BEGIN-OUTPUT
VariableInitializer.Expr(
  Expr.CastOperator(
    Expr.UnaryOperator,
    Expr.PointerType(Expr.BuiltInType("int"))
  )
)
END-OUTPUT
*/