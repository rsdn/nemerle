class A
{
  int[] x = { };
}
// в принципе было бы достаточно одного
//    VariableInitializer.Complex([])

/*
BEGIN-OUTPUT
TypeMember.Field where (
  memberType = Expr.ArrayType(Expr.BuiltInType("int"), 1),
  fields = [ (_, Expr.Ref(Id("x")), VariableInitializer.Complex([])) ]
)
END-OUTPUT
*/