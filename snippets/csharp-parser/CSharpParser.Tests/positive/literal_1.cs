class Foo
{
  void Foo()
  {
    Assert.That(3999999999u, Is.EqualTo(4000000000u).Within(5u));
  }
}
/*
BEGIN-OUTPUT
  Literal.Integer where(
    suffix = "u",
    val = 3999999999u
  )
END-OUTPUT
*/