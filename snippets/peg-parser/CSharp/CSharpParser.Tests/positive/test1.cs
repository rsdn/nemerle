namespace X
{
  class Foo
  {
    struct Bar {}
  }
}

/*
BEGIN-OUTPUT
  TypeDeclaration.Struct where (
    Name = Id("Bar")
  )
END-OUTPUT
*/