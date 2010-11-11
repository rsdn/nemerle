// NODE:    TypeDeclaration
// PATTERN: TypeDeclaration.Struct where ( Name = Identifier where ( Id = "Bar" ) )
namespace X
{
  class Foo
  {
    struct Bar {}
  }
}