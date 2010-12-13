#define X
#define Y
#define Z

#if false || X && (true || !false) // evaluates to 'true'
  #if Y
    #undef Z
  #else 
    #define Z
  #endif
#endif

#if !Z
  class Foo
  {
  }
#endif

/*
BEGIN-OUTPUT
  TypeDeclaration.Class where (
    Name = Id("Foo")
  )
END-OUTPUT
*/