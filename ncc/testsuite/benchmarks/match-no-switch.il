.assembly extern mscorlib
{
  .ver 1:0:5000:0
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 ) // .z\V.4..
}
.assembly extern Nemerle.stage1
{
  .ver 0:3:1:30685
  .publickeytoken = (E0 80 A9 C7 24 E2 BF CD ) // ....$...
}
.assembly 'out'
{
  .custom instance void class [mscorlib]System.Reflection.AssemblyConfigurationAttribute::.ctor(string) =  (
		01 00 14 43 6F 6E 74 61 69 6E 73 4E 65 6D 65 72   // ...ContainsNemer
		6C 65 54 79 70 65 73 00 00                      ) // leTypes..

  .hash algorithm 0x00008004
  .ver  0:0:0:0
}
.module 'out' // GUID = {5C9024A8-20DB-400B-9F8F-D0FE2ABBA7A6}


.namespace a
{
  .class private auto ansi abstract A
  	extends [mscorlib]System.Object
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.VariantAttribute::.ctor(string) =  (
		01 00 80 92 61 2E 41 2B 58 31 2C 61 2E 41 2B 58   // ....a.A+X1,a.A+X
		32 2C 61 2E 41 2B 58 33 2C 61 2E 41 2B 58 31 61   // 2,a.A+X3,a.A+X1a
		2C 61 2E 41 2B 58 32 61 2C 61 2E 41 2B 58 33 61   // ,a.A+X2a,a.A+X3a
		2C 61 2E 41 2B 58 31 62 2C 61 2E 41 2B 58 32 62   // ,a.A+X1b,a.A+X2b
		2C 61 2E 41 2B 58 33 62 2C 61 2E 41 2B 58 31 63   // ,a.A+X3b,a.A+X1c
		2C 61 2E 41 2B 58 32 63 2C 61 2E 41 2B 58 33 63   // ,a.A+X2c,a.A+X3c
		2C 61 2E 41 2B 58 31 61 63 2C 61 2E 41 2B 58 32   // ,a.A+X1ac,a.A+X2
		61 63 2C 61 2E 41 2B 58 33 61 63 2C 61 2E 41 2B   // ac,a.A+X3ac,a.A+
		58 31 62 63 2C 61 2E 41 2B 58 32 62 63 2C 61 2E   // X1bc,a.A+X2bc,a.
		41 2B 58 33 62 63 00 00                         ) // A+X3bc..

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 2C 2E 74 69 28 2E 74 70 28 29 28 53 79 73   // ..,.ti(.tp()(Sys
		74 65 6D 2E 4F 62 6A 65 63 74 28 29 29 28 53 79   // tem.Object())(Sy
		73 74 65 6D 2E 4F 62 6A 65 63 74 28 29 29 29 00   // stem.Object())).
		00                                              ) // .


    // method line 1
    .method family hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x20ec
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void object::.ctor()
	IL_0006:  ret 
    } // end of method A::instance default void .ctor () 

    // method line 2
    .method public virtual  hidebysig  abstract  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x0
    } // end of method A::instance default int32 _N_GetVariantCode () 

  .class nested famorassem auto ansi sealed X1
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X1 _N_constant_object

    // method line 3
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x20f4
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X1::instance default void .ctor () 

    // method line 4
    .method public static  hidebysig  specialname 
           default class a.A/X1 _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x20fc
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1 a.A/X1::_N_constant_object
	IL_0005:  ret 
    } // end of method X1::default class a.A/X1 _N_constant_object_generator () 

    // method line 5
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2104
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X1::.ctor()
	IL_0005:  stsfld  class a.A/X1 a.A/X1::_N_constant_object
	IL_000a:  ret 
    } // end of method X1::default void .cctor () 

    // method line 6
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2110
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.0 
	IL_0001:  ret 
    } // end of method X1::instance default int32 _N_GetVariantCode () 

  } // end of class X1

  .class nested famorassem auto ansi sealed X2
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X2 _N_constant_object

    // method line 7
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2114
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X2::instance default void .ctor () 

    // method line 8
    .method public static  hidebysig  specialname 
           default class a.A/X2 _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x211c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2 a.A/X2::_N_constant_object
	IL_0005:  ret 
    } // end of method X2::default class a.A/X2 _N_constant_object_generator () 

    // method line 9
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2124
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2::.ctor()
	IL_0005:  stsfld  class a.A/X2 a.A/X2::_N_constant_object
	IL_000a:  ret 
    } // end of method X2::default void .cctor () 

    // method line 10
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2130
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.1 
	IL_0001:  ret 
    } // end of method X2::instance default int32 _N_GetVariantCode () 

  } // end of class X2

  .class nested famorassem auto ansi sealed X3
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X3 _N_constant_object

    // method line 11
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2134
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X3::instance default void .ctor () 

    // method line 12
    .method public static  hidebysig  specialname 
           default class a.A/X3 _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x213c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3 a.A/X3::_N_constant_object
	IL_0005:  ret 
    } // end of method X3::default class a.A/X3 _N_constant_object_generator () 

    // method line 13
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2144
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3::.ctor()
	IL_0005:  stsfld  class a.A/X3 a.A/X3::_N_constant_object
	IL_000a:  ret 
    } // end of method X3::default void .cctor () 

    // method line 14
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2150
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.2 
	IL_0001:  ret 
    } // end of method X3::instance default int32 _N_GetVariantCode () 

  } // end of class X3

  .class nested famorassem auto ansi sealed X1a
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X1a _N_constant_object

    // method line 15
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2154
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X1a::instance default void .ctor () 

    // method line 16
    .method public static  hidebysig  specialname 
           default class a.A/X1a _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x215c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1a a.A/X1a::_N_constant_object
	IL_0005:  ret 
    } // end of method X1a::default class a.A/X1a _N_constant_object_generator () 

    // method line 17
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2164
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X1a::.ctor()
	IL_0005:  stsfld  class a.A/X1a a.A/X1a::_N_constant_object
	IL_000a:  ret 
    } // end of method X1a::default void .cctor () 

    // method line 18
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2170
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.3 
	IL_0001:  ret 
    } // end of method X1a::instance default int32 _N_GetVariantCode () 

  } // end of class X1a

  .class nested famorassem auto ansi sealed X2a
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X2a _N_constant_object

    // method line 19
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2174
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X2a::instance default void .ctor () 

    // method line 20
    .method public static  hidebysig  specialname 
           default class a.A/X2a _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x217c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2a a.A/X2a::_N_constant_object
	IL_0005:  ret 
    } // end of method X2a::default class a.A/X2a _N_constant_object_generator () 

    // method line 21
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2184
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2a::.ctor()
	IL_0005:  stsfld  class a.A/X2a a.A/X2a::_N_constant_object
	IL_000a:  ret 
    } // end of method X2a::default void .cctor () 

    // method line 22
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2190
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.4 
	IL_0001:  ret 
    } // end of method X2a::instance default int32 _N_GetVariantCode () 

  } // end of class X2a

  .class nested famorassem auto ansi sealed X3a
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X3a _N_constant_object

    // method line 23
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2194
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X3a::instance default void .ctor () 

    // method line 24
    .method public static  hidebysig  specialname 
           default class a.A/X3a _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x219c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3a a.A/X3a::_N_constant_object
	IL_0005:  ret 
    } // end of method X3a::default class a.A/X3a _N_constant_object_generator () 

    // method line 25
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x21a4
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3a::.ctor()
	IL_0005:  stsfld  class a.A/X3a a.A/X3a::_N_constant_object
	IL_000a:  ret 
    } // end of method X3a::default void .cctor () 

    // method line 26
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x21b0
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.5 
	IL_0001:  ret 
    } // end of method X3a::instance default int32 _N_GetVariantCode () 

  } // end of class X3a

  .class nested famorassem auto ansi sealed X1b
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X1b _N_constant_object

    // method line 27
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x21b4
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X1b::instance default void .ctor () 

    // method line 28
    .method public static  hidebysig  specialname 
           default class a.A/X1b _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x21bc
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1b a.A/X1b::_N_constant_object
	IL_0005:  ret 
    } // end of method X1b::default class a.A/X1b _N_constant_object_generator () 

    // method line 29
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x21c4
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X1b::.ctor()
	IL_0005:  stsfld  class a.A/X1b a.A/X1b::_N_constant_object
	IL_000a:  ret 
    } // end of method X1b::default void .cctor () 

    // method line 30
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x21d0
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.6 
	IL_0001:  ret 
    } // end of method X1b::instance default int32 _N_GetVariantCode () 

  } // end of class X1b

  .class nested famorassem auto ansi sealed X2b
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X2b _N_constant_object

    // method line 31
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x21d4
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X2b::instance default void .ctor () 

    // method line 32
    .method public static  hidebysig  specialname 
           default class a.A/X2b _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x21dc
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2b a.A/X2b::_N_constant_object
	IL_0005:  ret 
    } // end of method X2b::default class a.A/X2b _N_constant_object_generator () 

    // method line 33
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x21e4
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2b::.ctor()
	IL_0005:  stsfld  class a.A/X2b a.A/X2b::_N_constant_object
	IL_000a:  ret 
    } // end of method X2b::default void .cctor () 

    // method line 34
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x21f0
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.7 
	IL_0001:  ret 
    } // end of method X2b::instance default int32 _N_GetVariantCode () 

  } // end of class X2b

  .class nested famorassem auto ansi sealed X3b
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X3b _N_constant_object

    // method line 35
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x21f4
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X3b::instance default void .ctor () 

    // method line 36
    .method public static  hidebysig  specialname 
           default class a.A/X3b _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x21fc
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3b a.A/X3b::_N_constant_object
	IL_0005:  ret 
    } // end of method X3b::default class a.A/X3b _N_constant_object_generator () 

    // method line 37
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2204
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3b::.ctor()
	IL_0005:  stsfld  class a.A/X3b a.A/X3b::_N_constant_object
	IL_000a:  ret 
    } // end of method X3b::default void .cctor () 

    // method line 38
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2210
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.8 
	IL_0001:  ret 
    } // end of method X3b::instance default int32 _N_GetVariantCode () 

  } // end of class X3b

  .class nested famorassem auto ansi sealed X1c
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X1c _N_constant_object

    // method line 39
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2214
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X1c::instance default void .ctor () 

    // method line 40
    .method public static  hidebysig  specialname 
           default class a.A/X1c _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x221c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1c a.A/X1c::_N_constant_object
	IL_0005:  ret 
    } // end of method X1c::default class a.A/X1c _N_constant_object_generator () 

    // method line 41
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2224
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X1c::.ctor()
	IL_0005:  stsfld  class a.A/X1c a.A/X1c::_N_constant_object
	IL_000a:  ret 
    } // end of method X1c::default void .cctor () 

    // method line 42
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2230
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x09
	IL_0002:  ret 
    } // end of method X1c::instance default int32 _N_GetVariantCode () 

  } // end of class X1c

  .class nested famorassem auto ansi sealed X2c
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X2c _N_constant_object

    // method line 43
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2234
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X2c::instance default void .ctor () 

    // method line 44
    .method public static  hidebysig  specialname 
           default class a.A/X2c _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x223c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2c a.A/X2c::_N_constant_object
	IL_0005:  ret 
    } // end of method X2c::default class a.A/X2c _N_constant_object_generator () 

    // method line 45
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2244
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2c::.ctor()
	IL_0005:  stsfld  class a.A/X2c a.A/X2c::_N_constant_object
	IL_000a:  ret 
    } // end of method X2c::default void .cctor () 

    // method line 46
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2250
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0a
	IL_0002:  ret 
    } // end of method X2c::instance default int32 _N_GetVariantCode () 

  } // end of class X2c

  .class nested famorassem auto ansi sealed X3c
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X3c _N_constant_object

    // method line 47
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2254
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X3c::instance default void .ctor () 

    // method line 48
    .method public static  hidebysig  specialname 
           default class a.A/X3c _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x225c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3c a.A/X3c::_N_constant_object
	IL_0005:  ret 
    } // end of method X3c::default class a.A/X3c _N_constant_object_generator () 

    // method line 49
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2264
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3c::.ctor()
	IL_0005:  stsfld  class a.A/X3c a.A/X3c::_N_constant_object
	IL_000a:  ret 
    } // end of method X3c::default void .cctor () 

    // method line 50
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2270
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0b
	IL_0002:  ret 
    } // end of method X3c::instance default int32 _N_GetVariantCode () 

  } // end of class X3c

  .class nested famorassem auto ansi sealed X1ac
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X1ac _N_constant_object

    // method line 51
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2274
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X1ac::instance default void .ctor () 

    // method line 52
    .method public static  hidebysig  specialname 
           default class a.A/X1ac _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x227c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1ac a.A/X1ac::_N_constant_object
	IL_0005:  ret 
    } // end of method X1ac::default class a.A/X1ac _N_constant_object_generator () 

    // method line 53
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2284
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X1ac::.ctor()
	IL_0005:  stsfld  class a.A/X1ac a.A/X1ac::_N_constant_object
	IL_000a:  ret 
    } // end of method X1ac::default void .cctor () 

    // method line 54
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2290
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0c
	IL_0002:  ret 
    } // end of method X1ac::instance default int32 _N_GetVariantCode () 

  } // end of class X1ac

  .class nested famorassem auto ansi sealed X2ac
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X2ac _N_constant_object

    // method line 55
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2294
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X2ac::instance default void .ctor () 

    // method line 56
    .method public static  hidebysig  specialname 
           default class a.A/X2ac _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x229c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2ac a.A/X2ac::_N_constant_object
	IL_0005:  ret 
    } // end of method X2ac::default class a.A/X2ac _N_constant_object_generator () 

    // method line 57
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x22a4
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2ac::.ctor()
	IL_0005:  stsfld  class a.A/X2ac a.A/X2ac::_N_constant_object
	IL_000a:  ret 
    } // end of method X2ac::default void .cctor () 

    // method line 58
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x22b0
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0d
	IL_0002:  ret 
    } // end of method X2ac::instance default int32 _N_GetVariantCode () 

  } // end of class X2ac

  .class nested famorassem auto ansi sealed X3ac
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X3ac _N_constant_object

    // method line 59
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x22b4
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X3ac::instance default void .ctor () 

    // method line 60
    .method public static  hidebysig  specialname 
           default class a.A/X3ac _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x22bc
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3ac a.A/X3ac::_N_constant_object
	IL_0005:  ret 
    } // end of method X3ac::default class a.A/X3ac _N_constant_object_generator () 

    // method line 61
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x22c4
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3ac::.ctor()
	IL_0005:  stsfld  class a.A/X3ac a.A/X3ac::_N_constant_object
	IL_000a:  ret 
    } // end of method X3ac::default void .cctor () 

    // method line 62
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x22d0
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0e
	IL_0002:  ret 
    } // end of method X3ac::instance default int32 _N_GetVariantCode () 

  } // end of class X3ac

  .class nested famorassem auto ansi sealed X1bc
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X1bc _N_constant_object

    // method line 63
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x22d4
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X1bc::instance default void .ctor () 

    // method line 64
    .method public static  hidebysig  specialname 
           default class a.A/X1bc _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x22dc
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1bc a.A/X1bc::_N_constant_object
	IL_0005:  ret 
    } // end of method X1bc::default class a.A/X1bc _N_constant_object_generator () 

    // method line 65
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x22e4
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X1bc::.ctor()
	IL_0005:  stsfld  class a.A/X1bc a.A/X1bc::_N_constant_object
	IL_000a:  ret 
    } // end of method X1bc::default void .cctor () 

    // method line 66
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x22f0
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0f
	IL_0002:  ret 
    } // end of method X1bc::instance default int32 _N_GetVariantCode () 

  } // end of class X1bc

  .class nested famorassem auto ansi sealed X2bc
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X2bc _N_constant_object

    // method line 67
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x22f4
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X2bc::instance default void .ctor () 

    // method line 68
    .method public static  hidebysig  specialname 
           default class a.A/X2bc _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x22fc
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2bc a.A/X2bc::_N_constant_object
	IL_0005:  ret 
    } // end of method X2bc::default class a.A/X2bc _N_constant_object_generator () 

    // method line 69
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2304
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2bc::.ctor()
	IL_0005:  stsfld  class a.A/X2bc a.A/X2bc::_N_constant_object
	IL_000a:  ret 
    } // end of method X2bc::default void .cctor () 

    // method line 70
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2310
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x10
	IL_0002:  ret 
    } // end of method X2bc::instance default int32 _N_GetVariantCode () 

  } // end of class X2bc

  .class nested famorassem auto ansi sealed X3bc
  	extends a.A
  {
    .custom instance void class [Nemerle.stage1]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle.stage1]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 27 2E 74 69 28 2E 74 70 28 29 28 61 2E 41   // ..'.ti(.tp()(a.A
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		4F 62 6A 65 63 74 28 29 29 29 00 00             ) // Object()))..

    .field  public static   class a.A/X3bc _N_constant_object

    // method line 71
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2314
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X3bc::instance default void .ctor () 

    // method line 72
    .method public static  hidebysig  specialname 
           default class a.A/X3bc _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x231c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3bc a.A/X3bc::_N_constant_object
	IL_0005:  ret 
    } // end of method X3bc::default class a.A/X3bc _N_constant_object_generator () 

    // method line 73
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2324
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3bc::.ctor()
	IL_0005:  stsfld  class a.A/X3bc a.A/X3bc::_N_constant_object
	IL_000a:  ret 
    } // end of method X3bc::default void .cctor () 

    // method line 74
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2330
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x11
	IL_0002:  ret 
    } // end of method X3bc::instance default int32 _N_GetVariantCode () 

  } // end of class X3bc

  } // end of class a.A
}

.namespace a
{
  .class private auto ansi abstract Test
  	extends [mscorlib]System.Object
  {

    // method line 75
    .method private static  hidebysig 
           default void test_it (int32 max)  cil managed 
    {
        // Method begins at RVA 0x2334
	// Code size 766 (0x2fe)
	.maxstack 26
	.locals init (
		class a.A[]	V_0,
		int32	V_1,
		int32	V_2,
		bool	V_3,
		class a.A	V_4,
		class a.A	V_5)
	IL_0000:  ldc.i4.s 0x12
	IL_0002:  newarr a.A
	IL_0007:  dup 
	IL_0008:  ldc.i4 0
	IL_000d:  ldsfld  class a.A/X1 a.A/X1::_N_constant_object
	IL_0012:  stelem.ref 
	IL_0013:  dup 
	IL_0014:  ldc.i4 1
	IL_0019:  ldsfld  class a.A/X2 a.A/X2::_N_constant_object
	IL_001e:  stelem.ref 
	IL_001f:  dup 
	IL_0020:  ldc.i4 2
	IL_0025:  ldsfld  class a.A/X3 a.A/X3::_N_constant_object
	IL_002a:  stelem.ref 
	IL_002b:  dup 
	IL_002c:  ldc.i4 3
	IL_0031:  ldsfld  class a.A/X1a a.A/X1a::_N_constant_object
	IL_0036:  stelem.ref 
	IL_0037:  dup 
	IL_0038:  ldc.i4 4
	IL_003d:  ldsfld  class a.A/X2a a.A/X2a::_N_constant_object
	IL_0042:  stelem.ref 
	IL_0043:  dup 
	IL_0044:  ldc.i4 5
	IL_0049:  ldsfld  class a.A/X3a a.A/X3a::_N_constant_object
	IL_004e:  stelem.ref 
	IL_004f:  dup 
	IL_0050:  ldc.i4 6
	IL_0055:  ldsfld  class a.A/X1b a.A/X1b::_N_constant_object
	IL_005a:  stelem.ref 
	IL_005b:  dup 
	IL_005c:  ldc.i4 7
	IL_0061:  ldsfld  class a.A/X2b a.A/X2b::_N_constant_object
	IL_0066:  stelem.ref 
	IL_0067:  dup 
	IL_0068:  ldc.i4 8
	IL_006d:  ldsfld  class a.A/X3b a.A/X3b::_N_constant_object
	IL_0072:  stelem.ref 
	IL_0073:  dup 
	IL_0074:  ldc.i4 9
	IL_0079:  ldsfld  class a.A/X1c a.A/X1c::_N_constant_object
	IL_007e:  stelem.ref 
	IL_007f:  dup 
	IL_0080:  ldc.i4 10
	IL_0085:  ldsfld  class a.A/X2c a.A/X2c::_N_constant_object
	IL_008a:  stelem.ref 
	IL_008b:  dup 
	IL_008c:  ldc.i4 11
	IL_0091:  ldsfld  class a.A/X3c a.A/X3c::_N_constant_object
	IL_0096:  stelem.ref 
	IL_0097:  dup 
	IL_0098:  ldc.i4 12
	IL_009d:  ldsfld  class a.A/X1ac a.A/X1ac::_N_constant_object
	IL_00a2:  stelem.ref 
	IL_00a3:  dup 
	IL_00a4:  ldc.i4 13
	IL_00a9:  ldsfld  class a.A/X2ac a.A/X2ac::_N_constant_object
	IL_00ae:  stelem.ref 
	IL_00af:  dup 
	IL_00b0:  ldc.i4 14
	IL_00b5:  ldsfld  class a.A/X3ac a.A/X3ac::_N_constant_object
	IL_00ba:  stelem.ref 
	IL_00bb:  dup 
	IL_00bc:  ldc.i4 15
	IL_00c1:  ldsfld  class a.A/X1bc a.A/X1bc::_N_constant_object
	IL_00c6:  stelem.ref 
	IL_00c7:  dup 
	IL_00c8:  ldc.i4 16
	IL_00cd:  ldsfld  class a.A/X2bc a.A/X2bc::_N_constant_object
	IL_00d2:  stelem.ref 
	IL_00d3:  dup 
	IL_00d4:  ldc.i4 17
	IL_00d9:  ldsfld  class a.A/X3bc a.A/X3bc::_N_constant_object
	IL_00de:  stelem.ref 
	IL_00df:  stloc.0 
	IL_00e0:  ldc.i4.0 
	IL_00e1:  stloc.1 
	IL_00e2:  ldc.i4.0 
	IL_00e3:  stloc.2 
	IL_00e4:  ldloc.2 
	IL_00e5:  ldarg.0 
	IL_00e6:  blt.s IL_00ee

	IL_00e8:  ldc.i4.0 
	IL_00e9:  br IL_00ef

	IL_00ee:  ldc.i4.1 
	IL_00ef:  stloc.3 
	IL_00f0:  ldloc.3 
	IL_00f1:  brfalse IL_02f7

	IL_00f6:  ldloc.0 
	IL_00f7:  ldloc.2 
	IL_00f8:  ldloc.0 
	IL_00f9:  callvirt instance int32 class [mscorlib]System.Array::get_Length()
	IL_00fe:  rem 
	IL_00ff:  ldelem.ref 
	IL_0100:  stloc.s 4
	IL_0102:  ldloc.s 4
	IL_0104:  stloc.s 5
	IL_0106:  ldloc.s 5
	IL_0108:  brtrue.s IL_0110

	IL_010a:  newobj instance void class [Nemerle.stage1]Nemerle.Core.NullMatchException::.ctor()
	IL_010f:  throw 
	IL_0110:  ldloc.s 5
	IL_0112:  callvirt instance int32 class a.A::_N_GetVariantCode()
	IL_0117:  switch (
	  IL_016a,
	  IL_0170,
	  IL_0176,
	  IL_017c,
	  IL_0182,
	  IL_0188,
	  IL_018e,
	  IL_0194,
	  IL_019a,
	  IL_01a0,
	  IL_01a7,
	  IL_01ae,
	  IL_01b5,
	  IL_01bc,
	  IL_01c3,
	  IL_01ca,
	  IL_01d1,
	  IL_01d8)
	IL_0165:  br IL_022c

	IL_016a: br	  IL_0232 
	IL_0170: br	  IL_023b 
	IL_0176: br	  IL_0245 
	IL_017c: br	  IL_024f 
	IL_0182: br	  IL_0259 
	IL_0188: br	  IL_0263 
	IL_018e: br	  IL_026d 
	IL_0194: br	  IL_0277 
	IL_019a: br	  IL_0281 
	IL_01a0: br	  IL_028b 
	IL_01a7: br	  IL_0295 
	IL_01ae: br	  IL_029f 
	IL_01b5: br	  IL_02a9 
	IL_01bc: br	  IL_02b6 
	IL_01c3: br	  IL_02c0 
	IL_01ca: br	  IL_02ca 
	IL_01d1: br	  IL_02d4 
	IL_01d8: br	  IL_02e1 

	IL_022c:  newobj instance void class [Nemerle.stage1]Nemerle.Core.MatchFailureException::.ctor()
	IL_0231:  throw 
	IL_0232:  ldloc.1 
	IL_0233:  ldc.i4.1 
	IL_0234:  add.ovf 
	IL_0235:  stloc.1 
	IL_0236:  br IL_02ee

	IL_023b:  ldloc.1 
	IL_023c:  ldc.i4.s 0x0c
	IL_023e:  add.ovf 
	IL_023f:  stloc.1 
	IL_0240:  br IL_02ee

	IL_0245:  ldloc.1 
	IL_0246:  ldc.i4.s 0x0d
	IL_0248:  add.ovf 
	IL_0249:  stloc.1 
	IL_024a:  br IL_02ee

	IL_024f:  ldloc.1 
	IL_0250:  ldc.i4.s 0x0e
	IL_0252:  add.ovf 
	IL_0253:  stloc.1 
	IL_0254:  br IL_02ee

	IL_0259:  ldloc.1 
	IL_025a:  ldc.i4.s 0x0f
	IL_025c:  add.ovf 
	IL_025d:  stloc.1 
	IL_025e:  br IL_02ee

	IL_0263:  ldloc.1 
	IL_0264:  ldc.i4.s 0x10
	IL_0266:  add.ovf 
	IL_0267:  stloc.1 
	IL_0268:  br IL_02ee

	IL_026d:  ldloc.1 
	IL_026e:  ldc.i4.s 0x11
	IL_0270:  add.ovf 
	IL_0271:  stloc.1 
	IL_0272:  br IL_02ee

	IL_0277:  ldloc.1 
	IL_0278:  ldc.i4.s 0x12
	IL_027a:  add.ovf 
	IL_027b:  stloc.1 
	IL_027c:  br IL_02ee

	IL_0281:  ldloc.1 
	IL_0282:  ldc.i4.s 0x13
	IL_0284:  add.ovf 
	IL_0285:  stloc.1 
	IL_0286:  br IL_02ee

	IL_028b:  ldloc.1 
	IL_028c:  ldc.i4.s 0x0a
	IL_028e:  add.ovf 
	IL_028f:  stloc.1 
	IL_0290:  br IL_02ee

	IL_0295:  ldloc.1 
	IL_0296:  ldc.i4.s 0x0e
	IL_0298:  add.ovf 
	IL_0299:  stloc.1 
	IL_029a:  br IL_02ee

	IL_029f:  ldloc.1 
	IL_02a0:  ldc.i4.s 0x0d
	IL_02a2:  add.ovf 
	IL_02a3:  stloc.1 
	IL_02a4:  br IL_02ee

	IL_02a9:  ldloc.1 
	IL_02aa:  ldc.i4 133
	IL_02af:  add.ovf 
	IL_02b0:  stloc.1 
	IL_02b1:  br IL_02ee

	IL_02b6:  ldloc.1 
	IL_02b7:  ldc.i4.s 0x0c
	IL_02b9:  add.ovf 
	IL_02ba:  stloc.1 
	IL_02bb:  br IL_02ee

	IL_02c0:  ldloc.1 
	IL_02c1:  ldc.i4.s 0x7b
	IL_02c3:  add.ovf 
	IL_02c4:  stloc.1 
	IL_02c5:  br IL_02ee

	IL_02ca:  ldloc.1 
	IL_02cb:  ldc.i4.s 0x7c
	IL_02cd:  add.ovf 
	IL_02ce:  stloc.1 
	IL_02cf:  br IL_02ee

	IL_02d4:  ldloc.1 
	IL_02d5:  ldc.i4 156
	IL_02da:  add.ovf 
	IL_02db:  stloc.1 
	IL_02dc:  br IL_02ee

	IL_02e1:  ldloc.1 
	IL_02e2:  ldc.i4 153
	IL_02e7:  add.ovf 
	IL_02e8:  stloc.1 
	IL_02e9:  br IL_02ee

	IL_02ee:  ldloc.2 
	IL_02ef:  ldc.i4.1 
	IL_02f0:  add.ovf 
	IL_02f1:  stloc.2 
	IL_02f2:  br IL_00e4

	IL_02f7:  ldloc.1 
	IL_02f8:  call void class [mscorlib]System.Console::WriteLine(int32)
	IL_02fd:  ret 
    } // end of method Test::default void test_it (int32 max) 

    // method line 76
    .method private static  hidebysig 
           default void Main ()  cil managed 
    {
        // Method begins at RVA 0x2640
	.entrypoint
	// Code size 53 (0x35)
	.maxstack 9
	.locals init (
		valuetype [mscorlib]System.DateTime	V_0,
		valuetype [mscorlib]System.DateTime	V_1,
		valuetype [mscorlib]System.TimeSpan	V_2)
	IL_0000:  call valuetype [mscorlib]System.DateTime valuetype [mscorlib]System.DateTime::get_Now()
	IL_0005:  stloc.0 
	IL_0006:  ldc.i4 10000000
	IL_000b:  call void class a.Test::test_it(int32)
	IL_0010:  call valuetype [mscorlib]System.DateTime valuetype [mscorlib]System.DateTime::get_Now()
	IL_0015:  stloc.1 
	IL_0016:  ldstr "benchmarks/match-switch.n:62:10: : execution took "
	IL_001b:  ldloc.1 
	IL_001c:  ldloc.0 
	IL_001d:  call valuetype [mscorlib]System.TimeSpan valuetype [mscorlib]System.DateTime::op_Subtraction(valuetype [mscorlib]System.DateTime, valuetype [mscorlib]System.DateTime)
	IL_0022:  stloc.2 
	IL_0023:  ldloca.s 2
	IL_0025:  call instance string valuetype [mscorlib]System.TimeSpan::ToString()
	IL_002a:  call string string::Concat(string, string)
	IL_002f:  call void class [mscorlib]System.Console::WriteLine(string)
	IL_0034:  ret 
    } // end of method Test::default void Main () 

  } // end of class a.Test
}

