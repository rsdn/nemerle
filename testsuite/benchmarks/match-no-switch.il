.assembly extern mscorlib
{
  .ver 1:0:5000:0
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 ) // .z\V.4..
}
.assembly extern Nemerle
{
  .ver 0:3:1:37267
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
.module 'out' // GUID = {2F806936-B496-4771-83B9-FC0030F90407}


.namespace a
{
  .class private auto ansi abstract A
  	extends [mscorlib]System.Object
  {
    .custom instance void class [Nemerle]Nemerle.Internal.VariantAttribute::.ctor(string) =  (
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

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
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
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

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
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2110
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1 a.A/X1::_N_constant_object
	IL_0005:  ret 
    } // end of method X1::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 7
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2118
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.0 
	IL_0001:  ret 
    } // end of method X1::instance default int32 _N_GetVariantCode () 

  } // end of class X1

  .class nested famorassem auto ansi sealed X2
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X2 _N_constant_object

    // method line 8
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x211c
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X2::instance default void .ctor () 

    // method line 9
    .method public static  hidebysig  specialname 
           default class a.A/X2 _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x2124
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2 a.A/X2::_N_constant_object
	IL_0005:  ret 
    } // end of method X2::default class a.A/X2 _N_constant_object_generator () 

    // method line 10
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x212c
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2::.ctor()
	IL_0005:  stsfld  class a.A/X2 a.A/X2::_N_constant_object
	IL_000a:  ret 
    } // end of method X2::default void .cctor () 

    // method line 11
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2138
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2 a.A/X2::_N_constant_object
	IL_0005:  ret 
    } // end of method X2::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 12
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2140
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.1 
	IL_0001:  ret 
    } // end of method X2::instance default int32 _N_GetVariantCode () 

  } // end of class X2

  .class nested famorassem auto ansi sealed X3
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X3 _N_constant_object

    // method line 13
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2144
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X3::instance default void .ctor () 

    // method line 14
    .method public static  hidebysig  specialname 
           default class a.A/X3 _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x214c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3 a.A/X3::_N_constant_object
	IL_0005:  ret 
    } // end of method X3::default class a.A/X3 _N_constant_object_generator () 

    // method line 15
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2154
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3::.ctor()
	IL_0005:  stsfld  class a.A/X3 a.A/X3::_N_constant_object
	IL_000a:  ret 
    } // end of method X3::default void .cctor () 

    // method line 16
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2160
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3 a.A/X3::_N_constant_object
	IL_0005:  ret 
    } // end of method X3::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 17
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2168
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.2 
	IL_0001:  ret 
    } // end of method X3::instance default int32 _N_GetVariantCode () 

  } // end of class X3

  .class nested famorassem auto ansi sealed X1a
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X1a _N_constant_object

    // method line 18
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x216c
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X1a::instance default void .ctor () 

    // method line 19
    .method public static  hidebysig  specialname 
           default class a.A/X1a _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x2174
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1a a.A/X1a::_N_constant_object
	IL_0005:  ret 
    } // end of method X1a::default class a.A/X1a _N_constant_object_generator () 

    // method line 20
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x217c
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X1a::.ctor()
	IL_0005:  stsfld  class a.A/X1a a.A/X1a::_N_constant_object
	IL_000a:  ret 
    } // end of method X1a::default void .cctor () 

    // method line 21
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2188
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1a a.A/X1a::_N_constant_object
	IL_0005:  ret 
    } // end of method X1a::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 22
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2190
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.3 
	IL_0001:  ret 
    } // end of method X1a::instance default int32 _N_GetVariantCode () 

  } // end of class X1a

  .class nested famorassem auto ansi sealed X2a
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X2a _N_constant_object

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
    } // end of method X2a::instance default void .ctor () 

    // method line 24
    .method public static  hidebysig  specialname 
           default class a.A/X2a _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x219c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2a a.A/X2a::_N_constant_object
	IL_0005:  ret 
    } // end of method X2a::default class a.A/X2a _N_constant_object_generator () 

    // method line 25
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x21a4
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2a::.ctor()
	IL_0005:  stsfld  class a.A/X2a a.A/X2a::_N_constant_object
	IL_000a:  ret 
    } // end of method X2a::default void .cctor () 

    // method line 26
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x21b0
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2a a.A/X2a::_N_constant_object
	IL_0005:  ret 
    } // end of method X2a::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 27
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x21b8
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.4 
	IL_0001:  ret 
    } // end of method X2a::instance default int32 _N_GetVariantCode () 

  } // end of class X2a

  .class nested famorassem auto ansi sealed X3a
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X3a _N_constant_object

    // method line 28
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x21bc
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X3a::instance default void .ctor () 

    // method line 29
    .method public static  hidebysig  specialname 
           default class a.A/X3a _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x21c4
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3a a.A/X3a::_N_constant_object
	IL_0005:  ret 
    } // end of method X3a::default class a.A/X3a _N_constant_object_generator () 

    // method line 30
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x21cc
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3a::.ctor()
	IL_0005:  stsfld  class a.A/X3a a.A/X3a::_N_constant_object
	IL_000a:  ret 
    } // end of method X3a::default void .cctor () 

    // method line 31
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x21d8
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3a a.A/X3a::_N_constant_object
	IL_0005:  ret 
    } // end of method X3a::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 32
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x21e0
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.5 
	IL_0001:  ret 
    } // end of method X3a::instance default int32 _N_GetVariantCode () 

  } // end of class X3a

  .class nested famorassem auto ansi sealed X1b
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X1b _N_constant_object

    // method line 33
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x21e4
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X1b::instance default void .ctor () 

    // method line 34
    .method public static  hidebysig  specialname 
           default class a.A/X1b _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x21ec
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1b a.A/X1b::_N_constant_object
	IL_0005:  ret 
    } // end of method X1b::default class a.A/X1b _N_constant_object_generator () 

    // method line 35
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x21f4
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X1b::.ctor()
	IL_0005:  stsfld  class a.A/X1b a.A/X1b::_N_constant_object
	IL_000a:  ret 
    } // end of method X1b::default void .cctor () 

    // method line 36
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2200
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1b a.A/X1b::_N_constant_object
	IL_0005:  ret 
    } // end of method X1b::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 37
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2208
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.6 
	IL_0001:  ret 
    } // end of method X1b::instance default int32 _N_GetVariantCode () 

  } // end of class X1b

  .class nested famorassem auto ansi sealed X2b
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X2b _N_constant_object

    // method line 38
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x220c
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X2b::instance default void .ctor () 

    // method line 39
    .method public static  hidebysig  specialname 
           default class a.A/X2b _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x2214
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2b a.A/X2b::_N_constant_object
	IL_0005:  ret 
    } // end of method X2b::default class a.A/X2b _N_constant_object_generator () 

    // method line 40
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x221c
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2b::.ctor()
	IL_0005:  stsfld  class a.A/X2b a.A/X2b::_N_constant_object
	IL_000a:  ret 
    } // end of method X2b::default void .cctor () 

    // method line 41
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2228
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2b a.A/X2b::_N_constant_object
	IL_0005:  ret 
    } // end of method X2b::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 42
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2230
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.7 
	IL_0001:  ret 
    } // end of method X2b::instance default int32 _N_GetVariantCode () 

  } // end of class X2b

  .class nested famorassem auto ansi sealed X3b
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X3b _N_constant_object

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
    } // end of method X3b::instance default void .ctor () 

    // method line 44
    .method public static  hidebysig  specialname 
           default class a.A/X3b _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x223c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3b a.A/X3b::_N_constant_object
	IL_0005:  ret 
    } // end of method X3b::default class a.A/X3b _N_constant_object_generator () 

    // method line 45
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2244
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3b::.ctor()
	IL_0005:  stsfld  class a.A/X3b a.A/X3b::_N_constant_object
	IL_000a:  ret 
    } // end of method X3b::default void .cctor () 

    // method line 46
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2250
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3b a.A/X3b::_N_constant_object
	IL_0005:  ret 
    } // end of method X3b::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 47
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2258
	// Code size 2 (0x2)
	.maxstack 8
	IL_0000:  ldc.i4.8 
	IL_0001:  ret 
    } // end of method X3b::instance default int32 _N_GetVariantCode () 

  } // end of class X3b

  .class nested famorassem auto ansi sealed X1c
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X1c _N_constant_object

    // method line 48
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x225c
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X1c::instance default void .ctor () 

    // method line 49
    .method public static  hidebysig  specialname 
           default class a.A/X1c _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x2264
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1c a.A/X1c::_N_constant_object
	IL_0005:  ret 
    } // end of method X1c::default class a.A/X1c _N_constant_object_generator () 

    // method line 50
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x226c
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X1c::.ctor()
	IL_0005:  stsfld  class a.A/X1c a.A/X1c::_N_constant_object
	IL_000a:  ret 
    } // end of method X1c::default void .cctor () 

    // method line 51
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2278
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1c a.A/X1c::_N_constant_object
	IL_0005:  ret 
    } // end of method X1c::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 52
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2280
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x09
	IL_0002:  ret 
    } // end of method X1c::instance default int32 _N_GetVariantCode () 

  } // end of class X1c

  .class nested famorassem auto ansi sealed X2c
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X2c _N_constant_object

    // method line 53
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2284
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X2c::instance default void .ctor () 

    // method line 54
    .method public static  hidebysig  specialname 
           default class a.A/X2c _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x228c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2c a.A/X2c::_N_constant_object
	IL_0005:  ret 
    } // end of method X2c::default class a.A/X2c _N_constant_object_generator () 

    // method line 55
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2294
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2c::.ctor()
	IL_0005:  stsfld  class a.A/X2c a.A/X2c::_N_constant_object
	IL_000a:  ret 
    } // end of method X2c::default void .cctor () 

    // method line 56
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x22a0
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2c a.A/X2c::_N_constant_object
	IL_0005:  ret 
    } // end of method X2c::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 57
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x22a8
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0a
	IL_0002:  ret 
    } // end of method X2c::instance default int32 _N_GetVariantCode () 

  } // end of class X2c

  .class nested famorassem auto ansi sealed X3c
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X3c _N_constant_object

    // method line 58
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x22ac
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X3c::instance default void .ctor () 

    // method line 59
    .method public static  hidebysig  specialname 
           default class a.A/X3c _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x22b4
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3c a.A/X3c::_N_constant_object
	IL_0005:  ret 
    } // end of method X3c::default class a.A/X3c _N_constant_object_generator () 

    // method line 60
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x22bc
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3c::.ctor()
	IL_0005:  stsfld  class a.A/X3c a.A/X3c::_N_constant_object
	IL_000a:  ret 
    } // end of method X3c::default void .cctor () 

    // method line 61
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x22c8
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3c a.A/X3c::_N_constant_object
	IL_0005:  ret 
    } // end of method X3c::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 62
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x22d0
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0b
	IL_0002:  ret 
    } // end of method X3c::instance default int32 _N_GetVariantCode () 

  } // end of class X3c

  .class nested famorassem auto ansi sealed X1ac
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X1ac _N_constant_object

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
    } // end of method X1ac::instance default void .ctor () 

    // method line 64
    .method public static  hidebysig  specialname 
           default class a.A/X1ac _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x22dc
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1ac a.A/X1ac::_N_constant_object
	IL_0005:  ret 
    } // end of method X1ac::default class a.A/X1ac _N_constant_object_generator () 

    // method line 65
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x22e4
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X1ac::.ctor()
	IL_0005:  stsfld  class a.A/X1ac a.A/X1ac::_N_constant_object
	IL_000a:  ret 
    } // end of method X1ac::default void .cctor () 

    // method line 66
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x22f0
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1ac a.A/X1ac::_N_constant_object
	IL_0005:  ret 
    } // end of method X1ac::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 67
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x22f8
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0c
	IL_0002:  ret 
    } // end of method X1ac::instance default int32 _N_GetVariantCode () 

  } // end of class X1ac

  .class nested famorassem auto ansi sealed X2ac
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X2ac _N_constant_object

    // method line 68
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x22fc
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X2ac::instance default void .ctor () 

    // method line 69
    .method public static  hidebysig  specialname 
           default class a.A/X2ac _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x2304
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2ac a.A/X2ac::_N_constant_object
	IL_0005:  ret 
    } // end of method X2ac::default class a.A/X2ac _N_constant_object_generator () 

    // method line 70
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x230c
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2ac::.ctor()
	IL_0005:  stsfld  class a.A/X2ac a.A/X2ac::_N_constant_object
	IL_000a:  ret 
    } // end of method X2ac::default void .cctor () 

    // method line 71
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2318
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2ac a.A/X2ac::_N_constant_object
	IL_0005:  ret 
    } // end of method X2ac::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 72
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2320
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0d
	IL_0002:  ret 
    } // end of method X2ac::instance default int32 _N_GetVariantCode () 

  } // end of class X2ac

  .class nested famorassem auto ansi sealed X3ac
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X3ac _N_constant_object

    // method line 73
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2324
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X3ac::instance default void .ctor () 

    // method line 74
    .method public static  hidebysig  specialname 
           default class a.A/X3ac _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x232c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3ac a.A/X3ac::_N_constant_object
	IL_0005:  ret 
    } // end of method X3ac::default class a.A/X3ac _N_constant_object_generator () 

    // method line 75
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2334
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3ac::.ctor()
	IL_0005:  stsfld  class a.A/X3ac a.A/X3ac::_N_constant_object
	IL_000a:  ret 
    } // end of method X3ac::default void .cctor () 

    // method line 76
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2340
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3ac a.A/X3ac::_N_constant_object
	IL_0005:  ret 
    } // end of method X3ac::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 77
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2348
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0e
	IL_0002:  ret 
    } // end of method X3ac::instance default int32 _N_GetVariantCode () 

  } // end of class X3ac

  .class nested famorassem auto ansi sealed X1bc
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X1bc _N_constant_object

    // method line 78
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x234c
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X1bc::instance default void .ctor () 

    // method line 79
    .method public static  hidebysig  specialname 
           default class a.A/X1bc _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x2354
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1bc a.A/X1bc::_N_constant_object
	IL_0005:  ret 
    } // end of method X1bc::default class a.A/X1bc _N_constant_object_generator () 

    // method line 80
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x235c
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X1bc::.ctor()
	IL_0005:  stsfld  class a.A/X1bc a.A/X1bc::_N_constant_object
	IL_000a:  ret 
    } // end of method X1bc::default void .cctor () 

    // method line 81
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2368
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X1bc a.A/X1bc::_N_constant_object
	IL_0005:  ret 
    } // end of method X1bc::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 82
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2370
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x0f
	IL_0002:  ret 
    } // end of method X1bc::instance default int32 _N_GetVariantCode () 

  } // end of class X1bc

  .class nested famorassem auto ansi sealed X2bc
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X2bc _N_constant_object

    // method line 83
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x2374
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X2bc::instance default void .ctor () 

    // method line 84
    .method public static  hidebysig  specialname 
           default class a.A/X2bc _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x237c
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2bc a.A/X2bc::_N_constant_object
	IL_0005:  ret 
    } // end of method X2bc::default class a.A/X2bc _N_constant_object_generator () 

    // method line 85
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x2384
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X2bc::.ctor()
	IL_0005:  stsfld  class a.A/X2bc a.A/X2bc::_N_constant_object
	IL_000a:  ret 
    } // end of method X2bc::default void .cctor () 

    // method line 86
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x2390
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X2bc a.A/X2bc::_N_constant_object
	IL_0005:  ret 
    } // end of method X2bc::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 87
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x2398
	// Code size 3 (0x3)
	.maxstack 8
	IL_0000:  ldc.i4.s 0x10
	IL_0002:  ret 
    } // end of method X2bc::instance default int32 _N_GetVariantCode () 

  } // end of class X2bc

  .class nested famorassem auto ansi sealed X3bc
  	extends a.A
  	implements [mscorlib]System.Runtime.Serialization.IObjectReference  {
    .custom instance void class [Nemerle]Nemerle.Internal.ConstantVariantOptionAttribute::.ctor() =  (01 00 00 00 ) // ....

    .custom instance void class [Nemerle]Nemerle.Internal.TypeAttribute::.ctor(string) =  (
		01 00 80 85 2E 74 69 28 2E 74 70 28 29 28 61 2E   // .....ti(.tp()(a.
		41 28 29 53 79 73 74 65 6D 2E 52 75 6E 74 69 6D   // A()System.Runtim
		65 2E 53 65 72 69 61 6C 69 7A 61 74 69 6F 6E 2E   // e.Serialization.
		49 4F 62 6A 65 63 74 52 65 66 65 72 65 6E 63 65   // IObjectReference
		28 29 29 28 61 2E 41 28 29 53 79 73 74 65 6D 2E   // ())(a.A()System.
		52 75 6E 74 69 6D 65 2E 53 65 72 69 61 6C 69 7A   // Runtime.Serializ
		61 74 69 6F 6E 2E 49 4F 62 6A 65 63 74 52 65 66   // ation.IObjectRef
		65 72 65 6E 63 65 28 29 53 79 73 74 65 6D 2E 4F   // erence()System.O
		62 6A 65 63 74 28 29 29 29 00 00                ) // bject()))..

    .field  public static   class a.A/X3bc _N_constant_object

    // method line 88
    .method private hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x239c
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class a.A::.ctor()
	IL_0006:  ret 
    } // end of method X3bc::instance default void .ctor () 

    // method line 89
    .method public static  hidebysig  specialname 
           default class a.A/X3bc _N_constant_object_generator ()  cil managed 
    {
        // Method begins at RVA 0x23a4
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3bc a.A/X3bc::_N_constant_object
	IL_0005:  ret 
    } // end of method X3bc::default class a.A/X3bc _N_constant_object_generator () 

    // method line 90
    .method private static  hidebysig  specialname  rtspecialname 
           default void .cctor ()  cil managed 
    {
        // Method begins at RVA 0x23ac
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  newobj instance void class a.A/X3bc::.ctor()
	IL_0005:  stsfld  class a.A/X3bc a.A/X3bc::_N_constant_object
	IL_000a:  ret 
    } // end of method X3bc::default void .cctor () 

    // method line 91
    .method public final  virtual  hidebysig  newslot  specialname 
           instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515)  cil managed 
    {
        // Method begins at RVA 0x23b8
	.override class [mscorlib]System.Runtime.Serialization.IObjectReference::GetRealObject
	// Code size 6 (0x6)
	.maxstack 8
	IL_0000:  ldsfld  class a.A/X3bc a.A/X3bc::_N_constant_object
	IL_0005:  ret 
    } // end of method X3bc::instance default object GetRealObject (valuetype [mscorlib]System.Runtime.Serialization.StreamingContext _N_u515) 

    // method line 92
    .method public virtual  hidebysig  specialname 
           instance default int32 _N_GetVariantCode ()  cil managed 
    {
        // Method begins at RVA 0x23c0
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

    // method line 93
    .method private static  hidebysig 
           default void test_it (int32 max)  cil managed 
    {
        // Method begins at RVA 0x23c4
	// Code size 656 (0x290)
	.maxstack 8
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
	IL_00f1:  brfalse IL_0289

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

	IL_010a:  newobj instance void class [Nemerle]Nemerle.Core.NullMatchException::.ctor()
	IL_010f:  throw 
	IL_0110:  ldloc.s 5
	IL_0112:  callvirt instance int32 class a.A::_N_GetVariantCode()
	IL_0117:  switch (
	  IL_016a,
	  IL_016f,
	  IL_0174,
	  IL_0179,
	  IL_017e,
	  IL_0183,
	  IL_0188,
	  IL_018d,
	  IL_0192,
	  IL_0197,
	  IL_019c,
	  IL_01a1,
	  IL_01a6,
	  IL_01ab,
	  IL_01b0,
	  IL_01b5,
	  IL_01ba,
	  IL_01bf)
	IL_0164:  newobj instance void class [Nemerle]Nemerle.Core.MatchFailureException::.ctor()
	IL_0169:  throw 
	IL_016a:  br IL_01c4

	IL_016f:  br IL_01cd

	IL_0174:  br IL_01d7

	IL_0179:  br IL_01e1

	IL_017e:  br IL_01eb

	IL_0183:  br IL_01f5

	IL_0188:  br IL_01ff

	IL_018d:  br IL_0209

	IL_0192:  br IL_0213

	IL_0197:  br IL_021d

	IL_019c:  br IL_0227

	IL_01a1:  br IL_0231

	IL_01a6:  br IL_023b

	IL_01ab:  br IL_0248

	IL_01b0:  br IL_0252

	IL_01b5:  br IL_025c

	IL_01ba:  br IL_0266

	IL_01bf:  br IL_0273

	IL_01c4:  ldloc.1 
	IL_01c5:  ldc.i4.1 
	IL_01c6:  add.ovf 
	IL_01c7:  stloc.1 
	IL_01c8:  br IL_0280

	IL_01cd:  ldloc.1 
	IL_01ce:  ldc.i4.s 0x0c
	IL_01d0:  add.ovf 
	IL_01d1:  stloc.1 
	IL_01d2:  br IL_0280

	IL_01d7:  ldloc.1 
	IL_01d8:  ldc.i4.s 0x0d
	IL_01da:  add.ovf 
	IL_01db:  stloc.1 
	IL_01dc:  br IL_0280

	IL_01e1:  ldloc.1 
	IL_01e2:  ldc.i4.s 0x0e
	IL_01e4:  add.ovf 
	IL_01e5:  stloc.1 
	IL_01e6:  br IL_0280

	IL_01eb:  ldloc.1 
	IL_01ec:  ldc.i4.s 0x0f
	IL_01ee:  add.ovf 
	IL_01ef:  stloc.1 
	IL_01f0:  br IL_0280

	IL_01f5:  ldloc.1 
	IL_01f6:  ldc.i4.s 0x10
	IL_01f8:  add.ovf 
	IL_01f9:  stloc.1 
	IL_01fa:  br IL_0280

	IL_01ff:  ldloc.1 
	IL_0200:  ldc.i4.s 0x11
	IL_0202:  add.ovf 
	IL_0203:  stloc.1 
	IL_0204:  br IL_0280

	IL_0209:  ldloc.1 
	IL_020a:  ldc.i4.s 0x12
	IL_020c:  add.ovf 
	IL_020d:  stloc.1 
	IL_020e:  br IL_0280

	IL_0213:  ldloc.1 
	IL_0214:  ldc.i4.s 0x13
	IL_0216:  add.ovf 
	IL_0217:  stloc.1 
	IL_0218:  br IL_0280

	IL_021d:  ldloc.1 
	IL_021e:  ldc.i4.s 0x0a
	IL_0220:  add.ovf 
	IL_0221:  stloc.1 
	IL_0222:  br IL_0280

	IL_0227:  ldloc.1 
	IL_0228:  ldc.i4.s 0x0e
	IL_022a:  add.ovf 
	IL_022b:  stloc.1 
	IL_022c:  br IL_0280

	IL_0231:  ldloc.1 
	IL_0232:  ldc.i4.s 0x0d
	IL_0234:  add.ovf 
	IL_0235:  stloc.1 
	IL_0236:  br IL_0280

	IL_023b:  ldloc.1 
	IL_023c:  ldc.i4 133
	IL_0241:  add.ovf 
	IL_0242:  stloc.1 
	IL_0243:  br IL_0280

	IL_0248:  ldloc.1 
	IL_0249:  ldc.i4.s 0x0c
	IL_024b:  add.ovf 
	IL_024c:  stloc.1 
	IL_024d:  br IL_0280

	IL_0252:  ldloc.1 
	IL_0253:  ldc.i4.s 0x7b
	IL_0255:  add.ovf 
	IL_0256:  stloc.1 
	IL_0257:  br IL_0280

	IL_025c:  ldloc.1 
	IL_025d:  ldc.i4.s 0x7c
	IL_025f:  add.ovf 
	IL_0260:  stloc.1 
	IL_0261:  br IL_0280

	IL_0266:  ldloc.1 
	IL_0267:  ldc.i4 156
	IL_026c:  add.ovf 
	IL_026d:  stloc.1 
	IL_026e:  br IL_0280

	IL_0273:  ldloc.1 
	IL_0274:  ldc.i4 153
	IL_0279:  add.ovf 
	IL_027a:  stloc.1 
	IL_027b:  br IL_0280

	IL_0280:  ldloc.2 
	IL_0281:  ldc.i4.1 
	IL_0282:  add.ovf 
	IL_0283:  stloc.2 
	IL_0284:  br IL_00e4

	IL_0289:  ldloc.1 
	IL_028a:  call void class [mscorlib]System.Console::WriteLine(int32)
	IL_028f:  ret 
    } // end of method Test::default void test_it (int32 max) 

    // method line 94
    .method private static  hidebysig 
           default void Main ()  cil managed 
    {
        // Method begins at RVA 0x2660
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
	IL_0016:  ldstr "match-switch.n:62:10: : execution took "
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

