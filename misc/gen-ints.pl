#!/usr/bin/perl

sub inttype {
my ($long, $short, $signed, $int, $suff) = @_;
print <<EOF
  public class $long : INumericType
  {
    public Zero : object
    {
      get { 0$suff } 
    }
    
    public One : object
    {
      get { 1$suff }
    }

    public IsInteger : bool
    {
EOF
;

if ($int) {
print "      get { true }\n";
} else {
print "      get { false }\n";
}
print <<EOF
    }
    
    public IsSigned : bool
    {
EOF
;

if ($signed) {
print "      get { true }\n";
} else {
print "      get { false }\n";
}
print <<EOF
    }
    
    public SystemType : System.Type {
      get { Nemerle.Compiler.SystemType.$long }
    }
    
    public Binary (name : string, x : object, y : object) : object
    {
      def x = (x :> System.$long);
      def y = (y :> System.$long);
      match (name) {
        | "+" => x + y :> object
        | "-" => x - y :> object
        | "*" => x * y :> object
        | "/" => x / y :> object
        | "%" => x % y :> object
EOF
;
if ($int) {
print <<EOF
        | "%&" => x %& y :> object
        | "%|" => x %| y :> object
        | "%^" => x %^ y :> object
        | "<<" => x << (y :> int) :> object
        | ">>" => x >> (y :> int) :> object
EOF
;
}
print <<EOF
        | _ => 
          null
          // Util.ice ("invalid $short operator `" + name + "'")
      }
    }

    public Unary (name : string, x : object) : object
    {
      def x = x :> System.$long;
      match (name) {
        | "+" => +x :> object
EOF
;

print "        | \"-\" => -x :> object\n" if ($signed);
print "        | \"~\" => ~x :> object\n" if ($int);
print <<EOF
        | _ =>
          null
          // Util.ice ("invalid $short operator `" + name + "'")
      }
    }

    public FromLiteral (lit : Literal) : object
    {
      | L_$short (x) => x : object
      | _ => null
    }

    public ToLiteral (x : object) : Literal
    {
      L_$short (x :> System.$long)
    }

    public GetNemerleType () : Typedtree.Type
    {
      InternalType.$long
    }

    public GetTycon () : Tycon
    {
      InternalType.${long}_tc
    }

    public this () {}
  }


EOF
;}

print "// Begin generated code\n";
print "// Please edit ../misc/gen-ints.n, and not this file\n";
inttype ("SByte", "sbyte", 1, 1, "B");
inttype ("Byte", "byte", 0, 1, "UB");
inttype ("Int16", "short", 1, 1, "S");
inttype ("UInt16", "ushort", 0, 1, "US");
inttype ("Int32", "int", 1, 1, "");
inttype ("UInt32", "uint", 0, 1, "U");
inttype ("Int64", "long", 1, 1, "L");
inttype ("UInt64", "ulong", 0, 1, "UL");

inttype ("Single", "float", 1, 0, ".0F");
inttype ("Double", "double", 1, 0, ".0");
print "// End generated code\n";


# vim: expandtab
