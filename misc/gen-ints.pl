#!/usr/bin/perl

sub inttype {
my ($long, $short, $signed, $int, $suff) = @_;
print <<EOF
  class $long : INumericType {
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
      get { typeof (System.$long) }
    }
    
    public Binary (name : string, x : object, y : object) : object
    {
      def x = (x :> System.$long);
      def y = (y :> System.$long);
      match (name) {
        | "+" => (x + y :> object)
        | "-" => x - y
        | "*" => x * y
        | "/" => x / y
        | "%" => x % y
EOF
;
if ($int) {
print <<EOF
        | "%&" => x %& y
        | "%|" => x %| y
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
      def x = (x :> System.$long);
      match (name) {
        | "+" => (+x : object)
EOF
;

print "        | \"-\" => -x\n" if ($signed);
print <<EOF
        //| "~" => ~x
        | _ =>
          null
          // Util.ice ("invalid $short operator `" + name + "'")
      }
    }

    public FromLiteral (lit : Literal) : object
    {
      | L_$short (x) => (x :> object) // HACK! HACK! HACK! this is a bug in the compiler
      | _ => null
    }

    public ToLiteral (x : object) : Literal
    {
      L_$short ((x :> System.$long))
    }

    public this () {}
  }


EOF
;}

print "// Begin generated code\n";
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
