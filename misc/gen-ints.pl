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
    
    public Binary (is_checked : bool, name : string, x : object, y : object) : object
    {
      def x = (x :> System.$long);
      def y = (y :> System.$long);
      if (is_checked)
        match (name) {
          | "+" => (x + y) : object
          | "-" => x - y
          | "*" => x * y
          | "/" => x / y
          | "%" => x % y
EOF
;
$maybecast = ($short eq 'Int') ? "" : ":> int";
#$maybecast=":> int";
if ($int) {
print <<EOF
          | "&" => x %& y
          | "|" => x %| y
          | "^" => x %^ y
          | "%&" => x %& y
          | "%|" => x %| y
          | "%^" => x %^ y
          | "<<" => x << (y$maybecast)
          | ">>" => x >> (y$maybecast)
EOF
;
}
print <<EOF
          | _ => 
            null
            // Util.ice ("invalid $short operator `" + name + "'")
        }
      else
        unchecked {
          match (name) {
            | "+" => (x + y) : object
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
            | "%^" => x %^ y
            | "&" => x %& y
            | "|" => x %| y
            | "^" => x %^ y
            | "<<" => x << (y$maybecast)
            | ">>" => x >> (y$maybecast)
EOF
;
}
print <<EOF
            | _ => 
              null
              // Util.ice ("invalid $short operator `" + name + "'")
          }
        }
    }

    public Unary (is_checked : bool, name : string, x : object) : object
    {
      def x = x :> System.$long;
      if (is_checked)
        match (name) {
          | "+" => +x : object
EOF
;

print "        | \"-\" => -x\n" if ($signed);
print "        | \"~\" => ~x\n" if ($int);
print <<EOF
          | _ =>
            null
            // Util.ice ("invalid $short operator `" + name + "'")
        }
      else
        unchecked {
          match (name) {
            | "+" => +x : object
EOF
;

print "        | \"-\" => -x\n" if ($signed);
print "        | \"~\" => ~x\n" if ($int);
print <<EOF
            | _ =>
              null
              // Util.ice ("invalid $short operator `" + name + "'")
          }
        }
    }

    public FromLiteral (lit : Literal) : object
    {
      | Literal.$short (x) => x : object
      | Literal.Enum (l, _) => FromLiteral (l)
      | _ => null
    }

    public ToLiteral (x : object) : Literal
    {
      Literal.$short (x :> System.$long)
    }

    public GetNemerleType () : MType
    {
      InternalType.$long
    }

    public GetTypeInfo () : TypeInfo
    {
      InternalType.${long}_tc
    }

    public this () {}
  }


EOF
;}

print "// Begin generated code\n";
print "// Please edit ../misc/gen-ints.n, and not this file\n";
inttype ("SByte", "SByte", 1, 1, "B");
inttype ("Byte", "Byte", 0, 1, "UB");
inttype ("Int16", "Short", 1, 1, "S");
inttype ("UInt16", "UShort", 0, 1, "US");
inttype ("Int32", "Int", 1, 1, "");
inttype ("UInt32", "UInt", 0, 1, "U");
inttype ("Int64", "Long", 1, 1, "L");
inttype ("UInt64", "ULong", 0, 1, "UL");

inttype ("Single", "Float", 1, 0, ".0F");
inttype ("Double", "Double", 1, 0, ".0");
print "// End generated code\n";


# vim: expandtab
