#!/usr/bin/perl

sub inttype {
my ($long, $short, $signed) = @_;
print <<EOF
  class $long : IIntegerType {
    public Zero : object
    {
      get { 0 } 
    }
    
    public One : object
    {
      get { 1 }
    }
    
    public SystemType : System.Type {
      get { typeof (System.$long) }
    }
    
    public Binary (name : string, x : object, y : object) : object
    {
      def x = (x :> System.$long);
      def y = (y :> System.$long);
      match (name) {
        | "+" => x + y
        | "-" => x - y
        | "*" => x * y
        | "/" => x / y
        | "%" => x % y
        | "%&" => x %& y
        | "%|" => x %| y
        | _ => Util.ice ("invalid $short operator `" + name + "'")
      }
    }

    public Unary (name : string, x : object) : object
    {
      def x = (x :> System.$long);
      match (name) {
        | "+" => +x
EOF
;

print "        | \"-\" => -x\n" if ($signed);
print <<EOF
        //| "~" => ~x
        | _ => Util.ice ("invalid $short operator `" + name + "'")
      }
    }

    public FromLiteral (lit : Literal) : object
    {
      | L_$short (x) => (x : object)
      | _ => null
    }

    public this () {}
  }


EOF
;}

print "// Begin generated code\n";
inttype ("SByte", "sbyte", 1);
inttype ("Byte", "byte", 0);
inttype ("Int16", "short", 1);
inttype ("UInt16", "ushort", 0);
inttype ("Int32", "int", 1);
inttype ("UInt32", "uint", 0);
inttype ("Int64", "long", 1);
inttype ("UInt64", "ulong", 0);
print "// End generated code\n";
