// Copyright (c) 2003, 2004 The University of Wroclaw.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//    1. Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//    2. Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//    3. The name of the University may not be used to endorse or promote
//       products derived from this software without specific prior
//       written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
// NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
// TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
 
header{
    using System.Collections;
    using Nemerle.Collections;
}


options 
{   
    language = "CSharp";
    namespace = "Nemerle.CSharp";
}

/* ************************************************************************* */
/*                                                                           */
/* PARSER                                                                    */
/*                                                                           */
/* ************************************************************************* */

class CSharpParser extends Parser;

options 
{   
    k = 2;
}

//--------------------------
// C.0 Moved here from lexer
//--------------------------

literal 
returns [string return_string]
{
    return_string = "";
}
    :   return_string = boolean_literal 
    |   il:INTEGER_LITERAL               {return_string = il.getText();}
    |   hil:HEXADECIMAL_INTEGER_LITERAL  {return_string = hil.getText();}
    |   rl:REAL_LITERAL                  {return_string = rl.getText();}
    |   chl:CHARACTER_LITERAL            {return_string = chl.getText();}
    |   return_string = string_literal      
    |   n:NULL                           {return_string = n.getText();}
    ;
    
boolean_literal
returns [string return_string]
{
    return_string = "";
}
    :   t:TRUE  {return_string = t.getText();}
    |   f:FALSE   {return_string = f.getText();}
    ;

string_literal
returns [string return_string]
{
    return_string = "";
}
    :   rsl:REGULAR_STRING_LITERAL   {return_string = rsl.getText();}
    |   vsl:VERBATIM_STRING_LITERAL  {return_string = vsl.getText();}
    ;

//--------------------
//C.2.1 Basic concepts
//--------------------

namespace_name
returns [string [] return_strings]
{
    return_strings = new string[]{"",""};
}
    :   return_strings = namespace_or_type_name
    ;

type_name
returns [string [] return_strings]
{
    return_strings = new string[]{"",""};;
}
    :   return_strings = namespace_or_type_name
    ;

namespace_or_type_name
returns [string [] return_strings]
{
    return_strings = new string[]{"",""};;
}
    :   id1:IDENTIFIER  
        {return_strings[0] = ExtendedToken.getWhitespaces (id1);
         return_strings[1] = ExtendedToken.getTextOnly (id1);}
        (options {greedy=true;}: DOT  id2:IDENTIFIER 
            {return_strings[0] += ExtendedToken.getWhitespaces (id2);
             return_strings[1] += ("." + ExtendedToken.getTextOnly (id2));}
        )*
    ;      

//-----------  
//C.2.2 Types
//-----------

type
returns [string [] return_strings]
{
    return_strings = new string[]{"",""};
}
    :   (array_type)=> return_strings = array_type
    |   o:OBJECT 
        {return_strings[0] = ExtendedToken.getWhitespaces (o);
         return_strings[1] = ExtendedToken.getTextOnly (o);}
    |   s:STRING 
        {return_strings[0] = ExtendedToken.getWhitespaces (s);
         return_strings[1] = ExtendedToken.getTextOnly (s);}
    |   return_strings = type_name
    |   return_strings = simple_type
    ;

simple_type
returns [string [] return_strings]
{
    return_strings = new string[]{"",""};
}
    :   return_strings = numeric_type
    |   b:BOOL  
        {return_strings[0] = ExtendedToken.getWhitespaces (b);
         return_strings[1] = ExtendedToken.getTextOnly (b);}
    ;

numeric_type
returns [string [] return_strings]
{
    return_strings = new string[]{"",""};
}
    :   return_strings = integral_type
    |   return_strings = floating_point_type
    |   d:DECIMAL  
        {return_strings[0] = ExtendedToken.getWhitespaces (d);
         return_strings[1] = ExtendedToken.getTextOnly (d);}
    ;

integral_type
returns [string [] return_strings]
{
    return_strings = new string []{"",""};
}
    :   i1:CHAR   
        {return_strings[0] = ExtendedToken.getWhitespaces (i1);
         return_strings[1] = ExtendedToken.getTextOnly (i1);}
    |   i2:INT    
        {return_strings[0] = ExtendedToken.getWhitespaces (i2);
         return_strings[1] = ExtendedToken.getTextOnly (i2);}
    |   i3:LONG   
        {return_strings[0] = ExtendedToken.getWhitespaces (i3);
         return_strings[1] = ExtendedToken.getTextOnly (i3);}
    |   i4:SBYTE  
        {return_strings[0] = ExtendedToken.getWhitespaces (i4);
         return_strings[1] = ExtendedToken.getTextOnly (i4);}
    |   i5:BYTE   
        {return_strings[0] = ExtendedToken.getWhitespaces (i5);
         return_strings[1] = ExtendedToken.getTextOnly (i5);}
    |   i6:SHORT  
        {return_strings[0] = ExtendedToken.getWhitespaces (i6);
         return_strings[1] = ExtendedToken.getTextOnly (i6);}
    |   i7:UINT   
        {return_strings[0] = ExtendedToken.getWhitespaces (i7);
         return_strings[1] = ExtendedToken.getTextOnly (i7);}
    |   i8:ULONG  
        {return_strings[0] = ExtendedToken.getWhitespaces (i8);
         return_strings[1] = ExtendedToken.getTextOnly (i8);}
    |   i9:USHORT 
        {return_strings[0] = ExtendedToken.getWhitespaces (i9);
         return_strings[1] = ExtendedToken.getTextOnly (i9);}
    ;

floating_point_type
returns [string [] return_strings]
{
    return_strings = new string[]{"",""};
}
    :   f:FLOAT   
        {return_strings[0] = ExtendedToken.getWhitespaces (f);
         return_strings[1] = ExtendedToken.getTextOnly (f);}
    |   d:DOUBLE  
        {return_strings[0] = ExtendedToken.getWhitespaces (d);
         return_strings[1] = ExtendedToken.getTextOnly (d);}
    ;

array_type
returns [string [] return_strings]
{
    string [] rank = new string[] {"",""};
    return_strings = new string[] {"",""};
    string [] nat  = new string[] {"",""};
    string end = "";
}
    :   nat = non_array_type   rank = rank_specifier 
        {return_strings[1] = " array [" + rank[0];
            if(rank[1] != "1")
               return_strings[1] += (rank[1] + ",");
         return_strings[0] = nat[0];
            end = "]";
        }
        (rank = rank_specifier
            { return_strings[1] += " array [" + rank[0];
                if(rank[1] != "1")
                    return_strings[1] += (rank[1] + ",");
              end += "]"; }
        )*
        {   return_strings[1] += nat[1];
            return_strings[1] += end;
        }
    ; 

non_array_type
returns [string [] return_strings]
{ 
    return_strings = new string[] {"",""};
}
    :   o:OBJECT 
        {return_strings[0] = ExtendedToken.getWhitespaces (o);
         return_strings[1] = ExtendedToken.getTextOnly (o);}
    |   s:STRING 
        {return_strings[0] = ExtendedToken.getWhitespaces (s);
         return_strings[1] = ExtendedToken.getTextOnly (s);}
    |   return_strings = type_name
    |   return_strings = simple_type
    ;

rank_specifier
returns [string [] return_strings]
{ 
    return_strings = new string[] {"",""};
    int rank = 1;
}
    :   lb:LBRACK { return_strings[0] = ExtendedToken.getWhitespaces (lb);}
        (c:COMMA  {rank += 1;}
            {return_strings[0] = ExtendedToken.getWhitespaces (c);}
        )*   
        rb:RBRACK 
        {return_strings[0] = ExtendedToken.getWhitespaces (rb);
         return_strings[1] = rank.ToString ();   
        }
    ;

//---------------
//C.2.3 Variables
//---------------

variable_reference
returns [string return_string]
{
    return_string = "";
}
    :   return_string = expression
    ;

//-----------------
//C.2.4 Expressions
//-----------------

argument_list
returns [string return_string]
{
    string ar = "";
    return_string = "";
}
    :   return_string = argument 
        (c:COMMA   ar = argument
            {
                return_string += (c.getText () + ar);
            }
        )*
    ;

argument
returns [string return_string]
{
    return_string = "";
}
    :   return_string =  expression
    |   r:REF   return_string = variable_reference { return_string = r.getText () + return_string ; }
    |   o:OUT   return_string = variable_reference { return_string = o.getText () + return_string ; }
    ;

primary_expression    
returns [string return_string]
{
    string sope = "";
    string soace = "";
    return_string = "";
}
    :   (array_creation_expression)=>
            return_string = array_creation_expression (options {greedy=true;}: 
            soace = suffix_of_array_creation_expression { return_string += soace; })* 
    |   return_string = primary_no_array_creation_expression 
            (options {greedy=true;}: sope = suffix_of_primary_expression { return_string += sope; })*
    ;

suffix_of_array_creation_expression
returns [string return_string]
{
    return_string = "";
}
    :   return_string = member_access_end 
    |   d:DEC { return_string = d.getText (); }
    |   i:INC { return_string = i.getText (); }
    |   return_string = invocation_expression_end
    ;

suffix_of_primary_expression
returns [string return_string]
{
    return_string = "";
}
    :   return_string = suffix_of_array_creation_expression
    |   return_string = element_access_end
    ;

primary_no_array_creation_expression
returns [string return_string]
{
    string pt = "";
    return_string = "";
}
    :   return_string = literal
    |   return_string = simple_name
    |   return_string = parenthesized_expression
    |   return_string = this_access
    |   return_string = base_access
    |   pt = predefined_type   DOT   id:IDENTIFIER 
        { return_string = pt + "." + id.getText (); }
    |   (object_creation_expression)=>
            return_string = object_creation_expression
    |   return_string = delegate_creation_expression
    |   return_string = typeof_expression
    |   return_string = checked_expression
    |   return_string = unchecked_expression
    ;

delegate_creation_expression
returns [string return_string]
{
    string [] tp = new string[]{"",""};
    string exp = "";
    return_string = "";
}
    :   n:NEW   tp = type   lp:LPAREN   exp = expression rp:RPAREN
        {
            return_string = ((ExtendedToken)n).GetWhitespaces ()  + tp[0]+tp[1] + lp.getText () + exp + rp.getText ();
        }
    ;

object_creation_expression
returns [string return_string]
{
    string [] tp = new string[]{"",""};
    string al = "";
    return_string = "";
}
    :   n:NEW   tp = type  lp:LPAREN   (al = argument_list)?   rp:RPAREN
        {
            return_string = ((ExtendedToken)n).GetWhitespaces () + tp[0]+tp[1] + lp.getText () + al + rp.getText ();
        }
    ;

simple_name
returns [string return_string]
{
    return_string = "";
}
    :   id:IDENTIFIER {return_string = id.getText();}
    ;

parenthesized_expression
returns [string return_string]
{
    string exp = "";
    return_string = "";
}
    :   lp:LPAREN   exp = expression   rp:RPAREN
        {
            return_string = lp.getText () + exp + rp.getText ();
        }
    ;

member_access_end
returns [string return_string]
{
    return_string = "";
}
    :   DOT   id:IDENTIFIER {return_string = "." + id.getText ();}
    ;

predefined_type
returns [string return_string]
{
    return_string = "";
}
    :   pt1:BYTE     {return_string = pt1.getText();}
    |   pt2:CHAR     {return_string = pt2.getText();}
    |   pt3:DECIMAL  {return_string = pt3.getText();}
    |   pt4:DOUBLE   {return_string = pt4.getText();}
    |   pt5:FLOAT    {return_string = pt5.getText();}
    |   pt6:INT      {return_string = pt6.getText();}
    |   pt7:LONG     {return_string = pt7.getText();}
    |   pt8:OBJECT   {return_string = pt8.getText();}
    |   pt9:SBYTE    {return_string = pt9.getText();}
    |   pt10:SHORT   {return_string = pt10.getText();}
    |   pt11:STRING  {return_string = pt11.getText();}
    |   pt12:UINT    {return_string = pt12.getText();}
    |   pt13:ULONG   {return_string = pt13.getText();}
    |   pt14:USHORT  {return_string = pt14.getText();}
    ;

//invocation_expression
//returns [string return_string]
//{
//    return_string = "";
//}
//    :   primary_expression  LPAREN   (argument_list)?   RPAREN 
//    ;

invocation_expression_end
returns [string return_string]
{
    string al = "";
    return_string = "";
}
    :   lp:LPAREN   (al = argument_list)?   rp:RPAREN
        { return_string = lp.getText () + al + rp.getText ();}
    ;


//element_access
//returns [string return_string]
//{
//   return_string = "";
//}
//    :   primary_no_array_creation_expression LBRACK   expression_list   RBRACK
//    ;

element_access_end
returns [string return_string]
{
    string el = "";
    return_string = "";
}
    :   lb:LBRACK   el = expression_list   rb:RBRACK
        { return_string = lb.getText () + el + rb.getText ();}
    ;

expression_list
returns [string return_string]
{
    string exp = "";
    return_string = "";
}
    :   return_string = expression  ( c:COMMA  exp = expression { return_string += (c.getText () + exp); } )*
    ;

this_access
returns [string return_string]
{
    return_string = "";
}
    :   t:THIS {return_string = t.getText ();}
    ;

base_access
returns [string return_string]
{
    string el = "";
    return_string = "";
}
    :   (BASE   DOT)=> b1:BASE DOT id:IDENTIFIER
        {
            return_string = b1.getText () + "." + id.getText ();
        }
    |   b2:BASE   lb:LBRACK   el = expression_list   rb:RBRACK
        {
            return_string = b2.getText () + lb.getText () + el + rb.getText ();
        }
    ;

//post_increment_expression
//returns [string return_string]
//{
//    return_string = "";
//}
//    :   primary_expression   INC
//    ;

//post_decrement_expression
//returns [string return_string]
//{
//    return_string = "";
//}
//    :   primary_expression   DEC
//    ;

array_creation_expression 
returns [string return_string]
{
    string ai = "";
    string el = "";
    return_string = "";
    string [] tp = new string[]{"",""};
    string [] rank = new string[]{"",""};
}
    :   (NEW   array_type   array_initializer)=> 
            n1:NEW   tp = array_type   return_string  = array_initializer
        {
            return_string = ExtendedToken.getWhitespaces (n1) + tp[0] + return_string; 
        }
    |   n2:NEW   
        tp = non_array_type  
        lb:LBRACK   
        el = expression_list   
        rb:RBRACK   
        {
            return_string = ExtendedToken.getWhitespaces (n2) + tp[0]; 
            return_string = ExtendedToken.getWhitespaces (lb) + " array(" + el + ExtendedToken.getWhitespaces (rb) + ")";
        }
        (rank = rank_specifier
        {
            return_string +=  rank[0];
        }
        )*   
        (ai = array_initializer)?       
        {
            return_string +=  ai;
        }
        
    ;

typeof_expression
returns [string return_string]
{
    string [] tp = new string[]{"",""};
    return_string = "";
}
    :   t:TYPEOF   lp:LPAREN   ( v:VOID {tp[0] = v.getText ();} | tp = type )   rp:RPAREN
        {
            return_string = t.getText () + lp.getText () + tp[0]+tp[1] + rp.getText ();
        }
    ;

checked_expression
returns [string return_string]
{
    string exp = "";
    return_string = "";
}
    :   c:CHECKED   lp:LPAREN   exp = expression   rp:RPAREN
        {
            return_string = c.getText () + lp.getText () + exp + rp.getText ();
        }
    ;

unchecked_expression
returns [string return_string]
{
    string exp = "";
    return_string = "";
}
    :   u:UNCHECKED   lp:LPAREN   exp = expression   rp:RPAREN
        {
            return_string = u.getText () + lp.getText () + exp + rp.getText ();
        }
    ;

unary_expression
returns [string return_string]
{
    return_string = "";
}
    :   (cast_expression)=>
            return_string = cast_expression
    |   return_string = primary_expression
    |   p:PLUS   return_string = unary_expression   {return_string = p.getText () + return_string ;  }
    |   m:MINUS   return_string = unary_expression  {return_string = m.getText () + return_string ;  }
    |   l:LNOT   return_string = unary_expression   {return_string = l.getText () + return_string ;  }
    |   b:BNOT   return_string = unary_expression   {return_string = b.getText () + return_string ;  }
    |   STAR return_string = unary_expression  // ------------- ?? -----------------------
    |   return_string = pre_increment_expression
    |   return_string = pre_decrement_expression
    ;

cast_expression
returns [string return_string]
{
    string [] t = new string[]{"",""};
    return_string = "";
}
    :   lp:LPAREN   t = type  rp:RPAREN   return_string = unary_expression
        { return_string = lp.getText () + return_string + " :> " + t[0]+t[1] + rp.getText ()  ;  }
    ;

pre_increment_expression
returns [string return_string]
{
    return_string = "";
}
    :   i:INC  return_string = unary_expression  {return_string = i.getText () + return_string ;  }
    ;

pre_decrement_expression
returns [string return_string]
{
    return_string = "";
}
    :   d:DEC   return_string = unary_expression  {return_string = d.getText () + return_string ;  }
    ;

multiplicative_expression
returns [string return_string]
{
    string mop = "";
    string ue = "";
    return_string = "";
}
    :   return_string = unary_expression  
        (mop = multiplicative_op   ue = unary_expression
            {
                return_string += (mop + ue);
            }
        )*
    ;

multiplicative_op
returns [string return_string]
{
    return_string = "";
}
    :   s:STAR  {return_string = s.getText () ;  }
    |   d:DIV   {return_string = d.getText () ;  }
    |   m:MOD   {return_string = m.getText () ;  }
    ;

additive_expression    
returns [string return_string]
{
    string aop = "";
    string me = "";
    return_string = "";
}
    :   return_string = multiplicative_expression  
        ( aop = additive_op   me = multiplicative_expression
            {
                return_string += (aop + me);
            }
        )*
    ;

additive_op
returns [string return_string]
{
    return_string = "";
}
    :   p:PLUS   {return_string = p.getText () ;  }
    |   m:MINUS  {return_string = m.getText () ;  }
    ;

shift_expression
returns [string return_string]
{
    string sop = "";
    string ae = "";
    return_string = "";
}
    :   return_string = additive_expression   
        (sop = shift_op ae = additive_expression
            {
                return_string += (sop + ae);
            }
        )*
    ;

shift_op
returns [string return_string]
{
    return_string = "";
}
    :   sl:SL  {return_string = sl.getText () ;  }
    |   sr:SR  {return_string = sr.getText () ;  }
    ;

relational_expression    
returns [string return_string]
{
    string rop = "";
    string [] se = new string[]{"",""};
    string s = "";
    return_string = "";
}
    :   (shift_expression   relational_op)=> 
            return_string = shift_expression   
            (rop = relational_op   s = shift_expression
                {
                    return_string += (rop + s);
                }   
            )+
    |   (shift_expression   IS)=> 
            return_string = shift_expression   
            (tis:IS   se = type
                {
                    return_string += (tis.getText () + se[0]+se[1]);
                }
            )+
    |   (shift_expression   AS)=> 
            return_string = shift_expression   
            (tas:AS   se = type
                {
                    return_string += (tas.getText () + se[0]+se[1]);
                }
            )+
    |   return_string = shift_expression
    ;

relational_op
returns [string return_string]
{
    return_string = "";
}
    :   lt:LTHAN {return_string = lt.getText () ;  }
    |   gt:GTHAN {return_string = gt.getText () ;  }
    |   le:LE    {return_string = le.getText () ;  }
    |   ge:GE    {return_string = ge.getText () ;  }
    ;

equality_expression
returns [string return_string]
{
    string eop = "";
    string re = "";
    return_string = "";
}
    :   return_string = relational_expression   
        (eop = equality_op   re = relational_expression
            {
                return_string += (eop + re);
            }
        )*
    ;

equality_op
returns [string return_string]
{
    return_string = "";
}
    :   e:EQUAL     {return_string = e.getText () ;  }
    |   n:NOT_EQUAL {return_string = n.getText () ;  }
    ;

and_expression
returns [string return_string]
{
    string ee = "";
    return_string = "";
}
    :   return_string = equality_expression   
        ( b:BAND   ee = equality_expression
            {
                return_string += (((ExtendedToken)b).GetWhitespaces () + "%" +  ExtendedToken.getTextOnly (b) + ee);
            }
        )*
    ;

exclusive_or_expression
returns [string return_string]
{
    string ae = "";
    return_string = "";
}
    :   return_string = and_expression   
        (b:BXOR   ae = and_expression
            {
                return_string += (((ExtendedToken)b).GetWhitespaces () + "%" +  ExtendedToken.getTextOnly (b) + ae);
            }
        )*
    ;

inclusive_or_expression
returns [string return_string]
{
    string eoe = "";
    return_string = "";
}
    :   return_string = exclusive_or_expression   
        (b:BOR   eoe = exclusive_or_expression
            {
                return_string += (((ExtendedToken)b).GetWhitespaces () + "%" + ExtendedToken.getTextOnly (b) + eoe);
            }
        )*
    ;

conditional_and_expression
returns [string return_string]
{
    string ioe = "";
    return_string = "";
}
    :   return_string = inclusive_or_expression   
        (l:LAND   ioe = inclusive_or_expression
            {
                return_string += (((ExtendedToken)l).GetWhitespaces () + "%" +  ExtendedToken.getTextOnly (l) + ioe);
            }
        )*
    ;

conditional_or_expression
returns [string return_string]
{
    string cae = "";
    return_string = "";
}
    :   return_string = conditional_and_expression   
        (l:LOR   cae = conditional_and_expression
            {
                return_string += (l.getText () + cae);
            }
        )*
    ;

conditional_expression
returns [string return_string]
{
    return_string = "";
}
    :   return_string = conditional_or_expression //  (QUESTION   expression   COLON   expression)? // TODO ------------------------
    ;

assignment_operator 
returns [string return_string]
{
    return_string = "";
}
    :   a1:ASSIGN    {return_string = a1.getText () ;  }
    |   a2:PLUS_ASN  {return_string = a2.getText () ;  }
    |   a3:MINUS_ASN {return_string = a3.getText () ;  }
    |   a4:STAR_ASN  {return_string = a4.getText () ;  }
    |   a5:DIV_ASN   {return_string = a5.getText () ;  }
    |   a6:MOD_ASN   {return_string = a6.getText () ;  }
    |   a7:BAND_ASN  {return_string = a7.getText () ;  }
    |   a8:BOR_ASN   {return_string = a8.getText () ;  }
    |   a9:BXOR_ASN  {return_string = a9.getText () ;  }
    |   a10:SL_ASN   {return_string = a10.getText () ;  }
    |   a11:SR_ASN   {return_string = a11.getText () ;  }
    ;

expression
returns [string return_string]
{
    return_string = "";
}
    :   (conditional_expression)=> return_string = conditional_expression
    |   return_string = assignment
    ;

constant_expression
returns [string return_string]
{
    return_string = "";
}
    :   return_string = expression
    ;

boolean_expression
returns [string return_string]
{
    return_string = "";
}
    :   return_string = expression
    ;

//----------------
//C.2.5 Statements
//----------------

statement
returns [StatementTree t]
{
    t = new StatementTree();
    StatementTree st = new StatementTree();
    LinkedList a = new LinkedList ();
}
    //<labelled_statement>
    :   (IDENTIFIER   COLON)=> 
        i:IDENTIFIER    {a.Add (new StatementTree(i));}
        c:COLON         {a.Add (new StatementTree(c));}
        st = statement  {a.Add (st);}
        {t = new StatementTree ("LABEL",a);}
    //</labelled_statement>    

    |   (CONST)=> t = declaration_statement
    |   (type IDENTIFIER)=> t = declaration_statement
    |   t = embedded_statement        
    ;

embedded_statement
returns [StatementTree t]
{
    t = new StatementTree();
}
    :   t = block
    |   t = empty_statement        

    //<checked statement>
    |   (CHECKED   block)=> t = checked_statement
    //</checked statement>

    |   t = selection_statement
    |   t = iteration_statement
    |   t = jump_statement
    |   t = try_statement

    //<unchecked statement>
    |   (UNCHECKED   block)=> t = unchecked_statement
    //</unchecked statement>

    |   t = expression_statement
    |   t = lock_statement
    |   t = using_statement
    ;

empty_statement
returns [StatementTree t]
{
    t = new StatementTree();
}
    :   s:SEMI
        {t = new StatementTree (s);}
    ;

block
returns [StatementTree t]
{
    t = new StatementTree();
    StatementTree temp = new StatementTree ();
    LinkedList a = new LinkedList ();
}
    :   lb:LBRACE  
        {a.Add (new StatementTree(lb));}

        ( temp = statement {a.Add (temp);})*  

        rb:RBRACE
        {a.Add (new StatementTree(rb));}

        {t = new StatementTree ("BLOCK",a);}
    ;

declaration_statement
returns [StatementTree t]
{
    t = new StatementTree();
    string temp = "";
}
    :   temp = local_variable_declaration   s1:SEMI
        {
            t = new StatementTree (temp + ((ExtendedToken)s1).GetWhitespaces ());
        }
    |   temp = local_constant_declaration   s2:SEMI
        {
            t = new StatementTree (temp + ((ExtendedToken)s2).GetWhitespaces ());
        }
    ;

local_variable_declaration
returns [string return_string]
{
    string [] t = new string[]{"",""};
    string temp = "";
    return_string = "";
}
    :   t = type   return_string = local_variable_declarator[t] 
        ( c:COMMA   
            temp = local_variable_declarator[t]
            { return_string += (temp + ((ExtendedToken)c).GetWhitespaces ()); }
        )*
    ;

local_variable_declarator[string [] t]
returns [string return_string]
{
    string lvi = "";
    return_string = "";
}
    :   (IDENTIFIER ASSIGN NULL) =>
        id3:IDENTIFIER   a1:ASSIGN n:NULL
        {
            return_string = t[0] + "mutable " + id3.getText () + a1.getText () +
                            n.getText () + " : " + t[1] + ";";
        }

    |   (IDENTIFIER   ASSIGN)=>
        id1:IDENTIFIER   a:ASSIGN lvi = local_variable_initializer
        {
            return_string = t[0] + "mutable " + id1.getText () + a.getText () + lvi + ";";
        }

    |   id2:IDENTIFIER
        {
            return_string = t[0] + "mutable " + id2.getText () + " = Nemerle.Extensions.DefaultValue ( _ :" + t[1] + ");";
        }
;

local_variable_initializer
returns [string return_string]
{
    return_string = "";
}
    :   return_string = expression
    |   return_string = array_initializer
    ;

local_constant_declaration
returns [string return_string]
{
    return_string = "";
    string [] t = new string[]{"",""};
    string temp = "";
}
    :   c:CONST   t = type   return_string = local_constant_declarator[((ExtendedToken)c).GetWhitespaces (),""]
        (cm:COMMA  
            temp = local_constant_declarator[((ExtendedToken)c).GetWhitespaces (),((ExtendedToken)cm).GetWhitespaces ()]
            { return_string += temp; }
        )*
    ;

local_constant_declarator [string c1,string c2]
returns [string return_string]
{
    return_string = "";
    string ce = "";
}
    :   id:IDENTIFIER   a:ASSIGN   ce = constant_expression
        {
            return_string = c1 + "def " + c2 + id.getText () + a.getText () + ce + ";";
        }
    ;

expression_statement
returns [StatementTree t]
{
    t = new StatementTree();
    string temp = "";
    LinkedList a = new LinkedList ();
}
    :   temp = statement_expression   s:SEMI
        {
            a.Add ( new StatementTree (temp) );
            a.Add ( new StatementTree (s) );
            t = new StatementTree ("EXPRESSION_STATEMENT",a) ;
        }
    ;

statement_expression
returns [string return_string]
{
    return_string = "";
}
    :   (assignment)=>
            return_string = assignment
    |   return_string = primary_expression
     
//    |   (invocation_expression)=>
//            invocation_expression
//    |   (post_decrement_expression)=>
//            post_decrement_expression     
//    |   (post_decrement_expression)=>
//            post_decrement_expression
//    
//    |   object_creation_expression

    |   return_string = pre_increment_expression
    |   return_string = pre_decrement_expression
    ;

assignment
returns [string return_string]
{
    string temp = "";
    return_string = "";
}
    :   return_string = unary_expression   
        temp = assignment_operator   {return_string += temp;}
        temp = expression            {return_string += temp;}
    ;

selection_statement
returns [StatementTree t]
{
    t = new StatementTree();
}
    :   t = if_statement
    |   t = switch_statement
    ;

if_statement
returns [StatementTree t]
{
    t = new StatementTree();
    StatementTree t1 = new StatementTree();
    StatementTree t2; 
    LinkedList a = new LinkedList ();
    string be = "";
}
    :   (IF   LPAREN   boolean_expression   RPAREN   embedded_statement   ELSE)=>
        i1:IF                     {a.Add (new StatementTree(i1));}
        lp1:LPAREN                {a.Add (new StatementTree(lp1));}          
        be = boolean_expression   {a.Add (new StatementTree(be));}
        rp1:RPAREN                {a.Add (new StatementTree(rp1));}
        t1 = embedded_statement   {a.Add (t1);}
        e:ELSE                    {a.Add (new StatementTree(e));t2 = new StatementTree();}
        t2 = embedded_statement   {a.Add (t2);}
        { t = new StatementTree("IF",a);}

    |   i2:IF                     {a.Add (new StatementTree( ((ExtendedToken)i2).GetWhitespaces () + "when"));}
        lp2:LPAREN                {a.Add (new StatementTree(lp2));}
        be = boolean_expression   {a.Add (new StatementTree(be));}
        rp2:RPAREN                {a.Add (new StatementTree(rp2));}
        t1 = embedded_statement   {a.Add (t1);}
        { t = new StatementTree("WHEN",a); }
    ;

switch_statement
returns [StatementTree t]
{
    t = new StatementTree();
}
    :   SWITCH LPAREN   expression  RPAREN   switch_block
    ;

switch_block
returns [StatementTree t]
{
    t = new StatementTree();
}
    :   LBRACE   (switch_section)*   RBRACE
    ;

switch_section
returns [StatementTree t]
{
    t = new StatementTree();
}
    :   (options {greedy=true;}: switch_label)+   (options {greedy=true;}: statement)+
    ;

switch_label
    :   CASE   constant_expression   COLON
    |   DEFAULT COLON
    ;

iteration_statement
returns [StatementTree t]
{
    t = new StatementTree();
}
    :   t = while_statement
    |   t = do_statement
    |   t = for_statement
    |   t = foreach_statement
    ;

while_statement
returns [StatementTree t]
{
    t = new StatementTree();
    StatementTree t1 = new StatementTree();
    LinkedList a = new LinkedList ();
    string be = "";
}
    :   w:WHILE                     {a.Add (new StatementTree(w));}
        lp:LPAREN                   {a.Add (new StatementTree(lp));}
        be = boolean_expression     {a.Add (new StatementTree(be));}
        rp:RPAREN                   {a.Add (new StatementTree(rp));}
        t1 = embedded_statement     {a.Add (t1);}
        { t = new StatementTree("WHILE",a); }
    ;

do_statement
returns [StatementTree t]
{
    t = new StatementTree();
    StatementTree t1 = new StatementTree();
    LinkedList a = new LinkedList ();
    string be = "";
}
    :   d:DO                       {a.Add (new StatementTree(d));}
        t1 = embedded_statement    {a.Add (t1);}
        w:WHILE                    {a.Add (new StatementTree(w));}
        lp:LPAREN                  {a.Add (new StatementTree(lp));}
        be = boolean_expression    {a.Add (new StatementTree(be));}
        rp:RPAREN                  {a.Add (new StatementTree(rp));}
        s:SEMI                     {a.Add (new StatementTree(s));}
        { t = new StatementTree("DO",a); }
    ;

for_statement
returns [StatementTree t]
{
    t = new StatementTree();
     StatementTree temp = new StatementTree();
    string finit = "";
    string fcond = "";
    string fiter = "";
    LinkedList a = new LinkedList ();
}
    :   f:FOR                        {a.Add (new StatementTree(f));}
        lp:LPAREN                    {a.Add (new StatementTree(lp));}
        (finit = for_initializer)?   {a.Add (new StatementTree(finit));}
        s1:SEMI                      {a.Add (new StatementTree(s1));}
        (fcond = for_condition)?     {a.Add (new StatementTree(fcond));}
        s2:SEMI                      {a.Add (new StatementTree(s2));}
        (fiter = for_iterator)?      {a.Add (new StatementTree(fiter));}
        rp:RPAREN                    {a.Add (new StatementTree(rp));}
        temp = embedded_statement    {a.Add (temp);}
        { t = new StatementTree("FOR",a); }
    ;

for_initializer
returns [string return_string]
{
    return_string = "";
}
    :   (type)=> return_string = local_variable_declaration
    |   return_string = statement_expression_list
    ;

for_condition
returns [string return_string]
{
    return_string = "";
}
    :   return_string = boolean_expression
    ;

for_iterator
returns [string return_string]
{
    return_string = "";
}
    :   return_string = statement_expression_list
    ;

statement_expression_list
returns [string return_string]
{
    string se = "";
    return_string = "";
    bool in_loop = false;
}
    :   return_string = statement_expression 
        (  c:COMMA       
            { in_loop = true; }
            se = statement_expression 
            {
                return_string += (ExtendedToken.getWhitespaces (c) + ";" + se);
            }
        )*
        {   if(in_loop)
            {
                return_string = "{" + return_string; 
                return_string += "}" ; 
            }
        }
    ;

foreach_statement
returns [StatementTree t]
{
    t = new StatementTree();
    StatementTree es = new StatementTree();
    LinkedList a = new LinkedList ();
    string [] tp = new string[]{"",""};
    string e = "";
}
    :   f:FOREACH       {a.Add (new StatementTree(f));}
        l:LPAREN        {a.Add (new StatementTree(l));}
        tp = type   
        id:IDENTIFIER   {a.Add (new StatementTree(id));}
                        {a.Add (new StatementTree(" : "));}
                        {a.Add (new StatementTree(tp[0]+tp[1]));}
        i:IN            {a.Add (new StatementTree(i));}
        e = expression  {a.Add (new StatementTree(e));}
        rp:RPAREN       {a.Add (new StatementTree(rp));}
        es = embedded_statement {a.Add (es);}
        { t = new StatementTree("FOREACH",a); }
    ;

jump_statement
returns [StatementTree t]
{
    t = new StatementTree();
}
    :   t = break_statement 
    |   t = continue_statement 
    |   t = goto_statement  
    |   t = return_statement  
    |   t = throw_statement
    ;

break_statement
returns [StatementTree t]
{
    t = new StatementTree();
    LinkedList a = new LinkedList ();
}
    :   b:BREAK s:SEMI
        {a.Add (new StatementTree(b));}
        {a.Add (new StatementTree(s));}
        { t = new StatementTree("BREAK",a); }
    ;

continue_statement
returns [StatementTree t]
{
    t = new StatementTree();
    LinkedList a = new LinkedList ();
}
    :   c:CONTINUE s:SEMI
        {a.Add (new StatementTree(c));}
        {a.Add (new StatementTree(s));}
        { t = new StatementTree("CONTINUE",a); }
    ;

goto_statement
returns [StatementTree t]
{
    t = new StatementTree();
    LinkedList a = new LinkedList ();
}
    :   (GOTO IDENTIFIER)=> 
        g:GOTO       {a.Add (new StatementTree(g));}
        i:IDENTIFIER {a.Add (new StatementTree(i));}
        s:SEMI       {a.Add (new StatementTree(s));}
        { t = new StatementTree("GOTO",a); }
    |   (GOTO CASE)=> GOTO CASE   constant_expression   SEMI
    |   GOTO DEFAULT SEMI
    ;

return_statement
returns [StatementTree t]
{
    t = new StatementTree();
    string exp = "";
    LinkedList a = new LinkedList ();
}
    :   r:RETURN               {a.Add (new StatementTree(r));}
        (exp = expression      {a.Add (new StatementTree(exp));})?    
        s:SEMI                 {a.Add (new StatementTree(s));}
        { t = new StatementTree("RETURN",a); }
    ;

throw_statement
returns [StatementTree t]
{
    t = new StatementTree();
    string e = "";
    LinkedList a = new LinkedList ();
}
    :   th:THROW          {a.Add (new StatementTree(th));}
        (e = expression)? {a.Add (new StatementTree(e));}
        s:SEMI            {a.Add (new StatementTree(s));}
        { t = new StatementTree("THROW",a); }
    ;

try_statement
returns [StatementTree t]
{
    t = new StatementTree();
    StatementTree temp = new StatementTree();
    LinkedList a = new LinkedList ();
}
    :   tr:TRY       {a.Add (new StatementTree(tr));}
        temp = block {a.Add (temp);}  
        {a.Add (new StatementTree ( ExtendedToken.getWhitespaces (tr) + "catch {"));}
        (    temp = catch_clauses {a.Add (temp);}    
             {a.Add (new StatementTree ( ExtendedToken.getWhitespaces (tr) + "}"));}
            (temp = finally_clause {a.Add (temp);}  )? 
            
            | temp = finally_clause {a.Add (temp);})        
            
        { t = new StatementTree("TRY",a); }
    ;

catch_clauses
returns [StatementTree t]
{
    t = new StatementTree();
    StatementTree temp = new StatementTree();
    LinkedList a = new LinkedList ();
}
    :   ( specific_catch_clause )=>
            (options {greedy=true;}: temp = specific_catch_clause {a.Add (temp);}
            )+   
            (temp = general_catch_clause {a.Add (temp);})?
        { t = new StatementTree("CATCH_CLAUSES",a); }
    |   temp = general_catch_clause {a.Add (temp);}
        { t = new StatementTree("CATCH_CLAUSES",a); }
    ;

specific_catch_clause
returns [StatementTree t]
{
    t = new StatementTree();
    string ids = "_";
    string [] tp = new string[]{"",""};
    string catched_type = "";
    LinkedList a = new LinkedList ();
}
    :       c:CATCH         
            lp:LPAREN       
            (     s:STRING {catched_type = s.getText ();}
                | o:OBJECT {catched_type = o.getText ();} 
                | tp = type_name {catched_type = tp[0] + tp[1];}
            )          
            (id:IDENTIFIER {ids = id.getText ();})?   
            rp:RPAREN   {a.Add (new StatementTree(ExtendedToken.getWhitespaces(c) + "| " 
                                                    + ExtendedToken.getWhitespaces(lp) + ids + " : " 
                                                    + catched_type
                                                    + ExtendedToken.getWhitespaces(rp) + " => " ));}
            t = block    {a.Add (t);}        
            { t = new StatementTree("SPECIFIC_CATCH",a); }
    ;   

general_catch_clause
returns [StatementTree t]
{
    t = new StatementTree();
    LinkedList a = new LinkedList ();
}
    :   c:CATCH   {a.Add (new StatementTree(ExtendedToken.getWhitespaces(c) + "| _ : System.Exception => " ));}
        t = block {a.Add (t);}
        { t = new StatementTree("GENERAL_CATCH",a); }
    ;

finally_clause
returns [StatementTree t]
{
    t = new StatementTree();
    LinkedList a = new LinkedList ();
}
    :   f:FINALLY   {a.Add (new StatementTree(f));}
        t = block   {a.Add (t);}
        { t = new StatementTree("FINALLY",a); }
    ;

checked_statement
returns [StatementTree t]
{
    t = new StatementTree();
    LinkedList a = new LinkedList ();
}
    :   c:CHECKED   {a.Add (new StatementTree(c));}
        t = block   {a.Add (t);}
        { t = new StatementTree("CHECKED",a); }
    ;

unchecked_statement
returns [StatementTree t]
{
    t = new StatementTree();
    LinkedList a = new LinkedList ();
}
    :   u:UNCHECKED  {a.Add (new StatementTree(u));}
        t = block    {a.Add (t);}
        { t = new StatementTree("UNCHECKED",a); }
    ;

lock_statement
returns [StatementTree t]
{
    t = new StatementTree();
    string e = "";
    LinkedList a = new LinkedList ();
}
    :   l:LOCK                  {a.Add (new StatementTree(l));}
        lp:LPAREN               {a.Add (new StatementTree(lp));}
        e = expression          {a.Add (new StatementTree(e));}
        rp:RPAREN               {a.Add (new StatementTree(rp));}
        t = embedded_statement  {a.Add (t);}
        { t = new StatementTree("LOCK",a); }
    ;

using_statement
returns [StatementTree t]
{
    t = new StatementTree() ;    
    LinkedList a = new LinkedList ();
    string e = "";
}
    :   u:USING                  {a.Add (new StatementTree(u));}
        lp:LPAREN                {a.Add (new StatementTree(lp));}
        t = resource_acquisition {a.Add (t);}
        rp:RPAREN                {a.Add (new StatementTree(rp));}
        t = embedded_statement   {a.Add (t);}  
        { t = new StatementTree("USING_STATEMENT",a); }
    ;

resource_acquisition
returns [StatementTree t]
{
    t = new StatementTree() ;
    string e = "";
}
    :   (type)=> e = local_variable_declaration { t = new StatementTree(e);}
    |   e = expression { t = new StatementTree(e);}
    ;

//----------------
//C.2.6 Namespaces
//----------------

compilation_unit
    :   (using_directive)*  
        (options {greedy=true;}:global_attributes)*  
        (namespace_member_declaration )* 
        {
            Emit.EmitString ("\n");
        }
    ;


namespace_declaration
{
    string semi = "";
    string qi;
}
    :   ns:NAMESPACE   qi = qualified_identifier   
        {
            Emit.EmitToken (ns);
            Emit.EmitString (qi);
        }
        
        namespace_body   (s:SEMI {semi = s.getText();})?
        {
            Emit.EmitString (semi);
        }
    ;


qualified_identifier
returns [string return_string]
{
    return_string = "";
}
    :   id1:IDENTIFIER {return_string = id1.getText();}  
        (DOT  id2:IDENTIFIER {return_string += ("." + id2.getText());})*
    ;


namespace_body 
    :   lb:LBRACE   
        {
            Emit.EmitToken (lb);
        }

        (using_directive)* (namespace_member_declaration)*   
        
        rb:RBRACE
        {                        
            Emit.EmitToken (rb);
        }
    ;


using_directive
{
    string [] nn = new string[]{"",""};
}
    :   (USING  IDENTIFIER ASSIGN)=> 
        u1:USING  id:IDENTIFIER a:ASSIGN  
        {
            Emit.EmitToken (u1);
            Emit.EmitToken (id);
            Emit.EmitToken (a);
        }
        nn = namespace_or_type_name s1:SEMI
        {    
            Emit.EmitString (nn[0]+nn[1]);
            Emit.EmitToken (s1);
        }
    |   u2:USING  
        {         
            Emit.EmitToken (u2);
        }
        nn = namespace_name 
        s2:SEMI
        {         
            Emit.EmitString (nn[0]+nn[1]);
            Emit.EmitToken (s2);
        }
    ;

namespace_member_declaration
    :   namespace_declaration 
    |   type_declaration 
    ;

type_declaration
    :   ((attributes)?   (enum_modifier)*   ENUM )=>
            enum_declaration    
    |   ((attributes)?   (struct_modifier)*   STRUCT)=>
            struct_declaration
    |   ((attributes)?   (interface_modifier)*   INTERFACE)=>
            interface_declaration
    |   ((attributes)?   (class_modifier)*   CLASS IDENTIFIER)=>
            class_declaration 
    |   delegate_declaration
    ;


//-------------
//C.2.7 Classes
//-------------

class_declaration
{
    string semi = "";
}
    :   (attributes)?   (class_modifier)*   
        c:CLASS id:IDENTIFIER  
        {
            Emit.EmitToken (c);
            Emit.EmitToken (id);
        }
        (class_base)?   class_body   (s:SEMI {semi = s.getText();})?
        {
            Emit.EmitString (semi);
        }
    ;

class_modifier
    :   cm1:NEW          { Emit.EmitToken(cm1);}
    |   cm2:PUBLIC       { Emit.EmitToken(cm2);}
    |   cm3:PROTECTED    { Emit.EmitToken(cm3);}
    |   cm4:INTERNAL     { Emit.EmitToken(cm4);}
    |   cm5:PRIVATE      { Emit.EmitToken(cm5);}
    |   cm6:SEALED       { Emit.EmitToken(cm6);}
    |   cm7:ABSTRACT     { Emit.EmitToken(cm7);}

    ;

class_base
{
    string [] t1 = new string[]{"",""};
}
    :   cl:COLON 
        {
            Emit.EmitToken (cl);
        }  
        t1 = type_name  
        {
            Emit.EmitString (t1[0]+t1[1]);
        }
        (cm:COMMA {Emit.EmitToken (cm);} 
            t1 = type_name {Emit.EmitString (t1[0]+t1[1]);})*
    ;

class_body
    :   lb:LBRACE   
        {
            Emit.EmitToken (lb);
        }
        
        (class_member_declaration)*   

        rb:RBRACE
        {
            Emit.EmitToken (rb);
        }
    ;

class_member_declaration
    :   (constant_declaration)=> 
            constant_declaration
    |   (field_declaration)=> 
            field_declaration
    |   (method_declaration)=> 
            method_declaration
    |   (property_declaration)=>
            property_declaration
    |   (event_declaration)=>
            event_declaration
    |   (indexer_declaration)=>
            indexer_declaration
    |   (operator_declaration)=>
            operator_declaration
    |   (constructor_declaration)=>
            constructor_declaration
    |   (destructor_declaration)=>
            destructor_declaration
    |   (static_constructor_declaration)=>
            static_constructor_declaration
    |   type_declaration
    ;

constant_declaration
{
    string [] t = new string[]{"",""};
}
    :   { Emit.BeginBuffer ();}
        (attributes)?   (constant_modifier)*   
        { Emit.EndBuffer () ;} 
        c1:CONST   t=type   constant_declarator[false, Emit.Buffer + ExtendedToken.getWhitespaces(c1) ,t[0]+t[1]] 
        (c2:COMMA constant_declarator[true, Emit.Buffer  + ExtendedToken.getWhitespaces(c2) ,t[0]+t[1]])*   s:SEMI
        { Emit.EmitString(ExtendedToken.getWhitespaces (s));}
    ;

constant_modifier
    :   cm1:NEW            {Emit.EmitToken (cm1);}
    |   cm2:PUBLIC         {Emit.EmitToken (cm2);}
    |   cm3:PROTECTED      {Emit.EmitToken (cm3);}
    |   cm4:INTERNAL       {Emit.EmitToken (cm4);}
    |   cm5:PRIVATE        {Emit.EmitToken (cm5);}
    ;

constant_declarator [bool emit_prefix,string prefix,string t]
{
    string ce = "";
}
    :   { 
            if(emit_prefix)
                Emit.EmitString (prefix); 
        }
        id:IDENTIFIER   a:ASSIGN   
        {
            if(!prefix.EndsWith(" ") && !id.getText().StartsWith(" "))
                Emit.EmitString (" " + id.getText());
            else
                Emit.EmitToken (id);
            Emit.EmitString (" : ");
            Emit.EmitString (t);            
            Emit.EmitToken (a);
        }
        ce = constant_expression    
        {
            Emit.EmitString (ce);
            Emit.EmitString (";");
        }
    ;

field_declaration
{
    string [] t = new string[]{"",""};
    bool is_readonly = false;
    bool ret;
}
    :   { Emit.BeginBuffer ();}
        (attributes)?   (ret = field_modifier {is_readonly = is_readonly || ret;} )*   
        t = type
        { 
            Emit.EmitString(t[0]);
            if (!is_readonly)
            {
                Emit.EmitString(" mutable ");                
            }   
            Emit.EndBuffer () ;
        } 
        variable_declarator[false,""," " +t[1]] 
        (c:COMMA variable_declarator[true,Emit.Buffer + ExtendedToken.getWhitespaces (c) ," " + t[1]])*  s:SEMI
        { Emit.EmitString(ExtendedToken.getWhitespaces (s)); }
    ;

field_modifier
returns [bool is_readonly]
{
    is_readonly = false;
}
    :   fm1:NEW            {Emit.EmitToken (fm1);}
    |   fm2:PUBLIC         {Emit.EmitToken (fm2);}
    |   fm3:PROTECTED      {Emit.EmitToken (fm3);}
    |   fm4:INTERNAL       {Emit.EmitToken (fm4);}
    |   fm5:PRIVATE        {Emit.EmitToken (fm5);}
    |   fm6:STATIC         {Emit.EmitToken (fm6);}
    |   fm7:READONLY       {is_readonly = true;}
    |   fm8:VOLATILE       {Emit.EmitToken (fm8);}
    ;

variable_declarator [bool emit_prefix,string prefix,string t]
{
    string temp = "";
}
    :   {
            if(emit_prefix)
                Emit.EmitString(prefix);
        }
        id:IDENTIFIER 
        {
            Emit.EmitToken (id);
            Emit.EmitString (" :");
            Emit.EmitString (t);
        }
        (a:ASSIGN {Emit.EmitToken (a);}
            temp = variable_initializer { Emit.EmitString (temp);})? 
        {Emit.EmitString (";"); }
    ;
    
variable_initializer
returns [string return_string]
{
    return_string = "";
}
    :   (array_initializer)=> return_string = array_initializer        
    |   return_string = expression       
    ;

method_declaration
    :   method_header   method_body
    ;

method_header
{
    string [] rt =  new string[]{"",""};
    bool mod = false;
}
    :   (attributes)?   (method_modifier {mod = true;})*  rt = return_type_array  
        { 
            if(!mod)
                Emit.EmitString(rt[0]);
        }
        member_name           
        lp:LPAREN   {Emit.EmitToken (lp);}
        (formal_parameter_list)? 
        rp:RPAREN   
        {
            Emit.EmitToken (rp);
            Emit.EmitString (" : ");         
            if(mod)
                Emit.EmitString(rt[0]);
            Emit.EmitString (rt[1]);
        }
        
        
    ;

method_modifier
    :   cm1:NEW        {Emit.EmitToken (cm1);}
    |   cm2:PUBLIC     {Emit.EmitToken (cm2);}
    |   cm3:PROTECTED  {Emit.EmitToken (cm3);}
    |   cm4:INTERNAL   {Emit.EmitToken (cm4);}
    |   cm5:PRIVATE    {Emit.EmitToken (cm5);}
    |   cm6:STATIC     {Emit.EmitToken (cm6);}
    |   cm7:VIRTUAL    {Emit.EmitToken (cm7);}
    |   cm8:SEALED     {Emit.EmitToken (cm8);}
    |   cm9:OVERRIDE   {Emit.EmitToken (cm9);}
    |   cm10:ABSTRACT  {Emit.EmitToken (cm10);}
    |   cm11:EXTERN    {Emit.EmitToken (cm11);}
    ;

return_type 
returns [string return_string]
{
    return_string = "";
    string [] temp = new string[] {"",""};
}
    :   temp = type
        { return_string = temp[0] + temp[1]; }
    |   v:VOID {return_string = v.getText();}
    ;

return_type_array
returns [string [] return_strings]
{
    return_strings = new string[] {"",""};
}
    :   return_strings = type
    |   v:VOID 
        { return_strings[0] = ExtendedToken.getWhitespaces (v); 
          return_strings[1] = ExtendedToken.getTextOnly (v); }
    ;

member_name
{
    string [] mn = new string[]{"",""};
}
    :   mn = type_name 
        {
            Emit.EmitString (mn[0]+mn[1]);
        }
    ;

method_body
{
    StatementTree t = new StatementTree ();
}
    :   t = block 
        {
            Emit.EmitString ( t.ToString () );
        }
    |   s:SEMI
        { Emit.EmitToken (s); }
    ;

formal_parameter_list
    :   (fixed_parameter (options {greedy=true;}:COMMA   fixed_parameter)*   COMMA   parameter_array)=>
            fixed_parameter 
            (options {greedy=true;}:
                c1:COMMA   { Emit.EmitToken (c1); }
                fixed_parameter            
            )*   
            c3:COMMA  { Emit.EmitToken (c3); } parameter_array
    |   (fixed_parameter (COMMA   fixed_parameter)*) =>       
            fixed_parameter 
            (c2:COMMA   { Emit.EmitToken (c2); }
            fixed_parameter            
            )*       
    |   parameter_array
    ;

fixed_parameter
{
    string p = "";
    string [] t = new string[]{"",""};
}
    :   (attributes)?   (p = parameter_modifier)?   t = type  id:IDENTIFIER
        {
            Emit.EmitString ( id.getText () + " : " + p + t[0]+t[1] );
        }        
    ;

parameter_modifier
returns [string return_string]
{
    return_string = "";
}
    :   r:REF {return_string = r.getText();}
    |   o:OUT {return_string = o.getText();}
    ;

parameter_array
{
    string [] t = new string[]{"",""};
}
    :   (attributes)?   p:PARAMS   t = array_type  id:IDENTIFIER
        {
            Emit.EmitString ( p.getText () + id.getText () + " :" + t[0]+t[1] );
        }        
    ;

property_declaration
{
    string [] t= new string[]{"",""};
    bool mod = false;
}
    :   (attributes)?   (property_modifier {mod = true;})*   t = type   
        {
            if(!mod)
                Emit.EmitString(t[0]);
        }
        member_name   
        {
            if(mod)
                Emit.EmitString(" :" + t[0]+t[1]);
            else
                Emit.EmitString(" : " + t[1]);
        }

        lb:LBRACE   
        {Emit.EmitToken (lb);}

        accessor_declarations   

        rb:RBRACE
        {Emit.EmitToken (rb);}
    ;

property_modifier
    :   cm1:NEW        {Emit.EmitToken (cm1);}
    |   cm2:PUBLIC     {Emit.EmitToken (cm2);}
    |   cm3:PROTECTED  {Emit.EmitToken (cm3);}
    |   cm4:INTERNAL   {Emit.EmitToken (cm4);}
    |   cm5:PRIVATE    {Emit.EmitToken (cm5);}
    |   cm6:STATIC     {Emit.EmitToken (cm6);}
    |   cm7:VIRTUAL    {Emit.EmitToken (cm7);}
    |   cm8:SEALED     {Emit.EmitToken (cm8);}
    |   cm9:OVERRIDE   {Emit.EmitToken (cm9);}
    |   cm10:ABSTRACT  {Emit.EmitToken (cm10);}
    |   cm11:EXTERN    {Emit.EmitToken (cm11);}
    ;
    
accessor_declarations
    :   (get_accessor_declaration)=> get_accessor_declaration   (set_accessor_declaration)?
    |   set_accessor_declaration   (get_accessor_declaration)?
    ;

get_accessor_declaration
    :   (attributes)?   
        g:GET    {Emit.EmitToken(g);}
        accessor_body
    ;

set_accessor_declaration
    :   (attributes)?   
        s:SET    {Emit.EmitToken(s);}
        accessor_body
    ;

accessor_body
{
    StatementTree t = new StatementTree ();
}
    :   t = block 
        {
            Emit.EmitString ( t.ToString () );
        }
    |   s:SEMI   {Emit.EmitToken(s);}
    ;

event_declaration
returns [string return_string]
{
    return_string = "";
}
    :   ((attributes)?   (event_modifier)*   EVENT  type   member_name   RBRACE)=>

        (attributes)?   (event_modifier)*   
        e1:EVENT  
        type   
        member_name   
        lb1:LBRACE   
        event_accessor_declarations   
        rb1:RBRACE
        
    |   (attributes)?   (event_modifier)*   
        EVENT  
        type   
        variable_declarator[false,"",""] (COMMA variable_declarator[false,"",""])*   SEMI
    ;

event_modifier
returns [string return_string]
{
    return_string = "";
}
    :   NEW
    |   PUBLIC
    |   PROTECTED
    |   INTERNAL
    |   PRIVATE
    |   STATIC
    |   VIRTUAL
    |   SEALED
    |   OVERRIDE
    |   ABSTARCT
    |   EXTERN
    ;

event_accessor_declarations
returns [string return_string]
{
    return_string = "";
}
    :   ((attributes)?   ADD)=>
            add_accessor_declaration   remove_accessor_declaration
    |   remove_accessor_declaration   add_accessor_declaration
    ;

add_accessor_declaration
returns [string return_string]
{
    return_string = "";
}
    :   (attributes)?  idaad:IDENTIFIER {idaad.getText()=="add"}?    block 
    ;

remove_accessor_declaration
returns [string return_string]
{
    return_string = "";
}
    :   (attributes)?  idrad:IDENTIFIER {idrad.getText()=="remove"}?  block 
    ;

indexer_declaration
{
    bool mod = false;
}
    :   (attributes)?   (indexer_modifier {mod = true;})*   
        indexer_declarator[mod]   
        lb:LBRACE              {Emit.EmitToken (lb);}
        accessor_declarations   
        rb:RBRACE              {Emit.EmitToken (rb);}
    ;

indexer_modifier
    :   cm1:NEW        {Emit.EmitToken (cm1);}
    |   cm2:PUBLIC     {Emit.EmitToken (cm2);}
    |   cm3:PROTECTED  {Emit.EmitToken (cm3);}
    |   cm4:INTERNAL   {Emit.EmitToken (cm4);}
    |   cm5:PRIVATE    {Emit.EmitToken (cm5);}
    |   cm6:STATIC     {Emit.EmitToken (cm6);}
    |   cm7:VIRTUAL    {Emit.EmitToken (cm7);}
    |   cm8:SEALED     {Emit.EmitToken (cm8);}
    |   cm9:OVERRIDE   {Emit.EmitToken (cm9);}
    |   cm10:ABSTRACT  {Emit.EmitToken (cm10);}
    |   cm11:EXTERN    {Emit.EmitToken (cm11);}
    ;

indexer_declarator[bool mod]
{
    string [] tp = new string[] {"",""};
    string [] tn = new string[] {"",""};
}
    :   (type   THIS)=> 
        tp = type 
        {
            if(!mod)
                Emit.EmitString(tp[0]);
        }
        t1:THIS        {Emit.EmitString (ExtendedToken.getWhitespaces(t1) + "Item");}
        lb1:LBRACK     {Emit.EmitToken (lb1);}
        formal_parameter_list   
        rb1:RBRACK     {Emit.EmitToken (rb1);}
        {
            if(mod)
                Emit.EmitString (" :" + tp[0] + tp[1]);
            else
                Emit.EmitString (" : " + tp[1]);
        }
    |   tp = type   
        {
            if(!mod)
                Emit.EmitString(tp[0]);
        }
        tn = type_name   {Emit.EmitString (tn[0] + tn[1]);}
        d:DOT            {Emit.EmitToken (d);}
        t2:THIS          {Emit.EmitString (ExtendedToken.getWhitespaces(t2) + "Item");}
        lb2:LBRACK       {Emit.EmitToken (lb2);}
        formal_parameter_list   
        rb2:RBRACK       {Emit.EmitToken (rb2);}        
        {
            if(mod)
                Emit.EmitString (" :" + tp[0] + tp[1]);
            else
                Emit.EmitString (" :" + tp[1]);
        }
    ;

operator_declaration
    :   (attributes)?   (operator_modifier)+   
        operator_declarator   
        operator_body
    ;

operator_modifier
    :   om1:PUBLIC   {Emit.EmitToken (om1);}
    |   om2:STATIC   {Emit.EmitToken (om2);}
    |   om3:EXTERN   {Emit.EmitToken (om3);}
    ;

operator_declarator
    :   (type   OPERATOR   overloadable_unary_operator   LPAREN   type   IDENTIFIER   RPAREN)=>
            unary_operator_declarator
    |   binary_operator_declarator
    |   conversion_operator_declarator
    ;

unary_operator_declarator
{
    string [] tp1 = new string []{"",""};
    string [] tp2 = new string []{"",""};
}
    :   tp1 = type        
        op:OPERATOR      {Emit.EmitString (ExtendedToken.getWhitespaces (op) + "@");}
        overloadable_unary_operator   
        lp:LPAREN        {Emit.EmitToken (lp);}
        tp2 = type       
        id:IDENTIFIER    {Emit.EmitToken (id);}
                         {Emit.EmitString (" : " + tp2[0] + tp2[1]);}
        rp:RPAREN        {Emit.EmitToken (rp);}
                         {Emit.EmitString (" : " + tp1[0] + tp1[1]);}
    ;

overloadable_unary_operator
    :   oun1:PLUS    {Emit.EmitToken (oun1);}
    |   oun2:MINUS   {Emit.EmitToken (oun2);}
    |   oun3:LNOT    {Emit.EmitToken (oun3);}
    |   oun4:BNOT    {Emit.EmitToken (oun4);}
    |   oun5:INC     {Emit.EmitToken (oun5);}
    |   oun6:DEC     {Emit.EmitToken (oun6);}
    |   oun7:TRUE    {Emit.EmitToken (oun7);}
    |   oun8:FALSE   {Emit.EmitToken (oun8);}
    ;

binary_operator_declarator
{
    string [] tp1 = new string []{"",""};
    string [] tp2 = new string []{"",""};
    string [] tp3 = new string []{"",""};
}
    :   tp1 = type   
        op:OPERATOR                  {Emit.EmitString (ExtendedToken.getWhitespaces (op) + "@");}
        overloadable_binary_operator   
        lp:LPAREN                    {Emit.EmitToken (lp);}
        tp2 = type           
        id1: IDENTIFIER              {Emit.EmitToken (id1);}
                                     {Emit.EmitString (" : " + tp2[0] + tp2[1]);}
        cm:COMMA                     {Emit.EmitToken (cm);}
        tp3 = type 
        id2:IDENTIFIER               {Emit.EmitToken (id2);}
                                     {Emit.EmitString (" : " + tp3[0] + tp3[1]);}
        rp:RPAREN                    {Emit.EmitToken (rp);}
                                     {Emit.EmitString (" : " + tp1[0] + tp1[1]);}
    ;

overloadable_binary_operator
    :   obn1:PLUS       {Emit.EmitToken (obn1);}
    |   obn2:MINUS      {Emit.EmitToken (obn2);}
    |   obn3:STAR       {Emit.EmitToken (obn3);}
    |   obn4:DIV        {Emit.EmitToken (obn4);}
    |   obn5:MOD        {Emit.EmitToken (obn5);}
    |   obn6:BAND       {Emit.EmitString ("%"); Emit.EmitToken (obn6);}
    |   obn7:BOR        {Emit.EmitString ("%"); Emit.EmitToken (obn7);}
    |   obn8:BXOR       {Emit.EmitString ("%"); Emit.EmitToken (obn8);}
    |   obn9:SL         {Emit.EmitToken (obn9);}
    |   obn10:SR        {Emit.EmitToken (obn10);}
    |   obn11:EQUAL     {Emit.EmitToken (obn11);}
    |   obn12:NOT_EQUAL {Emit.EmitToken (obn12);}
    |   obn13:GTHAN     {Emit.EmitToken (obn13);}
    |   obn14:LTHAN     {Emit.EmitToken (obn14);}
    |   obn15:LE        {Emit.EmitToken (obn15);}
    |   obn16:GE        {Emit.EmitToken (obn16);}
    ;

conversion_operator_declarator
    :   (IMPLICIT)=> IMPLICIT   OPERATOR   type   LPAREN   type   IDENTIFIER   RPAREN
    |   EXPLICIT   OPERATOR   type   LPAREN   type   IDENTIFIER   RPAREN
    ;

operator_body
{
    StatementTree t = new StatementTree();
}
    :   t = block {Emit.EmitString ( t.ToString () );}
    |   s:SEMI    {Emit.EmitToken (s);}
    ;

constructor_declaration
{
    string cd = "";    
}
    :   (attributes)?   (constructor_modifier)*   cd = constructor_declarator   constructor_body[cd]
    ;

constructor_modifier
    :   cm1:PUBLIC    {Emit.EmitToken(cm1);}
    |   cm2:PROTECTED {Emit.EmitToken(cm2);}
    |   cm3:INTERNAL  {Emit.EmitToken(cm3);}
    |   cm4:PRIVATE   {Emit.EmitToken(cm4);}
    |   cm5:EXTERN    {Emit.EmitToken(cm5);}
    ;

constructor_declarator
returns [string return_string]
{
    return_string ="";
}
    :   id:IDENTIFIER   
        {
            Emit.EmitString ( ((ExtendedToken)id).GetWhitespaces () + "this");           
        }
        lp:LPAREN   
        {Emit.EmitToken(lp);}

        (formal_parameter_list)?   

        rp:RPAREN   
        {Emit.EmitToken(rp);}

        (return_string = constructor_initializer)? 
    ;

constructor_initializer
returns [string return_string]
{
    string al = "";
    return_string ="";
}
    :   (COLON  BASE)=> c1:COLON b:BASE   lp1:LPAREN   (al=argument_list)?   rp1:RPAREN
        {
            return_string = ((ExtendedToken)c1).GetWhitespaces () + b.getText () + lp1.getText () + al + rp1.getText();
        }
    |   c2:COLON  t:THIS   lp2:LPAREN   (al=argument_list)?   rp2:RPAREN
        {
            return_string = ((ExtendedToken)c2).GetWhitespaces () + t.getText () + lp2.getText () + al + rp2.getText();
        }
    ;

constructor_body[string ctor_initializer]
{
    StatementTree t = new StatementTree();
    LinkedList a = new LinkedList ();
}
    :   lb:LBRACE    {a.Add (new StatementTree(lb));}
        
        {
            if(ctor_initializer != "")
                a.Add (new StatementTree(ctor_initializer + ";"));
        }

        (t = statement {a.Add (t);})*   

        rb:RBRACE    {a.Add (new StatementTree(rb));}
        {
            t = new StatementTree("BLOCK",a);
            Emit.EmitString( t.ToString ());
        }
    |   s:SEMI
        {
            if(ctor_initializer != "")
                Emit.EmitString( ((ExtendedToken)s).GetWhitespaces () + "{ " + ctor_initializer + " }");
            else
                Emit.EmitToken (s);
        }
    ;

static_constructor_declaration
    :   (attributes)?   
        static_constructor_modifiers   
        id:IDENTIFIER            {Emit.EmitString (ExtendedToken.getWhitespaces(id) + "this");}
        lp:LPAREN                {Emit.EmitToken (lp);}
        rp:RPAREN                {Emit.EmitToken (rp);}
        static_constructor_body
    ;

static_constructor_modifiers
    :   (STATIC)=> s1:STATIC {Emit.EmitToken (s1);} (e1:EXTERN {Emit.EmitToken (e1);})?
    |   (e2:EXTERN {Emit.EmitToken (e2);})?  s2:STATIC {Emit.EmitToken (s2);}
    ;

static_constructor_body
{
    StatementTree t = new StatementTree();
}
    :   t = block  {Emit.EmitString ( t.ToString () );}
    |   s:SEMI {Emit.EmitToken (s);}
    ;

destructor_declaration
    :   (attributes)?   (e:EXTERN {Emit.EmitToken (e);})?   
        b:BNOT            {Emit.EmitToken (b);}
        id:IDENTIFIER     {Emit.EmitString (ExtendedToken.getWhitespaces(id) + "this");}
        lp:LPAREN         {Emit.EmitToken (lp);}
        rp:RPAREN         {Emit.EmitToken (rp);}
        destructor_body
    ;

destructor_body
{
    StatementTree t = new StatementTree();
}
    :   t = block {Emit.EmitString ( t.ToString () );}
    |   s:SEMI    {Emit.EmitToken (s);}
    ;

//-------------
//C.2.8 Structs
//-------------

struct_declaration
    :   (attributes)?   (struct_modifier)*   
        s:STRUCT              {Emit.EmitToken(s);}
        id:IDENTIFIER         {Emit.EmitToken(id);}
        (struct_interfaces)?   
        struct_body   
        (sm:SEMI              {Emit.EmitToken (sm);})?
    ;

struct_modifier
    :   sm1:NEW       {Emit.EmitToken(sm1);}
    |   sm2:PUBLIC    {Emit.EmitToken(sm2);}
    |   sm3:PROTECTED {Emit.EmitToken(sm3);}
    |   sm4:INTERNAL  {Emit.EmitToken(sm4);}
    |   sm5:PRIVATE   {Emit.EmitToken(sm5);}
    ;

struct_interfaces
{
    string [] tp = new string []{"",""};
}
    :   c1:COLON        {Emit.EmitToken(c1);}
        tp = type_name  {Emit.EmitString(tp[0]+tp[1]);}
        (cm:COMMA   tp = type_name
            {Emit.EmitToken(cm);
             Emit.EmitString(tp[0]+tp[1]);}
        )*
    ;
struct_body
    :   lb:LBRACE    {Emit.EmitToken(lb);}

        (struct_member_declaration)*   

        rb:RBRACE    {Emit.EmitToken(rb);}
    ;

struct_member_declaration
    :   ((attributes)?   (constant_modifier)*   CONST)=> 
            constant_declaration
    |   ((attributes)?   (field_modifier)*   type   variable_declarator[false,"",""])=> 
            field_declaration
    |   ((attributes)?   (method_modifier)*   return_type   member_name   LPAREN)=> 
            method_declaration
    |   ((attributes)?   (property_modifier)*   type   member_name   LBRACE)=>
            property_declaration
    |   ((attributes)?   (event_modifier)*   EVENT )=>
            event_declaration
    |   (indexer_declaration)=>
            indexer_declaration
    |   ((attributes)?   (operator_modifier)+   operator_declarator   operator_body)=>
            operator_declaration
    |   (constructor_declaration)=>
            constructor_declaration
    |   ((attributes)?   static_constructor_modifiers)=>
            static_constructor_declaration
    |   type_declaration
    ;

//------------
//C.2.9 Arrays
//------------

array_initializer
returns [string return_string]
{
    return_string = "";
    string temp = "";
}
    :   (LBRACE   variable_initializer_list COMMA  RBRACE)=>
            lb1:LBRACE   temp = variable_initializer_list c:COMMA  rb1:RBRACE
        {
            return_string = ExtendedToken.getWhitespaces (lb1) + " array" + "[" + temp + ExtendedToken.getWhitespaces(c) + 
            ExtendedToken.getWhitespaces(rb1) + "]";
        }
    |   lb2:LBRACE   (temp = variable_initializer_list)?  rb2:RBRACE
        {
            return_string = ExtendedToken.getWhitespaces (lb2) + " array" + "[" + temp +
            ExtendedToken.getWhitespaces(rb2) + "]";;
        }
    ;

variable_initializer_list
returns [string return_string]
{
    return_string = "";
    string temp = "";
}
    :   return_string = variable_initializer   
        (options {greedy=true;}:c:COMMA   temp=variable_initializer
            {
                return_string += (c.getText () + temp);
            } 
        )*
    ;

//-----------------
//C.2.10 Interfaces
//-----------------

interface_declaration
    :   (attributes)?   (interface_modifier)*   
        i:INTERFACE  id:IDENTIFIER   
        {
            Emit.EmitToken (i);
            Emit.EmitToken (id);
        }
        (interface_base)?   interface_body   (s:SEMI {Emit.EmitToken (s);})?
    ;

interface_modifier
    :   im1:NEW       {Emit.EmitToken(im1);}
    |   im2:PUBLIC    {Emit.EmitToken(im2);}
    |   im3:PROTECTED {Emit.EmitToken(im3);}
    |   im4:INTERNAL  {Emit.EmitToken(im4);}
    |   im5:PRIVATE   {Emit.EmitToken(im5);}
    ;
    
interface_base
{
    string [] t1 = new string[]{"",""};
}
    :   c1:COLON 
        {
            Emit.EmitToken (c1);
        }  
        t1 = type_name 
        {
            Emit.EmitString (t1[0]+t1[1]);
        }
            (cm:COMMA  {Emit.EmitToken (cm);}  t1 = type_name {Emit.EmitString (t1[0]+t1[1]);})*
    ;

interface_body
    :   lb:LBRACE
        {
            Emit.EmitToken (lb);
        }
        
        (interface_member_declaration)*   
        
        rb:RBRACE
        {
            Emit.EmitToken (rb);
        }
    ;

interface_member_declaration
    :   ((attributes)?   (NEW)?   return_type   IDENTIFIER  LPAREN)=>
            interface_method_declaration
    |   ((attributes)?   (NEW)?   type   IDENTIFIER LBRACE)=>
            interface_property_declaration
    |   ((attributes)?   (NEW)?  EVENT)=>
            interface_event_declaration
    |   interface_indexer_declaration
    ;

interface_method_declaration
{
    string [] tp = new string []{"",""};
}
    :   (attributes)?   (n:NEW {Emit.EmitToken(n);})?   
        tp = return_type_array
                          {Emit.EmitString(tp[0]);}
        id:IDENTIFIER     {Emit.EmitToken(id);} 
        lp:LPAREN         {Emit.EmitToken(lp);}
        (formal_parameter_list)?   
        rp:RPAREN         {Emit.EmitToken(rp);}
                          {Emit.EmitString(" : " + tp[1]);}
        s:SEMI            {Emit.EmitToken(s);}
    ;

interface_property_declaration
{
    string [] tp = new string []{"",""};
}
    :   (attributes)?   (n:NEW {Emit.EmitToken(n);})?   
        tp = type            {Emit.EmitString(tp[0]);}
        id:IDENTIFIER        {Emit.EmitToken(id);} 
                             {Emit.EmitString(" : " + tp[1]);}
        lb:LBRACE            {Emit.EmitToken(lb);} 
        interface_accessors   
        rb:RBRACE            {Emit.EmitToken(rb);} 
    ;

interface_accessors
    :   ((attributes)?   GET   SEMI   (attributes)?   SET   SEMI)=>
            (attributes)?   
            get1:GET       {Emit.EmitToken(get1);} 
            s11:SEMI       {Emit.EmitToken(s11);} 
            (attributes)?   
            set1:SET       {Emit.EmitToken(set1);} 
            s21:SEMI       {Emit.EmitToken(s21);} 
    |   ((attributes)?   SET   SEMI   (attributes)?   GET   SEMI)=>
            (attributes)?   
            set2:SET       {Emit.EmitToken(set2);} 
            s12:SEMI       {Emit.EmitToken(s12);} 
            (attributes)?   
            get2:GET       {Emit.EmitToken(get2);} 
            s22:SEMI       {Emit.EmitToken(s22);} 
    |   ((attributes)?   SET)=>
            (attributes)?   
            set3:SET       {Emit.EmitToken(set3);} 
            s3:SEMI        {Emit.EmitToken(s3);} 
    |   (attributes)?   
        get4:GET           {Emit.EmitToken(get4);} 
        s4:SEMI            {Emit.EmitToken(s4);} 
    ;

interface_event_declaration
{
    string [] tp = new string []{"",""};
}
    :   (attributes)?   (n:NEW {Emit.EmitToken(n);})?  
        e:EVENT           {Emit.EmitToken(e);} 
        tp = type         
        id:IDENTIFIER     {Emit.EmitToken(id);} 
                          {Emit.EmitString(" : " + tp[0] + tp[1] );}  
        s:SEMI            {Emit.EmitToken(s);} 
    ;

interface_indexer_declaration
{
    string [] tp = new string []{"",""};
}
    :   (attributes)?   (n:NEW {Emit.EmitToken(n);})?  
        tp = type              {Emit.EmitString(tp[0]);}
        t:THIS                 {Emit.EmitString(ExtendedToken.getWhitespaces (t) + "Item");}
        lb:LBRACK              {Emit.EmitToken(lb);} 
        formal_parameter_list  
        rb:RBRACK              {Emit.EmitToken(rb);} 
                               {Emit.EmitString(" : " + tp[1]);}  
        l:LBRACE               {Emit.EmitToken(l);} 
        interface_accessors   
        r:RBRACE               {Emit.EmitToken(r);} 
    ;

//------------
//C.2.11 Enums
//------------

enum_declaration
    :   (attributes)?   
        (enum_modifier)*   
        e:ENUM         {Emit.EmitToken(e);} 
        id:IDENTIFIER  {Emit.EmitToken(id);} 
        (enum_base)?   
        enum_body   
        (s:SEMI {Emit.EmitToken(s);} )?
    ;

enum_base
{
    string [] tp = new string [] {"",""};
}
    :   c:COLON tp = integral_type
        {
            Emit.EmitToken(c);
            Emit.EmitString(tp[0]+tp[1]);
        }
    ;

enum_body
    :   (LBRACE   enum_member_declarations   COMMA)=>
            lb:LBRACE   {Emit.EmitToken(lb);} 
            enum_member_declarations   
            c:COMMA     {Emit.EmitString (ExtendedToken.getWhitespaces (c));}
            rb:RBRACE   {Emit.EmitToken(rb);} 
    |   (LBRACE   (enum_member_declarations)?   RBRACE) =>
            lb1:LBRACE   {Emit.EmitToken(lb1);} 
            enum_member_declarations   
            rb1:RBRACE   {Emit.EmitToken(rb1);} 
    ;

enum_modifier
    :   em1:NEW          {Emit.EmitToken(em1);} 
    |   em2:PUBLIC       {Emit.EmitToken(em2);} 
    |   em3:PROTECTED    {Emit.EmitToken(em3);} 
    |   em4:INTERNAL     {Emit.EmitToken(em4);} 
    |   em5:PRIVATE      {Emit.EmitToken(em5);} 
    ;
    
enum_member_declarations
    :   enum_member_declaration  (options {greedy=true;}: c:COMMA {Emit.EmitString (ExtendedToken.getWhitespaces (c));}  
            enum_member_declaration)*
    ;

enum_member_declaration
{
    string ce = "";
}
    :   (attributes)?   
        id:IDENTIFIER   
        { Emit.EmitString (ExtendedToken.getWhitespaces (id) + "| " + ExtendedToken.getTextOnly (id)) ;}
        (a:ASSIGN   ce = constant_expression
            {
                Emit.EmitToken (a);
                Emit.EmitString (ce);
            }
        )?
    ;

//----------------
//C.2.12 Delegates
//----------------

delegate_declaration
{
    string rt = "";
}
    :   (attributes)?   (delegate_modifier)*   
        d:DELEGATE               {Emit.EmitToken(d);} 
        rt = return_type        
        id:IDENTIFIER            {Emit.EmitToken(id);} 
        lp:LPAREN                {Emit.EmitToken(lp);} 
        (formal_parameter_list)? 
        rp:RPAREN                {Emit.EmitToken(rp);} 
                                 {Emit.EmitString(" : " + rt);}
        s:SEMI                   {Emit.EmitToken(s);} 
    ;


delegate_modifier
    :   dm1:NEW          {Emit.EmitToken(dm1);}
    |   dm2:PUBLIC       {Emit.EmitToken(dm2);}
    |   dm3:PROTECTED    {Emit.EmitToken(dm3);}
    |   dm4:INTERNAL     {Emit.EmitToken(dm4);}
    |   dm5:PRIVATE      {Emit.EmitToken(dm5);}
    ;

//-----------------
//C.2.13 Attributes
//-----------------

global_attributes
    :  (options {greedy=true;}:global_attribute_section)+
    ;

global_attribute_section
    :   (LBRACK   global_attribute_target_specifier   attribute_list   COMMA)=>
        LBRACK   global_attribute_target_specifier   attribute_list   COMMA RBRACK
    |   LBRACK   global_attribute_target_specifier   attribute_list   RBRACK
    ;

global_attribute_target_specifier
    :   idgat:IDENTIFIER {idgat.getText()=="assembly" || idgat.getText()=="module"}?  COLON
    ;

attributes
    :  (attribute_section)+
    ;

attribute_section
    :   LBRACK   (attribute_target_specifier)?   attribute_list   (COMMA)? RBRACK
    ;

attribute_target_specifier
    :   attribute_target   COLON
    ;

attribute_target
    :   idat:IDENTIFIER 
       {idat.getText()=="field"    || //FIELD
        idat.getText()=="event"    || //EVENT
        idat.getText()=="method"   || //METHOD
        idat.getText()=="param"    || //PARAM
        idat.getText()=="property" || //PROPERTY
        idat.getText()=="return"   || //RETURN
        idat.getText()=="type" }?     //TYPE
    ;

attribute_list
    :   attribute (options {greedy=true;}: COMMA attribute)* 
    ;

attribute
    :   attribute_name (attribute_arguments)?
    ;

attribute_name
    :   type_name
    ;

attribute_arguments
    :   (LPAREN   positional_argument_list   COMMA   named_argument_list   RPAREN)=>
            LPAREN   positional_argument_list   COMMA   named_argument_list   RPAREN
    |   (LPAREN   named_argument_list   RPAREN)=>
            LPAREN   named_argument_list   RPAREN
    |   LPAREN   (positional_argument_list)?   RPAREN
    ;

positional_argument_list
    :   attribute_argument_expression (options {greedy=true;}: COMMA attribute_argument_expression)*
    ;

named_argument_list
    :   IDENTIFIER   ASSIGN   attribute_argument_expression   (COMMA   IDENTIFIER   ASSIGN   attribute_argument_expression)*
    ;

attribute_argument_expression
    :   expression
    ;


/* ************************************************************************* */
/*                                                                           */
/* LEXER                                                                     */
/*                                                                           */
/* ************************************************************************* */

class CSharpLexer extends Lexer;

options 
{
    k=4;
}

//--------------
//C.1.7 Keywords
//--------------

tokens
{
    ABSTRACT    =   "abstract";         LONG        =   "long";
    AS          =   "as";               NAMESPACE   =   "namespace";
    BASE        =   "base";             NEW         =   "new";
    BOOL        =   "bool";             NULL        =   "null";
    BREAK       =   "break";            OBJECT      =   "object";
    BYTE        =   "byte";             OPERATOR    =   "operator";
    CASE        =   "case";             OUT         =   "out";
    CATCH       =   "catch";            OVERRIDE    =   "override";
    CHAR        =   "char";             PARAMS      =   "params";
    CHECKED     =   "checked";          PRIVATE     =   "private";
    CLASS       =   "class";            PROTECTED   =   "protected";
    CONST       =   "const";            PUBLIC      =   "public";
    CONTINUE    =   "continue";         READONLY    =   "readonly";
    DECIMAL     =   "decimal";          REF         =   "ref";
    DEFAULT     =   "default";          RETURN      =   "return";
    DELEGATE    =   "delegate";         SBYTE       =   "sbyte";
    DO          =   "do";               SEALED      =   "sealed";
    DOUBLE      =   "double";           SHORT       =   "short";
    ELSE        =   "else";             SIZEOF      =   "sizeof";
    ENUM        =   "enum";             STACKALLOC  =   "stackalloc";
    EVENT       =   "event";            STATIC      =   "static";
    EXPLICIT    =   "explicit";         STRING      =   "string";
    EXTERN      =   "extern";           STRUCT      =   "struct";
    FALSE       =   "false";            SWITCH      =   "switch";
    FINALLY     =   "finally";          THIS        =   "this";
    FIXED       =   "fixed";            THROW       =   "throw";
    FLOAT       =   "float";            TRUE        =   "true";
    FOR         =   "for";              TRY         =   "try";
    FOREACH     =   "foreach";          TYPEOF      =   "typeof";
    GOTO        =   "goto";             UINT        =   "uint";
    IF          =   "if";               ULONG       =   "ulong";
    IMPLICIT    =   "implicit";         UNCHECKED   =   "unchecked";
    IN          =   "in";               UNSAFE      =   "unsafe";
    INT         =   "int";              USHORT      =   "ushort";
    INTERFACE   =   "interface";        USING       =   "using";
    INTERNAL    =   "internal";         VIRTUAL     =   "virtual";
    IS          =   "is";               VOID        =   "void";
    LOCK        =   "lock";             WHILE       =   "while";
    GET         =   "get";              SET         =   "set";
}

//----------------------
//C.1.1 Line terminators
//----------------------

protected
NEW_LINE
    :   ('\u000D' '\u000A')=> '\u000D' '\u000A' {newline();}
    |   '\u000D' {newline();}
    |   '\u000A' {newline();}
    |   '\u2028' {newline();}
    |   '\u2029' {newline();}
    ;

//-----------------
//C.1.2 White space
//-----------------

WHITESPACE
    :   (   ' ' 
        |   '\u0009'
        |   '\u000B'
        |   '\u000C'
        |   NEW_LINE
        )+ { 
            _ttype = Token.SKIP;
            ExtendedToken.AddToWhitespaces ($getText) ;            
           }
    ;

protected
NEW_LINE_CHARACTER
    :   ('\u000D' | '\u000A' | '\u2028' | '\u2029')
    ;
    
protected
NOT_NEW_LINE
    :   ~( '\u000D' | '\u000A' | '\u2028' | '\u2029')
    ;

//--------------
//C.1.3 Comments
//--------------

SINGLE_LINE_COMMENT
    :   "//"  (NOT_NEW_LINE)*  (NEW_LINE) 
        { 
            _ttype = Token.SKIP;
            ExtendedToken.AddToWhitespaces ($getText);
        }
    ;

DELIMITED_COMMENT
    :   "/*"  
        (   { LA(2)!='/' }? '*'
        |   NEW_LINE 
        |   ~('*'|'\u000D'|'\u000A'|'\u2028'|'\u2029')
        )*
        "*/" 
        {
            _ttype = Token.SKIP;
            ExtendedToken.AddToWhitespaces ($getText);
        }
    ; 
//----------------------------------------
//C.1.5 Unicode character escape sequences
//----------------------------------------

UNICODE_ESCAPE_SEQUENCE
    :   ('\\' 'u'   HEX_DIGIT   HEX_DIGIT   HEX_DIGIT  HEX_DIGIT)
    |   ('\\' 'U'   HEX_DIGIT   HEX_DIGIT   HEX_DIGIT  HEX_DIGIT  
                    HEX_DIGIT   HEX_DIGIT   HEX_DIGIT  HEX_DIGIT)
    ;
//-----------------
//C.1.6 Identifiers
//-----------------

IDENTIFIER
    :   IDENTIFIER_START_CHARACTER (IDENTIFIER_PART_CHARACTER)*
    |   '@'  IDENTIFIER_START_CHARACTER (IDENTIFIER_PART_CHARACTER)*
    ;
    
protected
IDENTIFIER_START_CHARACTER
    :   ('a'..'z'|'A'..'Z'|'_'|'$') 
    ;
    
protected
IDENTIFIER_PART_CHARACTER
    :   ('a'..'z'|'A'..'Z'|'_'|'0'..'9'|'$') 
    ;

//--------------
//C.1.8 Literals
//--------------

protected
DECIMAL_DIGIT
    :   '0' 
    |   '1' 
    |   '2' 
    |   '3' 
    |   '4' 
    |   '5' 
    |   '6' 
    |   '7' 
    |   '8' 
    |   '9'
    ;

HEXADECIMAL_INTEGER_LITERAL
    :   "0x"   (HEX_DIGIT)+   (INTEGER_TYPE_SUFFIX)?
    |   "0X"   (HEX_DIGIT)+   (INTEGER_TYPE_SUFFIX)?
    ;

protected
HEX_DIGIT
    :   '0' 
    |   '1' 
    |   '2' 
    |   '3' 
    |   '4' 
    |   '5' 
    |   '6' 
    |   '7'
    |   '8' 
    |   '9'
    |   'A'
    |   'B'
    |   'C'
    |   'D'
    |   'E'
    |   'F'
    |   'a'
    |   'b'
    |   'c'
    |   'd'
    |   'e'
    |   'f'
    ;

protected
INTEGER_TYPE_SUFFIX
    :   'U' 
    |   'u' 
    |   'L' 
    |   'l' 
    |   "UL" 
    |   "Ul" 
    |   "uL" 
    |   "ul" 
    |   "LU" 
    |   "Lu" 
    |   "lU" 
    |   "lu"
    ;

NUMERIC_LITERAL
    :   (".")=>   
            "." (DECIMAL_DIGIT)+   (EXPONENT_PART)?   (("F" | "f" | "D" | "d" | "M" | "m" ))? 
        {$setType(REAL_LITERAL);}
    |   ((DECIMAL_DIGIT)+   "." )=> 
            (DECIMAL_DIGIT)+   "."   (DECIMAL_DIGIT)+   (EXPONENT_PART)?   (("F" | "f" | "D" | "d" | "M" | "m" ))?
        {$setType(REAL_LITERAL);}
    |   ((DECIMAL_DIGIT)+   EXPONENT_PART)=> 
            (DECIMAL_DIGIT)+   EXPONENT_PART  (("F" | "f" | "D" | "d" | "M" | "m" ))?
        {$setType(REAL_LITERAL);}
    |   ((DECIMAL_DIGIT)+   ("F" | "f" | "D" | "d" | "M" | "m" ))=>
            (DECIMAL_DIGIT)+   ("F" | "f" | "D" | "d" | "M" | "m" )
        {$setType(REAL_LITERAL);}
    |   (DECIMAL_DIGIT)+   (INTEGER_TYPE_SUFFIX)? 
        {$setType(INTEGER_LITERAL);}
    |   '.'
        {$setType(DOT);}
    ;

protected
EXPONENT_PART
    :   ("e"   (SIGN)?   (DECIMAL_DIGIT)+)=>
            "e"   (SIGN)?   (DECIMAL_DIGIT)+
    |   "E"   (SIGN)?   (DECIMAL_DIGIT)+
    ;

protected
SIGN
    :   PLUS 
    |   MINUS
    ;

CHARACTER_LITERAL
    :   "'"   CHARACTER   "'"
    ;

protected
CHARACTER
    :   SIMPLE_CHARACTER
    |   SIMPLE_ESCAPE_SEQUENCE
    |   HEXADECIMAL_ESCAPE_SEQUENCE
    |   UNICODE_ESCAPE_SEQUENCE
    ;

protected
SIMPLE_CHARACTER
    :   ~( '\'' | '\\' | '\u000D' | '\u000A' | '\u2028' | '\u2029')
    ;

protected
SIMPLE_ESCAPE_SEQUENCE
    :   "\\'" | "\\\"" | "\\\\" | "\\0" | "\\a"  
    |   "\\b" | "\\f"  | "\\n"  | "\\r" | "\\t" 
    |   "\\v" ;

protected
HEXADECIMAL_ESCAPE_SEQUENCE
    :   ('\\' 'x' HEX_DIGIT)
        ( options {warnWhenFollowAmbig = false;}: 
        HEX_DIGIT 
            ( options {warnWhenFollowAmbig = false;}:
            HEX_DIGIT 
                ( options {warnWhenFollowAmbig = false;}:
                HEX_DIGIT
                )?
            )?
        )?
    ;

REGULAR_STRING_LITERAL
    :   '\"'   (REGULAR_STRING_LITERAL_CHARACTER)*   '\"'
    ;

protected
REGULAR_STRING_LITERAL_CHARACTER
    :   SINGLE_REGULAR_STRING_LITERAL_CHARCACTER
    |   SIMPLE_ESCAPE_SEQUENCE
    |   HEXADECIMAL_ESCAPE_SEQUENCE
    |   UNICODE_ESCAPE_SEQUENCE
    ;

protected
SINGLE_REGULAR_STRING_LITERAL_CHARCACTER
    :   ~( '\"' | '\\' | '\u000D' | '\u000A' | '\u2028' | '\u2029')
    ;

VERBATIM_STRING_LITERAL
{string s="";}
    :    '@' "\""   
        (   "\"\""              {s+=("\"");}
        |   "\\"                {s+=("\\\\");}
        |   ch:~('\"' | '\\')   {s+=(ch);}
        )* 
        "\""    
    ;

//-------------------------------
//C.1.9 Operators and punctuators
//-------------------------------

LBRACE      :   '{'     ;   RBRACE      :   '}'     ;
LBRACK      :   '['     ;   RBRACK      :   ']'     ;
LPAREN      :   '('     ;   RPAREN      :   ')'     ;


PLUS        :   '+'     ;   PLUS_ASN    :   "+="    ;   
MINUS       :   '-'     ;   MINUS_ASN   :   "-="    ;   
STAR        :   '*'     ;   STAR_ASN    :   "*="    ;
DIV         :   '/'     ;   DIV_ASN     :   "/="    ;
MOD         :   '%'     ;   MOD_ASN     :   "%="    ;
INC         :   "++"    ;   DEC         :   "--"    ;

SL          :   "<<"    ;   SL_ASN      :   "<<="   ;
SR          :   ">>"    ;   SR_ASN      :   ">>="   ;

BAND        :   '&'     ;   BAND_ASN    :   "&="    ;   
BOR         :   '|'     ;   BOR_ASN     :   "|="    ;   
BXOR        :   '^'     ;   BXOR_ASN    :   "^="    ;
BNOT        :   '~'     ;

ASSIGN      :   '='     ;   EQUAL       :   "=="    ;
LTHAN       :   '<'     ;   LE          :   "<="    ;
GTHAN       :   ">"     ;   GE          :   ">="    ;
LNOT        :   '!'     ;   NOT_EQUAL   :   "!="    ;
LOR         :   "||"    ;   LAND        :   "&&"    ;

COMMA       :   ','     ;   COLON       :   ':'     ;   
SEMI        :   ';'     ;   HASH        :   '#'     ;
QUOTE       :   "\""    ;   QUESTION    :   '?'     ;

//--------------------------------
//C.1.10 Pre-processing directives
//--------------------------------


PP_DIRECTIVE
    :   HASH (PP_WHITESPACE)?
        ( PP_DECLARATION
        |  (PP_END_REGION) => PP_END_REGION 
        |  PP_CONDITIONAL
        |  PP_LINE
        |  PP_DIAGNOSTIC
        |  PP_START_REGION )        
        { _ttype = Token.SKIP;}
    ;

protected   
PP_WHITESPACE
    :   ( options {greedy=true;}:  ' '
        |   '\u0009' 
        |   '\u000B' 
        |   '\u000C' 
        )+
        { _ttype = Token.SKIP; }
    ;

protected
PP_NEW_LINE
    :    ( SINGLE_LINE_COMMENT | NEW_LINE )
    ;


protected
PP_EXPRESSION
    :    PP_OR_EXPRESSION 
    ;

protected
PP_OR_EXPRESSION
    :   PP_AND_EXPRESSION  (options {greedy=true;}: (PP_WHITESPACE)?   LOR   (PP_WHITESPACE)?   PP_AND_EXPRESSION )*
    ;

protected
PP_AND_EXPRESSION
    :   PP_EQUALITY_EXPRESSION  (options {greedy=true;}: (PP_WHITESPACE)?   LAND   (PP_WHITESPACE)?   PP_EQUALITY_EXPRESSION )*
    ;

protected
PP_EQUALITY_EXPRESSION
    :   PP_UNARY_EXPRESSION  (options {greedy=true;}: (PP_WHITESPACE)?   EQUALITY_OP   (PP_WHITESPACE)?   PP_UNARY_EXPRESSION )*
    ;

protected
EQUALITY_OP
    :   EQUAL
    |   NOT_EQUAL
    ;

protected
PP_UNARY_EXPRESSION
    :   PP_PRIMARY_EXPRESSION
    |   LNOT   (PP_WHITESPACE)?   PP_UNARY_EXPRESSION
    ;


protected
PP_PRIMARY_EXPRESSION
    :   ("true")=> "true"
    |   ("false")=> "false"
    |   CONDITIONAL_SYMBOL
    |   LPAREN   (PP_WHITESPACE)?   PP_EXPRESSION   (PP_WHITESPACE)?   RPAREN
    ;

protected
PP_CONDITIONAL
    :   PP_IF_SECTION  
    |   PP_ELIF_SECTION
    |   PP_ELSE_SECTION
    |   PP_ENDIF
    ;

protected
PP_IF_SECTION
    :   "if"   PP_WHITESPACE   PP_EXPRESSION (PP_WHITESPACE)?  PP_NEW_LINE 
    ;

protected
PP_ELIF_SECTION
    :   "elif"   PP_WHITESPACE   PP_EXPRESSION  (PP_WHITESPACE)? PP_NEW_LINE 
    ;

protected
PP_ELSE_SECTION
    :    "else"  PP_NEW_LINE  
    ;

protected
PP_ENDIF
    :   "endif"   PP_NEW_LINE
    ;

protected
PP_DECLARATION
    :   "define"   PP_WHITESPACE   CONDITIONAL_SYMBOL   PP_NEW_LINE
    |   "undef"   PP_WHITESPACE   CONDITIONAL_SYMBOL   PP_NEW_LINE
    ;

protected
CONDITIONAL_SYMBOL
    :   IDENTIFIER
    ;

protected
PP_LINE
    :   "line"   PP_WHITESPACE   LINE_INDICATOR   PP_NEW_LINE
    ;

protected
LINE_INDICATOR
    :   (DECIMAL_DIGIT)+   ( PP_WHITESPACE   FILE_NAME )?
    |   "default"
    ;

protected
FILE_NAME
    :   "\""   (FILE_NAME_CHARACTER)+   "\""
    ;

protected
FILE_NAME_CHARACTER
    :   ~('\"')
    ;

protected
PP_DIAGNOSTIC
    :   "error"   PP_MESSAGE
    |   "warning"   PP_MESSAGE
    ;

protected
PP_START_REGION
    :   "region"   PP_MESSAGE
    ;

protected
PP_END_REGION
    :   "endregion"   PP_MESSAGE
    ;

protected
PP_MESSAGE
    :   (NOT_NEW_LINE)*   NEW_LINE
    ;

