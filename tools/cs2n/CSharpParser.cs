// $ANTLR 2.7.4: "csharpgrammar.g" -> "CSharpParser.cs"$

    using System.Collections;
    using Nemerle.Collections;

namespace Nemerle.CSharp
{
	// Generate the header common to all output files.
	using System;
	
	using TokenBuffer              = antlr.TokenBuffer;
	using TokenStreamException     = antlr.TokenStreamException;
	using TokenStreamIOException   = antlr.TokenStreamIOException;
	using ANTLRException           = antlr.ANTLRException;
	using LLkParser = antlr.LLkParser;
	using Token                    = antlr.Token;
	using TokenStream              = antlr.TokenStream;
	using RecognitionException     = antlr.RecognitionException;
	using NoViableAltException     = antlr.NoViableAltException;
	using MismatchedTokenException = antlr.MismatchedTokenException;
	using SemanticException        = antlr.SemanticException;
	using ParserSharedInputState   = antlr.ParserSharedInputState;
	using BitSet                   = antlr.collections.impl.BitSet;
	
	public 	class CSharpParser : antlr.LLkParser
	{
		public const int EOF = 1;
		public const int NULL_TREE_LOOKAHEAD = 3;
		public const int INTEGER_LITERAL = 4;
		public const int HEXADECIMAL_INTEGER_LITERAL = 5;
		public const int REAL_LITERAL = 6;
		public const int CHARACTER_LITERAL = 7;
		public const int NULL = 8;
		public const int TRUE = 9;
		public const int FALSE = 10;
		public const int REGULAR_STRING_LITERAL = 11;
		public const int VERBATIM_STRING_LITERAL = 12;
		public const int IDENTIFIER = 13;
		public const int DOT = 14;
		public const int OBJECT = 15;
		public const int STRING = 16;
		public const int BOOL = 17;
		public const int DECIMAL = 18;
		public const int CHAR = 19;
		public const int INT = 20;
		public const int LONG = 21;
		public const int SBYTE = 22;
		public const int BYTE = 23;
		public const int SHORT = 24;
		public const int UINT = 25;
		public const int ULONG = 26;
		public const int USHORT = 27;
		public const int FLOAT = 28;
		public const int DOUBLE = 29;
		public const int LBRACK = 30;
		public const int COMMA = 31;
		public const int RBRACK = 32;
		public const int REF = 33;
		public const int OUT = 34;
		public const int DEC = 35;
		public const int INC = 36;
		public const int NEW = 37;
		public const int LPAREN = 38;
		public const int RPAREN = 39;
		public const int THIS = 40;
		public const int BASE = 41;
		public const int TYPEOF = 42;
		public const int VOID = 43;
		public const int CHECKED = 44;
		public const int UNCHECKED = 45;
		public const int PLUS = 46;
		public const int MINUS = 47;
		public const int LNOT = 48;
		public const int BNOT = 49;
		public const int STAR = 50;
		public const int DIV = 51;
		public const int MOD = 52;
		public const int SL = 53;
		public const int SR = 54;
		public const int IS = 55;
		public const int AS = 56;
		public const int LTHAN = 57;
		public const int GTHAN = 58;
		public const int LE = 59;
		public const int GE = 60;
		public const int EQUAL = 61;
		public const int NOT_EQUAL = 62;
		public const int BAND = 63;
		public const int BXOR = 64;
		public const int BOR = 65;
		public const int LAND = 66;
		public const int LOR = 67;
		public const int QUESTION = 68;
		public const int COLON = 69;
		public const int ASSIGN = 70;
		public const int PLUS_ASN = 71;
		public const int MINUS_ASN = 72;
		public const int STAR_ASN = 73;
		public const int DIV_ASN = 74;
		public const int MOD_ASN = 75;
		public const int BAND_ASN = 76;
		public const int BOR_ASN = 77;
		public const int BXOR_ASN = 78;
		public const int SL_ASN = 79;
		public const int SR_ASN = 80;
		public const int CONST = 81;
		public const int SEMI = 82;
		public const int LBRACE = 83;
		public const int RBRACE = 84;
		public const int IF = 85;
		public const int ELSE = 86;
		public const int SWITCH = 87;
		public const int CASE = 88;
		public const int DEFAULT = 89;
		public const int WHILE = 90;
		public const int DO = 91;
		public const int FOR = 92;
		public const int FOREACH = 93;
		public const int IN = 94;
		public const int BREAK = 95;
		public const int CONTINUE = 96;
		public const int GOTO = 97;
		public const int RETURN = 98;
		public const int THROW = 99;
		public const int TRY = 100;
		public const int CATCH = 101;
		public const int FINALLY = 102;
		public const int LOCK = 103;
		public const int USING = 104;
		public const int NAMESPACE = 105;
		public const int ENUM = 106;
		public const int STRUCT = 107;
		public const int INTERFACE = 108;
		public const int CLASS = 109;
		public const int PUBLIC = 110;
		public const int PROTECTED = 111;
		public const int INTERNAL = 112;
		public const int PRIVATE = 113;
		public const int SEALED = 114;
		public const int ABSTRACT = 115;
		public const int STATIC = 116;
		public const int READONLY = 117;
		public const int VOLATILE = 118;
		public const int VIRTUAL = 119;
		public const int OVERRIDE = 120;
		public const int EXTERN = 121;
		public const int PARAMS = 122;
		public const int GET = 123;
		public const int SET = 124;
		public const int EVENT = 125;
		public const int ABSTARCT = 126;
		public const int ADD = 127;
		public const int OPERATOR = 128;
		public const int IMPLICIT = 129;
		public const int EXPLICIT = 130;
		public const int DELEGATE = 131;
		public const int SIZEOF = 132;
		public const int STACKALLOC = 133;
		public const int FIXED = 134;
		public const int UNSAFE = 135;
		public const int NEW_LINE = 136;
		public const int WHITESPACE = 137;
		public const int NEW_LINE_CHARACTER = 138;
		public const int NOT_NEW_LINE = 139;
		public const int SINGLE_LINE_COMMENT = 140;
		public const int DELIMITED_COMMENT = 141;
		public const int UNICODE_ESCAPE_SEQUENCE = 142;
		public const int IDENTIFIER_START_CHARACTER = 143;
		public const int IDENTIFIER_PART_CHARACTER = 144;
		public const int DECIMAL_DIGIT = 145;
		public const int HEX_DIGIT = 146;
		public const int INTEGER_TYPE_SUFFIX = 147;
		public const int NUMERIC_LITERAL = 148;
		public const int EXPONENT_PART = 149;
		public const int SIGN = 150;
		public const int CHARACTER = 151;
		public const int SIMPLE_CHARACTER = 152;
		public const int SIMPLE_ESCAPE_SEQUENCE = 153;
		public const int HEXADECIMAL_ESCAPE_SEQUENCE = 154;
		public const int REGULAR_STRING_LITERAL_CHARACTER = 155;
		public const int SINGLE_REGULAR_STRING_LITERAL_CHARCACTER = 156;
		public const int HASH = 157;
		public const int QUOTE = 158;
		public const int PP_DIRECTIVE = 159;
		public const int PP_WHITESPACE = 160;
		public const int PP_NEW_LINE = 161;
		public const int PP_EXPRESSION = 162;
		public const int PP_OR_EXPRESSION = 163;
		public const int PP_AND_EXPRESSION = 164;
		public const int PP_EQUALITY_EXPRESSION = 165;
		public const int EQUALITY_OP = 166;
		public const int PP_UNARY_EXPRESSION = 167;
		public const int PP_PRIMARY_EXPRESSION = 168;
		public const int PP_CONDITIONAL = 169;
		public const int PP_IF_SECTION = 170;
		public const int PP_ELIF_SECTION = 171;
		public const int PP_ELSE_SECTION = 172;
		public const int PP_ENDIF = 173;
		public const int PP_DECLARATION = 174;
		public const int CONDITIONAL_SYMBOL = 175;
		public const int PP_LINE = 176;
		public const int LINE_INDICATOR = 177;
		public const int FILE_NAME = 178;
		public const int FILE_NAME_CHARACTER = 179;
		public const int PP_DIAGNOSTIC = 180;
		public const int PP_START_REGION = 181;
		public const int PP_END_REGION = 182;
		public const int PP_MESSAGE = 183;
		
		
		protected void initialize()
		{
			tokenNames = tokenNames_;
		}
		
		
		protected CSharpParser(TokenBuffer tokenBuf, int k) : base(tokenBuf, k)
		{
			initialize();
		}
		
		public CSharpParser(TokenBuffer tokenBuf) : this(tokenBuf,2)
		{
		}
		
		protected CSharpParser(TokenStream lexer, int k) : base(lexer,k)
		{
			initialize();
		}
		
		public CSharpParser(TokenStream lexer) : this(lexer,2)
		{
		}
		
		public CSharpParser(ParserSharedInputState state) : base(state,2)
		{
			initialize();
		}
		
	public string  literal() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  il = null;
		Token  hil = null;
		Token  rl = null;
		Token  chl = null;
		Token  n = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case TRUE:
			case FALSE:
			{
				return_string=boolean_literal();
				break;
			}
			case INTEGER_LITERAL:
			{
				il = LT(1);
				match(INTEGER_LITERAL);
				if (0==inputState.guessing)
				{
					return_string = il.getText();
				}
				break;
			}
			case HEXADECIMAL_INTEGER_LITERAL:
			{
				hil = LT(1);
				match(HEXADECIMAL_INTEGER_LITERAL);
				if (0==inputState.guessing)
				{
					return_string = hil.getText();
				}
				break;
			}
			case REAL_LITERAL:
			{
				rl = LT(1);
				match(REAL_LITERAL);
				if (0==inputState.guessing)
				{
					return_string = rl.getText();
				}
				break;
			}
			case CHARACTER_LITERAL:
			{
				chl = LT(1);
				match(CHARACTER_LITERAL);
				if (0==inputState.guessing)
				{
					return_string = chl.getText();
				}
				break;
			}
			case REGULAR_STRING_LITERAL:
			case VERBATIM_STRING_LITERAL:
			{
				return_string=string_literal();
				break;
			}
			case NULL:
			{
				n = LT(1);
				match(NULL);
				if (0==inputState.guessing)
				{
					return_string = n.getText();
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  boolean_literal() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  t = null;
		Token  f = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case TRUE:
			{
				t = LT(1);
				match(TRUE);
				if (0==inputState.guessing)
				{
					return_string = t.getText();
				}
				break;
			}
			case FALSE:
			{
				f = LT(1);
				match(FALSE);
				if (0==inputState.guessing)
				{
					return_string = f.getText();
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  string_literal() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  rsl = null;
		Token  vsl = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case REGULAR_STRING_LITERAL:
			{
				rsl = LT(1);
				match(REGULAR_STRING_LITERAL);
				if (0==inputState.guessing)
				{
					return_string = rsl.getText();
				}
				break;
			}
			case VERBATIM_STRING_LITERAL:
			{
				vsl = LT(1);
				match(VERBATIM_STRING_LITERAL);
				if (0==inputState.guessing)
				{
					return_string = vsl.getText();
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  namespace_name() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			return_string=namespace_or_type_name();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_1_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  namespace_or_type_name() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  id1 = null;
		Token  id2 = null;
		
		return_string = "";
		
		
		try {      // for error handling
			id1 = LT(1);
			match(IDENTIFIER);
			if (0==inputState.guessing)
			{
				return_string = id1.getText();
			}
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==DOT) && (LA(2)==IDENTIFIER))
					{
						match(DOT);
						id2 = LT(1);
						match(IDENTIFIER);
						if (0==inputState.guessing)
						{
							return_string += ("." + id2.getText());
						}
					}
					else
					{
						goto _loop8_breakloop;
					}
					
				}
_loop8_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_2_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  type_name() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			return_string=namespace_or_type_name();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_2_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  type() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  o = null;
		Token  s = null;
		
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched11 = false;
			if (((tokenSet_3_.member(LA(1))) && (LA(2)==DOT||LA(2)==LBRACK)))
			{
				int _m11 = mark();
				synPredMatched11 = true;
				inputState.guessing++;
				try {
					{
						array_type();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched11 = false;
				}
				rewind(_m11);
				inputState.guessing--;
			}
			if ( synPredMatched11 )
			{
				return_string=array_type();
			}
			else if ((LA(1)==OBJECT) && (tokenSet_4_.member(LA(2)))) {
				o = LT(1);
				match(OBJECT);
				if (0==inputState.guessing)
				{
					return_string = o.getText();
				}
			}
			else if ((LA(1)==STRING) && (tokenSet_4_.member(LA(2)))) {
				s = LT(1);
				match(STRING);
				if (0==inputState.guessing)
				{
					return_string = s.getText();
				}
			}
			else if ((LA(1)==IDENTIFIER) && (tokenSet_5_.member(LA(2)))) {
				return_string=type_name();
			}
			else if (((LA(1) >= BOOL && LA(1) <= DOUBLE)) && (tokenSet_4_.member(LA(2)))) {
				return_string=simple_type();
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_4_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  array_type() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		int rank = 1;
		return_string = "";
		
		
		try {      // for error handling
			non_array_type();
			rank=rank_specifier();
			if (0==inputState.guessing)
			{
				return_string = " array <" + rank.ToString () + ">";
			}
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==LBRACK))
					{
						rank=rank_specifier();
					}
					else
					{
						goto _loop18_breakloop;
					}
					
				}
_loop18_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_6_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  simple_type() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  b = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case DECIMAL:
			case CHAR:
			case INT:
			case LONG:
			case SBYTE:
			case BYTE:
			case SHORT:
			case UINT:
			case ULONG:
			case USHORT:
			case FLOAT:
			case DOUBLE:
			{
				return_string=numeric_type();
				break;
			}
			case BOOL:
			{
				b = LT(1);
				match(BOOL);
				if (0==inputState.guessing)
				{
					return_string = b.getText();
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_7_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  numeric_type() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  d = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case CHAR:
			case INT:
			case LONG:
			case SBYTE:
			case BYTE:
			case SHORT:
			case UINT:
			case ULONG:
			case USHORT:
			{
				return_string=integral_type();
				break;
			}
			case FLOAT:
			case DOUBLE:
			{
				return_string=floating_point_type();
				break;
			}
			case DECIMAL:
			{
				d = LT(1);
				match(DECIMAL);
				if (0==inputState.guessing)
				{
					return_string = d.getText();
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_7_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  integral_type() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  i1 = null;
		Token  i2 = null;
		Token  i3 = null;
		Token  i4 = null;
		Token  i5 = null;
		Token  i6 = null;
		Token  i7 = null;
		Token  i8 = null;
		Token  i9 = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case CHAR:
			{
				i1 = LT(1);
				match(CHAR);
				if (0==inputState.guessing)
				{
					return_string = i1.getText();
				}
				break;
			}
			case INT:
			{
				i2 = LT(1);
				match(INT);
				if (0==inputState.guessing)
				{
					return_string = i2.getText();
				}
				break;
			}
			case LONG:
			{
				i3 = LT(1);
				match(LONG);
				if (0==inputState.guessing)
				{
					return_string = i3.getText();
				}
				break;
			}
			case SBYTE:
			{
				i4 = LT(1);
				match(SBYTE);
				if (0==inputState.guessing)
				{
					return_string = i4.getText();
				}
				break;
			}
			case BYTE:
			{
				i5 = LT(1);
				match(BYTE);
				if (0==inputState.guessing)
				{
					return_string = i5.getText();
				}
				break;
			}
			case SHORT:
			{
				i6 = LT(1);
				match(SHORT);
				if (0==inputState.guessing)
				{
					return_string = i6.getText();
				}
				break;
			}
			case UINT:
			{
				i7 = LT(1);
				match(UINT);
				if (0==inputState.guessing)
				{
					return_string = i7.getText();
				}
				break;
			}
			case ULONG:
			{
				i8 = LT(1);
				match(ULONG);
				if (0==inputState.guessing)
				{
					return_string = i8.getText();
				}
				break;
			}
			case USHORT:
			{
				i9 = LT(1);
				match(USHORT);
				if (0==inputState.guessing)
				{
					return_string = i9.getText();
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_8_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  floating_point_type() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  f = null;
		Token  d = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case FLOAT:
			{
				f = LT(1);
				match(FLOAT);
				if (0==inputState.guessing)
				{
					return_string = f.getText();
				}
				break;
			}
			case DOUBLE:
			{
				d = LT(1);
				match(DOUBLE);
				if (0==inputState.guessing)
				{
					return_string = d.getText();
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_7_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void non_array_type() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case OBJECT:
			{
				match(OBJECT);
				break;
			}
			case STRING:
			{
				match(STRING);
				break;
			}
			case IDENTIFIER:
			{
				type_name();
				break;
			}
			case BOOL:
			case DECIMAL:
			case CHAR:
			case INT:
			case LONG:
			case SBYTE:
			case BYTE:
			case SHORT:
			case UINT:
			case ULONG:
			case USHORT:
			case FLOAT:
			case DOUBLE:
			{
				simple_type();
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_9_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public int  rank_specifier() //throws RecognitionException, TokenStreamException
{
		int rank;
		
		
		rank = 1;
		
		
		try {      // for error handling
			match(LBRACK);
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						match(COMMA);
						if (0==inputState.guessing)
						{
							rank += 1;
						}
					}
					else
					{
						goto _loop22_breakloop;
					}
					
				}
_loop22_breakloop:				;
			}    // ( ... )*
			match(RBRACK);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_10_);
			}
			else
			{
				throw;
			}
		}
		return rank;
	}
	
	public string  variable_reference() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			return_string=expression();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_11_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched123 = false;
			if (((tokenSet_12_.member(LA(1))) && (tokenSet_13_.member(LA(2)))))
			{
				int _m123 = mark();
				synPredMatched123 = true;
				inputState.guessing++;
				try {
					{
						conditional_expression();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched123 = false;
				}
				rewind(_m123);
				inputState.guessing--;
			}
			if ( synPredMatched123 )
			{
				return_string=conditional_expression();
			}
			else if ((tokenSet_12_.member(LA(1))) && (tokenSet_14_.member(LA(2)))) {
				return_string=assignment();
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_15_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  argument_list() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  c = null;
		
		string ar = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=argument();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						c = LT(1);
						match(COMMA);
						ar=argument();
						if (0==inputState.guessing)
						{
							
							return_string += (c.getText () + ar);
							
						}
					}
					else
					{
						goto _loop26_breakloop;
					}
					
				}
_loop26_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_16_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  argument() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  r = null;
		Token  o = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case INTEGER_LITERAL:
			case HEXADECIMAL_INTEGER_LITERAL:
			case REAL_LITERAL:
			case CHARACTER_LITERAL:
			case NULL:
			case TRUE:
			case FALSE:
			case REGULAR_STRING_LITERAL:
			case VERBATIM_STRING_LITERAL:
			case IDENTIFIER:
			case OBJECT:
			case STRING:
			case DECIMAL:
			case CHAR:
			case INT:
			case LONG:
			case SBYTE:
			case BYTE:
			case SHORT:
			case UINT:
			case ULONG:
			case USHORT:
			case FLOAT:
			case DOUBLE:
			case DEC:
			case INC:
			case NEW:
			case LPAREN:
			case THIS:
			case BASE:
			case TYPEOF:
			case CHECKED:
			case UNCHECKED:
			case PLUS:
			case MINUS:
			case LNOT:
			case BNOT:
			case STAR:
			{
				return_string=expression();
				break;
			}
			case REF:
			{
				r = LT(1);
				match(REF);
				return_string=variable_reference();
				if (0==inputState.guessing)
				{
					return_string = r.getText () + return_string ;
				}
				break;
			}
			case OUT:
			{
				o = LT(1);
				match(OUT);
				return_string=variable_reference();
				if (0==inputState.guessing)
				{
					return_string = o.getText () + return_string ;
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_11_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  primary_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		string sope = "";
		string soace = "";
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched30 = false;
			if (((LA(1)==NEW) && (tokenSet_3_.member(LA(2)))))
			{
				int _m30 = mark();
				synPredMatched30 = true;
				inputState.guessing++;
				try {
					{
						array_creation_expression();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched30 = false;
				}
				rewind(_m30);
				inputState.guessing--;
			}
			if ( synPredMatched30 )
			{
				return_string=array_creation_expression();
				{    // ( ... )*
					for (;;)
					{
						if ((tokenSet_17_.member(LA(1))))
						{
							soace=suffix_of_array_creation_expression();
							if (0==inputState.guessing)
							{
								return_string += soace;
							}
						}
						else
						{
							goto _loop32_breakloop;
						}
						
					}
_loop32_breakloop:					;
				}    // ( ... )*
			}
			else if ((tokenSet_18_.member(LA(1))) && (tokenSet_19_.member(LA(2)))) {
				return_string=primary_no_array_creation_expression();
				{    // ( ... )*
					for (;;)
					{
						if ((tokenSet_20_.member(LA(1))))
						{
							sope=suffix_of_primary_expression();
							if (0==inputState.guessing)
							{
								return_string += sope;
							}
						}
						else
						{
							goto _loop34_breakloop;
						}
						
					}
_loop34_breakloop:					;
				}    // ( ... )*
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_21_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  array_creation_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		string at = "";
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched59 = false;
			if (((LA(1)==NEW) && (tokenSet_3_.member(LA(2)))))
			{
				int _m59 = mark();
				synPredMatched59 = true;
				inputState.guessing++;
				try {
					{
						match(NEW);
						array_type();
						array_initializer();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched59 = false;
				}
				rewind(_m59);
				inputState.guessing--;
			}
			if ( synPredMatched59 )
			{
				match(NEW);
				at=array_type();
				array_initializer();
			}
			else if ((LA(1)==NEW) && (tokenSet_3_.member(LA(2)))) {
				match(NEW);
				non_array_type();
				match(LBRACK);
				expression_list();
				match(RBRACK);
				{    // ( ... )*
					for (;;)
					{
						if ((LA(1)==LBRACK))
						{
							rank_specifier();
						}
						else
						{
							goto _loop61_breakloop;
						}
						
					}
_loop61_breakloop:					;
				}    // ( ... )*
				{
					switch ( LA(1) )
					{
					case LBRACE:
					{
						array_initializer();
						break;
					}
					case DOT:
					case COMMA:
					case RBRACK:
					case DEC:
					case INC:
					case LPAREN:
					case RPAREN:
					case PLUS:
					case MINUS:
					case STAR:
					case DIV:
					case MOD:
					case SL:
					case SR:
					case IS:
					case AS:
					case LTHAN:
					case GTHAN:
					case LE:
					case GE:
					case EQUAL:
					case NOT_EQUAL:
					case BAND:
					case BXOR:
					case BOR:
					case LAND:
					case LOR:
					case QUESTION:
					case COLON:
					case ASSIGN:
					case PLUS_ASN:
					case MINUS_ASN:
					case STAR_ASN:
					case DIV_ASN:
					case MOD_ASN:
					case BAND_ASN:
					case BOR_ASN:
					case BXOR_ASN:
					case SL_ASN:
					case SR_ASN:
					case SEMI:
					case RBRACE:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_22_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  suffix_of_array_creation_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  d = null;
		Token  i = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case DOT:
			{
				return_string=member_access_end();
				break;
			}
			case DEC:
			{
				d = LT(1);
				match(DEC);
				if (0==inputState.guessing)
				{
					return_string = d.getText ();
				}
				break;
			}
			case INC:
			{
				i = LT(1);
				match(INC);
				if (0==inputState.guessing)
				{
					return_string = i.getText ();
				}
				break;
			}
			case LPAREN:
			{
				return_string=invocation_expression_end();
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  primary_no_array_creation_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  id = null;
		
		string pt = "";
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case INTEGER_LITERAL:
			case HEXADECIMAL_INTEGER_LITERAL:
			case REAL_LITERAL:
			case CHARACTER_LITERAL:
			case NULL:
			case TRUE:
			case FALSE:
			case REGULAR_STRING_LITERAL:
			case VERBATIM_STRING_LITERAL:
			{
				return_string=literal();
				break;
			}
			case IDENTIFIER:
			{
				return_string=simple_name();
				break;
			}
			case LPAREN:
			{
				return_string=parenthesized_expression();
				break;
			}
			case THIS:
			{
				return_string=this_access();
				break;
			}
			case BASE:
			{
				return_string=base_access();
				break;
			}
			case OBJECT:
			case STRING:
			case DECIMAL:
			case CHAR:
			case INT:
			case LONG:
			case SBYTE:
			case BYTE:
			case SHORT:
			case UINT:
			case ULONG:
			case USHORT:
			case FLOAT:
			case DOUBLE:
			{
				pt=predefined_type();
				match(DOT);
				id = LT(1);
				match(IDENTIFIER);
				if (0==inputState.guessing)
				{
					return_string = pt + "." + id.getText ();
				}
				break;
			}
			case TYPEOF:
			{
				return_string=typeof_expression();
				break;
			}
			case CHECKED:
			{
				return_string=checked_expression();
				break;
			}
			case UNCHECKED:
			{
				return_string=unchecked_expression();
				break;
			}
			default:
				bool synPredMatched39 = false;
				if (((LA(1)==NEW) && (tokenSet_3_.member(LA(2)))))
				{
					int _m39 = mark();
					synPredMatched39 = true;
					inputState.guessing++;
					try {
						{
							object_creation_expression();
						}
					}
					catch (RecognitionException)
					{
						synPredMatched39 = false;
					}
					rewind(_m39);
					inputState.guessing--;
				}
				if ( synPredMatched39 )
				{
					return_string=object_creation_expression();
				}
				else if ((LA(1)==NEW) && (tokenSet_3_.member(LA(2)))) {
					return_string=delegate_creation_expression();
				}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			break; }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  suffix_of_primary_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case DOT:
			case DEC:
			case INC:
			case LPAREN:
			{
				return_string=suffix_of_array_creation_expression();
				break;
			}
			case LBRACK:
			{
				return_string=element_access_end();
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  member_access_end() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  id = null;
		
		return_string = "";
		
		
		try {      // for error handling
			match(DOT);
			id = LT(1);
			match(IDENTIFIER);
			if (0==inputState.guessing)
			{
				return_string = "." + id.getText ();
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  invocation_expression_end() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  lp = null;
		Token  rp = null;
		
		string al = "";
		return_string = "";
		
		
		try {      // for error handling
			lp = LT(1);
			match(LPAREN);
			{
				switch ( LA(1) )
				{
				case INTEGER_LITERAL:
				case HEXADECIMAL_INTEGER_LITERAL:
				case REAL_LITERAL:
				case CHARACTER_LITERAL:
				case NULL:
				case TRUE:
				case FALSE:
				case REGULAR_STRING_LITERAL:
				case VERBATIM_STRING_LITERAL:
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case REF:
				case OUT:
				case DEC:
				case INC:
				case NEW:
				case LPAREN:
				case THIS:
				case BASE:
				case TYPEOF:
				case CHECKED:
				case UNCHECKED:
				case PLUS:
				case MINUS:
				case LNOT:
				case BNOT:
				case STAR:
				{
					al=argument_list();
					break;
				}
				case RPAREN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			rp = LT(1);
			match(RPAREN);
			if (0==inputState.guessing)
			{
				return_string = lp.getText () + al + rp.getText ();
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  element_access_end() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  lb = null;
		Token  rb = null;
		
		string el = "";
		return_string = "";
		
		
		try {      // for error handling
			lb = LT(1);
			match(LBRACK);
			el=expression_list();
			rb = LT(1);
			match(RBRACK);
			if (0==inputState.guessing)
			{
				return_string = lb.getText () + el + rb.getText ();
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  simple_name() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  id = null;
		
		return_string = "";
		
		
		try {      // for error handling
			id = LT(1);
			match(IDENTIFIER);
			if (0==inputState.guessing)
			{
				return_string = id.getText();
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  parenthesized_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  lp = null;
		Token  rp = null;
		
		string exp = "";
		return_string = "";
		
		
		try {      // for error handling
			lp = LT(1);
			match(LPAREN);
			exp=expression();
			rp = LT(1);
			match(RPAREN);
			if (0==inputState.guessing)
			{
				
				return_string = lp.getText () + exp + rp.getText ();
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  this_access() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  t = null;
		
		return_string = "";
		
		
		try {      // for error handling
			t = LT(1);
			match(THIS);
			if (0==inputState.guessing)
			{
				return_string = t.getText ();
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  base_access() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  b1 = null;
		Token  id = null;
		Token  b2 = null;
		Token  lb = null;
		Token  rb = null;
		
		string el = "";
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched56 = false;
			if (((LA(1)==BASE) && (LA(2)==DOT)))
			{
				int _m56 = mark();
				synPredMatched56 = true;
				inputState.guessing++;
				try {
					{
						match(BASE);
						match(DOT);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched56 = false;
				}
				rewind(_m56);
				inputState.guessing--;
			}
			if ( synPredMatched56 )
			{
				b1 = LT(1);
				match(BASE);
				match(DOT);
				id = LT(1);
				match(IDENTIFIER);
				if (0==inputState.guessing)
				{
					
					return_string = b1.getText () + "." + id.getText ();
					
				}
			}
			else if ((LA(1)==BASE) && (LA(2)==LBRACK)) {
				b2 = LT(1);
				match(BASE);
				lb = LT(1);
				match(LBRACK);
				el=expression_list();
				rb = LT(1);
				match(RBRACK);
				if (0==inputState.guessing)
				{
					
					return_string = b2.getText () + lb.getText () + el + rb.getText ();
					
				}
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  predefined_type() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  pt1 = null;
		Token  pt2 = null;
		Token  pt3 = null;
		Token  pt4 = null;
		Token  pt5 = null;
		Token  pt6 = null;
		Token  pt7 = null;
		Token  pt8 = null;
		Token  pt9 = null;
		Token  pt10 = null;
		Token  pt11 = null;
		Token  pt12 = null;
		Token  pt13 = null;
		Token  pt14 = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case BYTE:
			{
				pt1 = LT(1);
				match(BYTE);
				if (0==inputState.guessing)
				{
					return_string = pt1.getText();
				}
				break;
			}
			case CHAR:
			{
				pt2 = LT(1);
				match(CHAR);
				if (0==inputState.guessing)
				{
					return_string = pt2.getText();
				}
				break;
			}
			case DECIMAL:
			{
				pt3 = LT(1);
				match(DECIMAL);
				if (0==inputState.guessing)
				{
					return_string = pt3.getText();
				}
				break;
			}
			case DOUBLE:
			{
				pt4 = LT(1);
				match(DOUBLE);
				if (0==inputState.guessing)
				{
					return_string = pt4.getText();
				}
				break;
			}
			case FLOAT:
			{
				pt5 = LT(1);
				match(FLOAT);
				if (0==inputState.guessing)
				{
					return_string = pt5.getText();
				}
				break;
			}
			case INT:
			{
				pt6 = LT(1);
				match(INT);
				if (0==inputState.guessing)
				{
					return_string = pt6.getText();
				}
				break;
			}
			case LONG:
			{
				pt7 = LT(1);
				match(LONG);
				if (0==inputState.guessing)
				{
					return_string = pt7.getText();
				}
				break;
			}
			case OBJECT:
			{
				pt8 = LT(1);
				match(OBJECT);
				if (0==inputState.guessing)
				{
					return_string = pt8.getText();
				}
				break;
			}
			case SBYTE:
			{
				pt9 = LT(1);
				match(SBYTE);
				if (0==inputState.guessing)
				{
					return_string = pt9.getText();
				}
				break;
			}
			case SHORT:
			{
				pt10 = LT(1);
				match(SHORT);
				if (0==inputState.guessing)
				{
					return_string = pt10.getText();
				}
				break;
			}
			case STRING:
			{
				pt11 = LT(1);
				match(STRING);
				if (0==inputState.guessing)
				{
					return_string = pt11.getText();
				}
				break;
			}
			case UINT:
			{
				pt12 = LT(1);
				match(UINT);
				if (0==inputState.guessing)
				{
					return_string = pt12.getText();
				}
				break;
			}
			case ULONG:
			{
				pt13 = LT(1);
				match(ULONG);
				if (0==inputState.guessing)
				{
					return_string = pt13.getText();
				}
				break;
			}
			case USHORT:
			{
				pt14 = LT(1);
				match(USHORT);
				if (0==inputState.guessing)
				{
					return_string = pt14.getText();
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_23_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  object_creation_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  n = null;
		Token  lp = null;
		Token  rp = null;
		
		string tp = "";
		string al = "";
		return_string = "";
		
		
		try {      // for error handling
			n = LT(1);
			match(NEW);
			tp=type();
			lp = LT(1);
			match(LPAREN);
			{
				switch ( LA(1) )
				{
				case INTEGER_LITERAL:
				case HEXADECIMAL_INTEGER_LITERAL:
				case REAL_LITERAL:
				case CHARACTER_LITERAL:
				case NULL:
				case TRUE:
				case FALSE:
				case REGULAR_STRING_LITERAL:
				case VERBATIM_STRING_LITERAL:
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case REF:
				case OUT:
				case DEC:
				case INC:
				case NEW:
				case LPAREN:
				case THIS:
				case BASE:
				case TYPEOF:
				case CHECKED:
				case UNCHECKED:
				case PLUS:
				case MINUS:
				case LNOT:
				case BNOT:
				case STAR:
				{
					al=argument_list();
					break;
				}
				case RPAREN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			rp = LT(1);
			match(RPAREN);
			if (0==inputState.guessing)
			{
				
				return_string = ((ExtendedToken)n).GetWhitespaces () + tp + lp.getText () + al + rp.getText ();
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  delegate_creation_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  n = null;
		Token  lp = null;
		Token  rp = null;
		
		string tp = "";
		string exp = "";
		return_string = "";
		
		
		try {      // for error handling
			n = LT(1);
			match(NEW);
			tp=type();
			lp = LT(1);
			match(LPAREN);
			exp=expression();
			rp = LT(1);
			match(RPAREN);
			if (0==inputState.guessing)
			{
				
				return_string = ((ExtendedToken)n).GetWhitespaces ()  + tp + lp.getText () + exp + rp.getText ();
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  typeof_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  t = null;
		Token  lp = null;
		Token  v = null;
		Token  rp = null;
		
		string tp = "";
		return_string = "";
		
		
		try {      // for error handling
			t = LT(1);
			match(TYPEOF);
			lp = LT(1);
			match(LPAREN);
			{
				switch ( LA(1) )
				{
				case VOID:
				{
					v = LT(1);
					match(VOID);
					if (0==inputState.guessing)
					{
						tp = v.getText ();
					}
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				{
					tp=type();
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			rp = LT(1);
			match(RPAREN);
			if (0==inputState.guessing)
			{
				
				return_string = t.getText () + lp.getText () + tp + rp.getText ();
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  checked_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  c = null;
		Token  lp = null;
		Token  rp = null;
		
		string exp = "";
		return_string = "";
		
		
		try {      // for error handling
			c = LT(1);
			match(CHECKED);
			lp = LT(1);
			match(LPAREN);
			exp=expression();
			rp = LT(1);
			match(RPAREN);
			if (0==inputState.guessing)
			{
				
				return_string = c.getText () + lp.getText () + exp + rp.getText ();
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  unchecked_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  u = null;
		Token  lp = null;
		Token  rp = null;
		
		string exp = "";
		return_string = "";
		
		
		try {      // for error handling
			u = LT(1);
			match(UNCHECKED);
			lp = LT(1);
			match(LPAREN);
			exp=expression();
			rp = LT(1);
			match(RPAREN);
			if (0==inputState.guessing)
			{
				
				return_string = u.getText () + lp.getText () + exp + rp.getText ();
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_0_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  expression_list() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  c = null;
		
		string exp = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						c = LT(1);
						match(COMMA);
						exp=expression();
						if (0==inputState.guessing)
						{
							return_string += (c.getText () + exp);
						}
					}
					else
					{
						goto _loop52_breakloop;
					}
					
				}
_loop52_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_24_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void array_initializer() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched499 = false;
			if (((LA(1)==LBRACE) && (tokenSet_25_.member(LA(2)))))
			{
				int _m499 = mark();
				synPredMatched499 = true;
				inputState.guessing++;
				try {
					{
						match(LBRACE);
						variable_initializer_list();
						match(COMMA);
						match(RBRACE);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched499 = false;
				}
				rewind(_m499);
				inputState.guessing--;
			}
			if ( synPredMatched499 )
			{
				match(LBRACE);
				variable_initializer_list();
				match(COMMA);
				match(RBRACE);
			}
			else if ((LA(1)==LBRACE) && (tokenSet_26_.member(LA(2)))) {
				match(LBRACE);
				{
					switch ( LA(1) )
					{
					case INTEGER_LITERAL:
					case HEXADECIMAL_INTEGER_LITERAL:
					case REAL_LITERAL:
					case CHARACTER_LITERAL:
					case NULL:
					case TRUE:
					case FALSE:
					case REGULAR_STRING_LITERAL:
					case VERBATIM_STRING_LITERAL:
					case IDENTIFIER:
					case OBJECT:
					case STRING:
					case DECIMAL:
					case CHAR:
					case INT:
					case LONG:
					case SBYTE:
					case BYTE:
					case SHORT:
					case UINT:
					case ULONG:
					case USHORT:
					case FLOAT:
					case DOUBLE:
					case DEC:
					case INC:
					case NEW:
					case LPAREN:
					case THIS:
					case BASE:
					case TYPEOF:
					case CHECKED:
					case UNCHECKED:
					case PLUS:
					case MINUS:
					case LNOT:
					case BNOT:
					case STAR:
					case LBRACE:
					{
						variable_initializer_list();
						break;
					}
					case RBRACE:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				match(RBRACE);
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_22_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  unary_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  p = null;
		Token  m = null;
		Token  l = null;
		Token  b = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case PLUS:
			{
				p = LT(1);
				match(PLUS);
				return_string=unary_expression();
				if (0==inputState.guessing)
				{
					return_string = p.getText () + return_string ;
				}
				break;
			}
			case MINUS:
			{
				m = LT(1);
				match(MINUS);
				return_string=unary_expression();
				if (0==inputState.guessing)
				{
					return_string = m.getText () + return_string ;
				}
				break;
			}
			case LNOT:
			{
				l = LT(1);
				match(LNOT);
				return_string=unary_expression();
				if (0==inputState.guessing)
				{
					return_string = l.getText () + return_string ;
				}
				break;
			}
			case BNOT:
			{
				b = LT(1);
				match(BNOT);
				return_string=unary_expression();
				if (0==inputState.guessing)
				{
					return_string = b.getText () + return_string ;
				}
				break;
			}
			case STAR:
			{
				match(STAR);
				unary_expression();
				break;
			}
			case INC:
			{
				return_string=pre_increment_expression();
				break;
			}
			case DEC:
			{
				return_string=pre_decrement_expression();
				break;
			}
			default:
				bool synPredMatched69 = false;
				if (((LA(1)==LPAREN) && (tokenSet_3_.member(LA(2)))))
				{
					int _m69 = mark();
					synPredMatched69 = true;
					inputState.guessing++;
					try {
						{
							cast_expression();
						}
					}
					catch (RecognitionException)
					{
						synPredMatched69 = false;
					}
					rewind(_m69);
					inputState.guessing--;
				}
				if ( synPredMatched69 )
				{
					return_string=cast_expression();
				}
				else if ((tokenSet_18_.member(LA(1))) && (tokenSet_19_.member(LA(2)))) {
					return_string=primary_expression();
				}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			break; }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_21_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  cast_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  lp = null;
		Token  rp = null;
		
		string t = "";
		return_string = "";
		
		
		try {      // for error handling
			lp = LT(1);
			match(LPAREN);
			t=type();
			rp = LT(1);
			match(RPAREN);
			return_string=unary_expression();
			if (0==inputState.guessing)
			{
				return_string = lp.getText () + return_string + " :> " + t + rp.getText ()  ;
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_21_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  pre_increment_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  i = null;
		
		return_string = "";
		
		
		try {      // for error handling
			i = LT(1);
			match(INC);
			return_string=unary_expression();
			if (0==inputState.guessing)
			{
				return_string = i.getText () + return_string ;
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_21_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  pre_decrement_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  d = null;
		
		return_string = "";
		
		
		try {      // for error handling
			d = LT(1);
			match(DEC);
			return_string=unary_expression();
			if (0==inputState.guessing)
			{
				return_string = d.getText () + return_string ;
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_21_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  multiplicative_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		string mop = "";
		string ue = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=unary_expression();
			{    // ( ... )*
				for (;;)
				{
					if (((LA(1) >= STAR && LA(1) <= MOD)))
					{
						mop=multiplicative_op();
						ue=unary_expression();
						if (0==inputState.guessing)
						{
							
							return_string += (mop + ue);
							
						}
					}
					else
					{
						goto _loop75_breakloop;
					}
					
				}
_loop75_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_27_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  multiplicative_op() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  s = null;
		Token  d = null;
		Token  m = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case STAR:
			{
				s = LT(1);
				match(STAR);
				if (0==inputState.guessing)
				{
					return_string = s.getText () ;
				}
				break;
			}
			case DIV:
			{
				d = LT(1);
				match(DIV);
				if (0==inputState.guessing)
				{
					return_string = d.getText () ;
				}
				break;
			}
			case MOD:
			{
				m = LT(1);
				match(MOD);
				if (0==inputState.guessing)
				{
					return_string = m.getText () ;
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_12_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  additive_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		string aop = "";
		string me = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=multiplicative_expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==PLUS||LA(1)==MINUS))
					{
						aop=additive_op();
						me=multiplicative_expression();
						if (0==inputState.guessing)
						{
							
							return_string += (aop + me);
							
						}
					}
					else
					{
						goto _loop79_breakloop;
					}
					
				}
_loop79_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_28_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  additive_op() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  p = null;
		Token  m = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case PLUS:
			{
				p = LT(1);
				match(PLUS);
				if (0==inputState.guessing)
				{
					return_string = p.getText () ;
				}
				break;
			}
			case MINUS:
			{
				m = LT(1);
				match(MINUS);
				if (0==inputState.guessing)
				{
					return_string = m.getText () ;
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_12_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  shift_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		string sop = "";
		string ae = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=additive_expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==SL||LA(1)==SR))
					{
						sop=shift_op();
						ae=additive_expression();
						if (0==inputState.guessing)
						{
							
							return_string += (sop + ae);
							
						}
					}
					else
					{
						goto _loop83_breakloop;
					}
					
				}
_loop83_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_29_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  shift_op() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  sl = null;
		Token  sr = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case SL:
			{
				sl = LT(1);
				match(SL);
				if (0==inputState.guessing)
				{
					return_string = sl.getText () ;
				}
				break;
			}
			case SR:
			{
				sr = LT(1);
				match(SR);
				if (0==inputState.guessing)
				{
					return_string = sr.getText () ;
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_12_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  relational_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  tis = null;
		Token  tas = null;
		
		string rop = "";
		string se = "";
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched87 = false;
			if (((tokenSet_12_.member(LA(1))) && (tokenSet_30_.member(LA(2)))))
			{
				int _m87 = mark();
				synPredMatched87 = true;
				inputState.guessing++;
				try {
					{
						shift_expression();
						relational_op();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched87 = false;
				}
				rewind(_m87);
				inputState.guessing--;
			}
			if ( synPredMatched87 )
			{
				return_string=shift_expression();
				{ // ( ... )+
				int _cnt89=0;
				for (;;)
				{
					if (((LA(1) >= LTHAN && LA(1) <= GE)))
					{
						rop=relational_op();
						se=shift_expression();
						if (0==inputState.guessing)
						{
							
							return_string += (rop + se);
							
						}
					}
					else
					{
						if (_cnt89 >= 1) { goto _loop89_breakloop; } else { throw new NoViableAltException(LT(1), getFilename());; }
					}
					
					_cnt89++;
				}
_loop89_breakloop:				;
				}    // ( ... )+
			}
			else {
				bool synPredMatched91 = false;
				if (((tokenSet_12_.member(LA(1))) && (tokenSet_31_.member(LA(2)))))
				{
					int _m91 = mark();
					synPredMatched91 = true;
					inputState.guessing++;
					try {
						{
							shift_expression();
							match(IS);
						}
					}
					catch (RecognitionException)
					{
						synPredMatched91 = false;
					}
					rewind(_m91);
					inputState.guessing--;
				}
				if ( synPredMatched91 )
				{
					return_string=shift_expression();
					{ // ( ... )+
					int _cnt93=0;
					for (;;)
					{
						if ((LA(1)==IS))
						{
							tis = LT(1);
							match(IS);
							se=type();
							if (0==inputState.guessing)
							{
								
								return_string += (tis.getText () + se);
								
							}
						}
						else
						{
							if (_cnt93 >= 1) { goto _loop93_breakloop; } else { throw new NoViableAltException(LT(1), getFilename());; }
						}
						
						_cnt93++;
					}
_loop93_breakloop:					;
					}    // ( ... )+
				}
				else {
					bool synPredMatched95 = false;
					if (((tokenSet_12_.member(LA(1))) && (tokenSet_32_.member(LA(2)))))
					{
						int _m95 = mark();
						synPredMatched95 = true;
						inputState.guessing++;
						try {
							{
								shift_expression();
								match(AS);
							}
						}
						catch (RecognitionException)
						{
							synPredMatched95 = false;
						}
						rewind(_m95);
						inputState.guessing--;
					}
					if ( synPredMatched95 )
					{
						return_string=shift_expression();
						{ // ( ... )+
						int _cnt97=0;
						for (;;)
						{
							if ((LA(1)==AS))
							{
								tas = LT(1);
								match(AS);
								se=type();
								if (0==inputState.guessing)
								{
									
									return_string += (tas.getText () + se);
									
								}
							}
							else
							{
								if (_cnt97 >= 1) { goto _loop97_breakloop; } else { throw new NoViableAltException(LT(1), getFilename());; }
							}
							
							_cnt97++;
						}
_loop97_breakloop:						;
						}    // ( ... )+
					}
					else if ((tokenSet_12_.member(LA(1))) && (tokenSet_33_.member(LA(2)))) {
						return_string=shift_expression();
					}
					else
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					}}
				}
				catch (RecognitionException ex)
				{
					if (0 == inputState.guessing)
					{
						reportError(ex);
						consume();
						consumeUntil(tokenSet_34_);
					}
					else
					{
						throw;
					}
				}
				return return_string;
			}
			
	public string  relational_op() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  lt = null;
		Token  gt = null;
		Token  le = null;
		Token  ge = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case LTHAN:
			{
				lt = LT(1);
				match(LTHAN);
				if (0==inputState.guessing)
				{
					return_string = lt.getText () ;
				}
				break;
			}
			case GTHAN:
			{
				gt = LT(1);
				match(GTHAN);
				if (0==inputState.guessing)
				{
					return_string = gt.getText () ;
				}
				break;
			}
			case LE:
			{
				le = LT(1);
				match(LE);
				if (0==inputState.guessing)
				{
					return_string = le.getText () ;
				}
				break;
			}
			case GE:
			{
				ge = LT(1);
				match(GE);
				if (0==inputState.guessing)
				{
					return_string = ge.getText () ;
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_12_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  equality_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		string eop = "";
		string re = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=relational_expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==EQUAL||LA(1)==NOT_EQUAL))
					{
						eop=equality_op();
						re=relational_expression();
						if (0==inputState.guessing)
						{
							
							return_string += (eop + re);
							
						}
					}
					else
					{
						goto _loop101_breakloop;
					}
					
				}
_loop101_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_35_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  equality_op() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  e = null;
		Token  n = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case EQUAL:
			{
				e = LT(1);
				match(EQUAL);
				if (0==inputState.guessing)
				{
					return_string = e.getText () ;
				}
				break;
			}
			case NOT_EQUAL:
			{
				n = LT(1);
				match(NOT_EQUAL);
				if (0==inputState.guessing)
				{
					return_string = n.getText () ;
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_12_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  and_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  b = null;
		
		string ee = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=equality_expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==BAND))
					{
						b = LT(1);
						match(BAND);
						ee=equality_expression();
						if (0==inputState.guessing)
						{
							
							return_string += (b.getText () + ee);
							
						}
					}
					else
					{
						goto _loop105_breakloop;
					}
					
				}
_loop105_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_36_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  exclusive_or_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  b = null;
		
		string ae = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=and_expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==BXOR))
					{
						b = LT(1);
						match(BXOR);
						ae=and_expression();
						if (0==inputState.guessing)
						{
							
							return_string += (b.getText () + ae);
							
						}
					}
					else
					{
						goto _loop108_breakloop;
					}
					
				}
_loop108_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_37_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  inclusive_or_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  b = null;
		
		string eoe = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=exclusive_or_expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==BOR))
					{
						b = LT(1);
						match(BOR);
						eoe=exclusive_or_expression();
						if (0==inputState.guessing)
						{
							
							return_string += (b.getText () + eoe);
							
						}
					}
					else
					{
						goto _loop111_breakloop;
					}
					
				}
_loop111_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_38_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  conditional_and_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  l = null;
		
		string ioe = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=inclusive_or_expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==LAND))
					{
						l = LT(1);
						match(LAND);
						ioe=inclusive_or_expression();
						if (0==inputState.guessing)
						{
							
							return_string += (l.getText () + ioe);
							
						}
					}
					else
					{
						goto _loop114_breakloop;
					}
					
				}
_loop114_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_39_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  conditional_or_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  l = null;
		
		string cae = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=conditional_and_expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==LOR))
					{
						l = LT(1);
						match(LOR);
						cae=conditional_and_expression();
						if (0==inputState.guessing)
						{
							
							return_string += (l.getText () + cae);
							
						}
					}
					else
					{
						goto _loop117_breakloop;
					}
					
				}
_loop117_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_40_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  conditional_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			return_string=conditional_or_expression();
			{
				switch ( LA(1) )
				{
				case QUESTION:
				{
					match(QUESTION);
					expression();
					match(COLON);
					expression();
					break;
				}
				case COMMA:
				case RBRACK:
				case RPAREN:
				case COLON:
				case SEMI:
				case RBRACE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_15_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  assignment_operator() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  a1 = null;
		Token  a2 = null;
		Token  a3 = null;
		Token  a4 = null;
		Token  a5 = null;
		Token  a6 = null;
		Token  a7 = null;
		Token  a8 = null;
		Token  a9 = null;
		Token  a10 = null;
		Token  a11 = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case ASSIGN:
			{
				a1 = LT(1);
				match(ASSIGN);
				if (0==inputState.guessing)
				{
					return_string = a1.getText () ;
				}
				break;
			}
			case PLUS_ASN:
			{
				a2 = LT(1);
				match(PLUS_ASN);
				if (0==inputState.guessing)
				{
					return_string = a2.getText () ;
				}
				break;
			}
			case MINUS_ASN:
			{
				a3 = LT(1);
				match(MINUS_ASN);
				if (0==inputState.guessing)
				{
					return_string = a3.getText () ;
				}
				break;
			}
			case STAR_ASN:
			{
				a4 = LT(1);
				match(STAR_ASN);
				if (0==inputState.guessing)
				{
					return_string = a4.getText () ;
				}
				break;
			}
			case DIV_ASN:
			{
				a5 = LT(1);
				match(DIV_ASN);
				if (0==inputState.guessing)
				{
					return_string = a5.getText () ;
				}
				break;
			}
			case MOD_ASN:
			{
				a6 = LT(1);
				match(MOD_ASN);
				if (0==inputState.guessing)
				{
					return_string = a6.getText () ;
				}
				break;
			}
			case BAND_ASN:
			{
				a7 = LT(1);
				match(BAND_ASN);
				if (0==inputState.guessing)
				{
					return_string = a7.getText () ;
				}
				break;
			}
			case BOR_ASN:
			{
				a8 = LT(1);
				match(BOR_ASN);
				if (0==inputState.guessing)
				{
					return_string = a8.getText () ;
				}
				break;
			}
			case BXOR_ASN:
			{
				a9 = LT(1);
				match(BXOR_ASN);
				if (0==inputState.guessing)
				{
					return_string = a9.getText () ;
				}
				break;
			}
			case SL_ASN:
			{
				a10 = LT(1);
				match(SL_ASN);
				if (0==inputState.guessing)
				{
					return_string = a10.getText () ;
				}
				break;
			}
			case SR_ASN:
			{
				a11 = LT(1);
				match(SR_ASN);
				if (0==inputState.guessing)
				{
					return_string = a11.getText () ;
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_12_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  assignment() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		string temp = "";
		return_string = "";
		
		
		try {      // for error handling
			return_string=unary_expression();
			temp=assignment_operator();
			if (0==inputState.guessing)
			{
				return_string += temp;
			}
			temp=expression();
			if (0==inputState.guessing)
			{
				return_string += temp;
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_15_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  constant_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			return_string=expression();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_41_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  boolean_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			return_string=expression();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_42_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public StatementTree  statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		Token  i = null;
		Token  c = null;
		
		t = new StatementTree();
		StatementTree st = new StatementTree();
		LinkedList a = new LinkedList ();
		
		
		try {      // for error handling
			bool synPredMatched128 = false;
			if (((LA(1)==IDENTIFIER) && (LA(2)==COLON)))
			{
				int _m128 = mark();
				synPredMatched128 = true;
				inputState.guessing++;
				try {
					{
						match(IDENTIFIER);
						match(COLON);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched128 = false;
				}
				rewind(_m128);
				inputState.guessing--;
			}
			if ( synPredMatched128 )
			{
				i = LT(1);
				match(IDENTIFIER);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(i));
				}
				c = LT(1);
				match(COLON);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(c));
				}
				st=statement();
				if (0==inputState.guessing)
				{
					a.Add (st);
				}
				if (0==inputState.guessing)
				{
					t = new StatementTree ("LABEL",a);
				}
			}
			else {
				bool synPredMatched130 = false;
				if (((tokenSet_43_.member(LA(1))) && ((LA(2) >= IDENTIFIER && LA(2) <= LBRACK))))
				{
					int _m130 = mark();
					synPredMatched130 = true;
					inputState.guessing++;
					try {
						{
							match(CONST);
						}
					}
					catch (RecognitionException)
					{
						synPredMatched130 = false;
					}
					rewind(_m130);
					inputState.guessing--;
				}
				if ( synPredMatched130 )
				{
					t=declaration_statement();
				}
				else {
					bool synPredMatched132 = false;
					if (((tokenSet_43_.member(LA(1))) && ((LA(2) >= IDENTIFIER && LA(2) <= LBRACK))))
					{
						int _m132 = mark();
						synPredMatched132 = true;
						inputState.guessing++;
						try {
							{
								type();
								match(IDENTIFIER);
							}
						}
						catch (RecognitionException)
						{
							synPredMatched132 = false;
						}
						rewind(_m132);
						inputState.guessing--;
					}
					if ( synPredMatched132 )
					{
						t=declaration_statement();
					}
					else if ((tokenSet_44_.member(LA(1))) && (tokenSet_45_.member(LA(2)))) {
						t=embedded_statement();
					}
					else
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					}}
				}
				catch (RecognitionException ex)
				{
					if (0 == inputState.guessing)
					{
						reportError(ex);
						consume();
						consumeUntil(tokenSet_46_);
					}
					else
					{
						throw;
					}
				}
				return t;
			}
			
	public StatementTree  declaration_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		Token  s = null;
		
		t = new StatementTree();
		string temp = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case IDENTIFIER:
			case OBJECT:
			case STRING:
			case BOOL:
			case DECIMAL:
			case CHAR:
			case INT:
			case LONG:
			case SBYTE:
			case BYTE:
			case SHORT:
			case UINT:
			case ULONG:
			case USHORT:
			case FLOAT:
			case DOUBLE:
			{
				local_variable_declaration();
				match(SEMI);
				break;
			}
			case CONST:
			{
				temp=local_constant_declaration();
				s = LT(1);
				match(SEMI);
				if (0==inputState.guessing)
				{
					
					t = new StatementTree (temp + ((ExtendedToken)s).GetWhitespaces ());
					
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_46_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  embedded_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case LBRACE:
			{
				t=Block();
				break;
			}
			case SEMI:
			{
				t=empty_statement();
				break;
			}
			case IF:
			case SWITCH:
			{
				t=selection_statement();
				break;
			}
			case WHILE:
			case DO:
			case FOR:
			case FOREACH:
			{
				t=iteration_statement();
				break;
			}
			case BREAK:
			case CONTINUE:
			case GOTO:
			case RETURN:
			case THROW:
			{
				t=jump_statement();
				break;
			}
			case TRY:
			{
				t=try_statement();
				break;
			}
			case LOCK:
			{
				lock_statement();
				break;
			}
			case USING:
			{
				using_statement();
				break;
			}
			default:
				bool synPredMatched135 = false;
				if (((LA(1)==CHECKED) && (LA(2)==LBRACE)))
				{
					int _m135 = mark();
					synPredMatched135 = true;
					inputState.guessing++;
					try {
						{
							match(CHECKED);
							Block();
						}
					}
					catch (RecognitionException)
					{
						synPredMatched135 = false;
					}
					rewind(_m135);
					inputState.guessing--;
				}
				if ( synPredMatched135 )
				{
					match(CHECKED);
					Block();
				}
				else {
					bool synPredMatched137 = false;
					if (((LA(1)==UNCHECKED) && (LA(2)==LBRACE)))
					{
						int _m137 = mark();
						synPredMatched137 = true;
						inputState.guessing++;
						try {
							{
								match(UNCHECKED);
								Block();
							}
						}
						catch (RecognitionException)
						{
							synPredMatched137 = false;
						}
						rewind(_m137);
						inputState.guessing--;
					}
					if ( synPredMatched137 )
					{
						match(UNCHECKED);
						Block();
					}
					else if ((tokenSet_12_.member(LA(1))) && (tokenSet_47_.member(LA(2)))) {
						t=expression_statement();
					}
				else
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}break; }
			}
			catch (RecognitionException ex)
			{
				if (0 == inputState.guessing)
				{
					reportError(ex);
					consume();
					consumeUntil(tokenSet_48_);
				}
				else
				{
					throw;
				}
			}
			return t;
		}
		
	public StatementTree  Block() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		Token  lb = null;
		Token  rb = null;
		
		t = new StatementTree();
		StatementTree temp = new StatementTree ();
		LinkedList a = new LinkedList ();
		
		
		try {      // for error handling
			lb = LT(1);
			match(LBRACE);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(lb));
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_49_.member(LA(1))))
					{
						temp=statement();
						if (0==inputState.guessing)
						{
							a.Add (temp);
						}
					}
					else
					{
						goto _loop141_breakloop;
					}
					
				}
_loop141_breakloop:				;
			}    // ( ... )*
			rb = LT(1);
			match(RBRACE);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(rb));
			}
			if (0==inputState.guessing)
			{
				t = new StatementTree ("BLOCK",a);
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_50_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  empty_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		Token  s = null;
		
		t = new StatementTree();
		
		
		try {      // for error handling
			s = LT(1);
			match(SEMI);
			if (0==inputState.guessing)
			{
				t = new StatementTree (s);
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  selection_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case IF:
			{
				t=if_statement();
				break;
			}
			case SWITCH:
			{
				t=switch_statement();
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  iteration_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case WHILE:
			{
				t=while_statement();
				break;
			}
			case DO:
			{
				t=do_statement();
				break;
			}
			case FOR:
			{
				for_statement();
				break;
			}
			case FOREACH:
			{
				foreach_statement();
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  jump_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case BREAK:
			{
				break_statement();
				break;
			}
			case CONTINUE:
			{
				continue_statement();
				break;
			}
			case GOTO:
			{
				t=goto_statement();
				break;
			}
			case RETURN:
			{
				t=return_statement();
				break;
			}
			case THROW:
			{
				throw_statement();
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  try_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			match(TRY);
			Block();
			end_of_try_statement();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  expression_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		Token  s = null;
		
		t = new StatementTree();
		string temp = "";
		LinkedList a = new LinkedList ();
		
		
		try {      // for error handling
			temp=statement_expression();
			s = LT(1);
			match(SEMI);
			if (0==inputState.guessing)
			{
				
				a.Add ( new StatementTree (temp) );
				a.Add ( new StatementTree (s) );
				t = new StatementTree ("EXPRESSION_STATEMENT",a) ;
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  lock_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			match(LOCK);
			match(LPAREN);
			expression();
			match(RPAREN);
			embedded_statement();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  using_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree() ;
		
		
		try {      // for error handling
			match(USING);
			match(LPAREN);
			resource_acquisition();
			match(RPAREN);
			embedded_statement();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public void local_variable_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			type();
			local_variable_declarator();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						match(COMMA);
						local_variable_declarator();
					}
					else
					{
						goto _loop145_breakloop;
					}
					
				}
_loop145_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_42_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  local_constant_declaration() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  c = null;
		Token  cm = null;
		
		return_string = "";
		string t = "";
		string temp = "";
		
		
		try {      // for error handling
			c = LT(1);
			match(CONST);
			t=type();
			return_string=local_constant_declarator(((ExtendedToken)c).GetWhitespaces (),"");
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						cm = LT(1);
						match(COMMA);
						temp=local_constant_declarator(((ExtendedToken)c).GetWhitespaces (),((ExtendedToken)cm).GetWhitespaces ());
						if (0==inputState.guessing)
						{
							return_string += temp;
						}
					}
					else
					{
						goto _loop151_breakloop;
					}
					
				}
_loop151_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_1_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void local_variable_declarator() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			match(IDENTIFIER);
			{
				switch ( LA(1) )
				{
				case ASSIGN:
				{
					match(ASSIGN);
					local_variable_initializer();
					break;
				}
				case COMMA:
				case RPAREN:
				case SEMI:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_51_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void local_variable_initializer() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case INTEGER_LITERAL:
			case HEXADECIMAL_INTEGER_LITERAL:
			case REAL_LITERAL:
			case CHARACTER_LITERAL:
			case NULL:
			case TRUE:
			case FALSE:
			case REGULAR_STRING_LITERAL:
			case VERBATIM_STRING_LITERAL:
			case IDENTIFIER:
			case OBJECT:
			case STRING:
			case DECIMAL:
			case CHAR:
			case INT:
			case LONG:
			case SBYTE:
			case BYTE:
			case SHORT:
			case UINT:
			case ULONG:
			case USHORT:
			case FLOAT:
			case DOUBLE:
			case DEC:
			case INC:
			case NEW:
			case LPAREN:
			case THIS:
			case BASE:
			case TYPEOF:
			case CHECKED:
			case UNCHECKED:
			case PLUS:
			case MINUS:
			case LNOT:
			case BNOT:
			case STAR:
			{
				expression();
				break;
			}
			case LBRACE:
			{
				array_initializer();
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_51_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  local_constant_declarator(
		string c1,string c2
	) //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  id = null;
		Token  a = null;
		
		return_string = "";
		string ce = "";
		
		
		try {      // for error handling
			id = LT(1);
			match(IDENTIFIER);
			a = LT(1);
			match(ASSIGN);
			ce=constant_expression();
			if (0==inputState.guessing)
			{
				
				return_string = c1 + "def " + c2 + id.getText () + a.getText () + ce + ";";
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_52_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  statement_expression() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched156 = false;
			if (((tokenSet_12_.member(LA(1))) && (tokenSet_14_.member(LA(2)))))
			{
				int _m156 = mark();
				synPredMatched156 = true;
				inputState.guessing++;
				try {
					{
						assignment();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched156 = false;
				}
				rewind(_m156);
				inputState.guessing--;
			}
			if ( synPredMatched156 )
			{
				return_string=assignment();
			}
			else if ((tokenSet_18_.member(LA(1))) && (tokenSet_53_.member(LA(2)))) {
				return_string=primary_expression();
			}
			else if ((LA(1)==INC) && (tokenSet_12_.member(LA(2)))) {
				return_string=pre_increment_expression();
			}
			else if ((LA(1)==DEC) && (tokenSet_12_.member(LA(2)))) {
				return_string=pre_decrement_expression();
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_51_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public StatementTree  if_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		Token  i1 = null;
		Token  lp1 = null;
		Token  rp1 = null;
		Token  e = null;
		Token  i2 = null;
		Token  lp2 = null;
		Token  rp2 = null;
		
		t = new StatementTree();
		StatementTree t1 = new StatementTree();
		StatementTree t2; 
		LinkedList a = new LinkedList ();
		string be = "";
		
		
		try {      // for error handling
			bool synPredMatched161 = false;
			if (((LA(1)==IF) && (LA(2)==LPAREN)))
			{
				int _m161 = mark();
				synPredMatched161 = true;
				inputState.guessing++;
				try {
					{
						match(IF);
						match(LPAREN);
						boolean_expression();
						match(RPAREN);
						embedded_statement();
						match(ELSE);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched161 = false;
				}
				rewind(_m161);
				inputState.guessing--;
			}
			if ( synPredMatched161 )
			{
				i1 = LT(1);
				match(IF);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(i1));
				}
				lp1 = LT(1);
				match(LPAREN);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(lp1));
				}
				be=boolean_expression();
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(be));
				}
				rp1 = LT(1);
				match(RPAREN);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(rp1));
				}
				t1=embedded_statement();
				if (0==inputState.guessing)
				{
					a.Add (t1);
				}
				e = LT(1);
				match(ELSE);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(e));t2 = new StatementTree();
				}
				t2=embedded_statement();
				if (0==inputState.guessing)
				{
					a.Add (t2);
				}
				if (0==inputState.guessing)
				{
					t = new StatementTree("IF",a);
				}
			}
			else if ((LA(1)==IF) && (LA(2)==LPAREN)) {
				i2 = LT(1);
				match(IF);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree( ((ExtendedToken)i2).GetWhitespaces () + "when"));
				}
				lp2 = LT(1);
				match(LPAREN);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(lp2));
				}
				be=boolean_expression();
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(be));
				}
				rp2 = LT(1);
				match(RPAREN);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(rp2));
				}
				t1=embedded_statement();
				if (0==inputState.guessing)
				{
					a.Add (t1);
				}
				if (0==inputState.guessing)
				{
					t = new StatementTree("WHEN",a);
				}
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  switch_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			match(SWITCH);
			match(LPAREN);
			expression();
			match(RPAREN);
			switch_Block();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  switch_Block() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			match(LBRACE);
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==CASE||LA(1)==DEFAULT))
					{
						switch_section();
					}
					else
					{
						goto _loop165_breakloop;
					}
					
				}
_loop165_breakloop:				;
			}    // ( ... )*
			match(RBRACE);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  switch_section() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			{ // ( ... )+
			int _cnt168=0;
			for (;;)
			{
				if ((LA(1)==CASE||LA(1)==DEFAULT))
				{
					switch_label();
				}
				else
				{
					if (_cnt168 >= 1) { goto _loop168_breakloop; } else { throw new NoViableAltException(LT(1), getFilename());; }
				}
				
				_cnt168++;
			}
_loop168_breakloop:			;
			}    // ( ... )+
			{ // ( ... )+
			int _cnt170=0;
			for (;;)
			{
				if ((tokenSet_49_.member(LA(1))))
				{
					statement();
				}
				else
				{
					if (_cnt170 >= 1) { goto _loop170_breakloop; } else { throw new NoViableAltException(LT(1), getFilename());; }
				}
				
				_cnt170++;
			}
_loop170_breakloop:			;
			}    // ( ... )+
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_54_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public void switch_label() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case CASE:
			{
				match(CASE);
				constant_expression();
				match(COLON);
				break;
			}
			case DEFAULT:
			{
				match(DEFAULT);
				match(COLON);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_55_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public StatementTree  while_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		Token  w = null;
		Token  lp = null;
		Token  rp = null;
		
		t = new StatementTree();
		StatementTree t1 = new StatementTree();
		LinkedList a = new LinkedList ();
		string be = "";
		
		
		try {      // for error handling
			w = LT(1);
			match(WHILE);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(w));
			}
			lp = LT(1);
			match(LPAREN);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(lp));
			}
			be=boolean_expression();
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(be));
			}
			rp = LT(1);
			match(RPAREN);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(rp));
			}
			t1=embedded_statement();
			if (0==inputState.guessing)
			{
				a.Add (t1);
			}
			if (0==inputState.guessing)
			{
				t = new StatementTree("WHILE",a);
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  do_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		Token  d = null;
		Token  w = null;
		Token  lp = null;
		Token  rp = null;
		Token  s = null;
		
		t = new StatementTree();
		StatementTree t1 = new StatementTree();
		LinkedList a = new LinkedList ();
		string be = "";
		
		
		try {      // for error handling
			d = LT(1);
			match(DO);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(d));
			}
			t1=embedded_statement();
			if (0==inputState.guessing)
			{
				a.Add (t1);
			}
			w = LT(1);
			match(WHILE);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(w));
			}
			lp = LT(1);
			match(LPAREN);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(lp));
			}
			be=boolean_expression();
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(be));
			}
			rp = LT(1);
			match(RPAREN);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(rp));
			}
			s = LT(1);
			match(SEMI);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(s));
			}
			if (0==inputState.guessing)
			{
				t = new StatementTree("DO",a);
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  for_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			match(FOR);
			match(LPAREN);
			{
				switch ( LA(1) )
				{
				case INTEGER_LITERAL:
				case HEXADECIMAL_INTEGER_LITERAL:
				case REAL_LITERAL:
				case CHARACTER_LITERAL:
				case NULL:
				case TRUE:
				case FALSE:
				case REGULAR_STRING_LITERAL:
				case VERBATIM_STRING_LITERAL:
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case DEC:
				case INC:
				case NEW:
				case LPAREN:
				case THIS:
				case BASE:
				case TYPEOF:
				case CHECKED:
				case UNCHECKED:
				case PLUS:
				case MINUS:
				case LNOT:
				case BNOT:
				case STAR:
				{
					for_initializer();
					break;
				}
				case SEMI:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			match(SEMI);
			{
				switch ( LA(1) )
				{
				case INTEGER_LITERAL:
				case HEXADECIMAL_INTEGER_LITERAL:
				case REAL_LITERAL:
				case CHARACTER_LITERAL:
				case NULL:
				case TRUE:
				case FALSE:
				case REGULAR_STRING_LITERAL:
				case VERBATIM_STRING_LITERAL:
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case DEC:
				case INC:
				case NEW:
				case LPAREN:
				case THIS:
				case BASE:
				case TYPEOF:
				case CHECKED:
				case UNCHECKED:
				case PLUS:
				case MINUS:
				case LNOT:
				case BNOT:
				case STAR:
				{
					for_condition();
					break;
				}
				case SEMI:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			match(SEMI);
			{
				switch ( LA(1) )
				{
				case INTEGER_LITERAL:
				case HEXADECIMAL_INTEGER_LITERAL:
				case REAL_LITERAL:
				case CHARACTER_LITERAL:
				case NULL:
				case TRUE:
				case FALSE:
				case REGULAR_STRING_LITERAL:
				case VERBATIM_STRING_LITERAL:
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case DEC:
				case INC:
				case NEW:
				case LPAREN:
				case THIS:
				case BASE:
				case TYPEOF:
				case CHECKED:
				case UNCHECKED:
				case PLUS:
				case MINUS:
				case LNOT:
				case BNOT:
				case STAR:
				{
					for_iterator();
					break;
				}
				case RPAREN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			match(RPAREN);
			embedded_statement();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  foreach_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			match(FOREACH);
			match(LPAREN);
			type();
			match(IDENTIFIER);
			match(IN);
			expression();
			match(RPAREN);
			embedded_statement();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  for_initializer() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			bool synPredMatched181 = false;
			if (((tokenSet_3_.member(LA(1))) && (LA(2)==IDENTIFIER||LA(2)==DOT||LA(2)==LBRACK)))
			{
				int _m181 = mark();
				synPredMatched181 = true;
				inputState.guessing++;
				try {
					{
						type();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched181 = false;
				}
				rewind(_m181);
				inputState.guessing--;
			}
			if ( synPredMatched181 )
			{
				local_variable_declaration();
			}
			else if ((tokenSet_12_.member(LA(1))) && (tokenSet_56_.member(LA(2)))) {
				statement_expression_list();
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_1_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  for_condition() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			boolean_expression();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_1_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  for_iterator() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			statement_expression_list();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_16_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public void statement_expression_list() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			statement_expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						match(COMMA);
						statement_expression();
					}
					else
					{
						goto _loop186_breakloop;
					}
					
				}
_loop186_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_42_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void break_statement() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			match(BREAK);
			match(SEMI);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void continue_statement() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			match(CONTINUE);
			match(SEMI);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public StatementTree  goto_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		Token  g = null;
		Token  i = null;
		Token  s = null;
		
		t = new StatementTree();
		LinkedList a = new LinkedList ();
		
		
		try {      // for error handling
			bool synPredMatched193 = false;
			if (((LA(1)==GOTO) && (LA(2)==IDENTIFIER)))
			{
				int _m193 = mark();
				synPredMatched193 = true;
				inputState.guessing++;
				try {
					{
						match(GOTO);
						match(IDENTIFIER);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched193 = false;
				}
				rewind(_m193);
				inputState.guessing--;
			}
			if ( synPredMatched193 )
			{
				g = LT(1);
				match(GOTO);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(g));
				}
				i = LT(1);
				match(IDENTIFIER);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(i));
				}
				s = LT(1);
				match(SEMI);
				if (0==inputState.guessing)
				{
					a.Add (new StatementTree(s));
				}
				if (0==inputState.guessing)
				{
					t = new StatementTree("GOTO",a);
				}
			}
			else {
				bool synPredMatched195 = false;
				if (((LA(1)==GOTO) && (LA(2)==CASE)))
				{
					int _m195 = mark();
					synPredMatched195 = true;
					inputState.guessing++;
					try {
						{
							match(GOTO);
							match(CASE);
						}
					}
					catch (RecognitionException)
					{
						synPredMatched195 = false;
					}
					rewind(_m195);
					inputState.guessing--;
				}
				if ( synPredMatched195 )
				{
					match(GOTO);
					match(CASE);
					constant_expression();
					match(SEMI);
				}
				else if ((LA(1)==GOTO) && (LA(2)==DEFAULT)) {
					match(GOTO);
					match(DEFAULT);
					match(SEMI);
				}
				else
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
			}
			catch (RecognitionException ex)
			{
				if (0 == inputState.guessing)
				{
					reportError(ex);
					consume();
					consumeUntil(tokenSet_48_);
				}
				else
				{
					throw;
				}
			}
			return t;
		}
		
	public StatementTree  return_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		Token  r = null;
		Token  s = null;
		
		t = new StatementTree();
		string exp = "";
		LinkedList a = new LinkedList ();
		
		
		try {      // for error handling
			r = LT(1);
			match(RETURN);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(r));
			}
			{
				switch ( LA(1) )
				{
				case INTEGER_LITERAL:
				case HEXADECIMAL_INTEGER_LITERAL:
				case REAL_LITERAL:
				case CHARACTER_LITERAL:
				case NULL:
				case TRUE:
				case FALSE:
				case REGULAR_STRING_LITERAL:
				case VERBATIM_STRING_LITERAL:
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case DEC:
				case INC:
				case NEW:
				case LPAREN:
				case THIS:
				case BASE:
				case TYPEOF:
				case CHECKED:
				case UNCHECKED:
				case PLUS:
				case MINUS:
				case LNOT:
				case BNOT:
				case STAR:
				{
					exp=expression();
					break;
				}
				case SEMI:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(exp));
			}
			s = LT(1);
			match(SEMI);
			if (0==inputState.guessing)
			{
				a.Add (new StatementTree(s));
			}
			if (0==inputState.guessing)
			{
				t = new StatementTree("RETURN",a);
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  throw_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			match(THROW);
			{
				switch ( LA(1) )
				{
				case INTEGER_LITERAL:
				case HEXADECIMAL_INTEGER_LITERAL:
				case REAL_LITERAL:
				case CHARACTER_LITERAL:
				case NULL:
				case TRUE:
				case FALSE:
				case REGULAR_STRING_LITERAL:
				case VERBATIM_STRING_LITERAL:
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case DEC:
				case INC:
				case NEW:
				case LPAREN:
				case THIS:
				case BASE:
				case TYPEOF:
				case CHECKED:
				case UNCHECKED:
				case PLUS:
				case MINUS:
				case LNOT:
				case BNOT:
				case STAR:
				{
					expression();
					break;
				}
				case SEMI:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			match(SEMI);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  end_of_try_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case CATCH:
			{
				catch_clauses();
				{
					switch ( LA(1) )
					{
					case FINALLY:
					{
						finally_clause();
						break;
					}
					case INTEGER_LITERAL:
					case HEXADECIMAL_INTEGER_LITERAL:
					case REAL_LITERAL:
					case CHARACTER_LITERAL:
					case NULL:
					case TRUE:
					case FALSE:
					case REGULAR_STRING_LITERAL:
					case VERBATIM_STRING_LITERAL:
					case IDENTIFIER:
					case OBJECT:
					case STRING:
					case BOOL:
					case DECIMAL:
					case CHAR:
					case INT:
					case LONG:
					case SBYTE:
					case BYTE:
					case SHORT:
					case UINT:
					case ULONG:
					case USHORT:
					case FLOAT:
					case DOUBLE:
					case DEC:
					case INC:
					case NEW:
					case LPAREN:
					case THIS:
					case BASE:
					case TYPEOF:
					case CHECKED:
					case UNCHECKED:
					case PLUS:
					case MINUS:
					case LNOT:
					case BNOT:
					case STAR:
					case CONST:
					case SEMI:
					case LBRACE:
					case RBRACE:
					case IF:
					case ELSE:
					case SWITCH:
					case CASE:
					case DEFAULT:
					case WHILE:
					case DO:
					case FOR:
					case FOREACH:
					case BREAK:
					case CONTINUE:
					case GOTO:
					case RETURN:
					case THROW:
					case TRY:
					case LOCK:
					case USING:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				break;
			}
			case FINALLY:
			{
				finally_clause();
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  catch_clauses() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			bool synPredMatched207 = false;
			if (((LA(1)==CATCH) && (LA(2)==LPAREN)))
			{
				int _m207 = mark();
				synPredMatched207 = true;
				inputState.guessing++;
				try {
					{
						specific_catch_clause();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched207 = false;
				}
				rewind(_m207);
				inputState.guessing--;
			}
			if ( synPredMatched207 )
			{
				{ // ( ... )+
				int _cnt209=0;
				for (;;)
				{
					if ((LA(1)==CATCH) && (LA(2)==LPAREN))
					{
						specific_catch_clause();
					}
					else
					{
						if (_cnt209 >= 1) { goto _loop209_breakloop; } else { throw new NoViableAltException(LT(1), getFilename());; }
					}
					
					_cnt209++;
				}
_loop209_breakloop:				;
				}    // ( ... )+
				{
					switch ( LA(1) )
					{
					case CATCH:
					{
						general_catch_clause();
						break;
					}
					case INTEGER_LITERAL:
					case HEXADECIMAL_INTEGER_LITERAL:
					case REAL_LITERAL:
					case CHARACTER_LITERAL:
					case NULL:
					case TRUE:
					case FALSE:
					case REGULAR_STRING_LITERAL:
					case VERBATIM_STRING_LITERAL:
					case IDENTIFIER:
					case OBJECT:
					case STRING:
					case BOOL:
					case DECIMAL:
					case CHAR:
					case INT:
					case LONG:
					case SBYTE:
					case BYTE:
					case SHORT:
					case UINT:
					case ULONG:
					case USHORT:
					case FLOAT:
					case DOUBLE:
					case DEC:
					case INC:
					case NEW:
					case LPAREN:
					case THIS:
					case BASE:
					case TYPEOF:
					case CHECKED:
					case UNCHECKED:
					case PLUS:
					case MINUS:
					case LNOT:
					case BNOT:
					case STAR:
					case CONST:
					case SEMI:
					case LBRACE:
					case RBRACE:
					case IF:
					case ELSE:
					case SWITCH:
					case CASE:
					case DEFAULT:
					case WHILE:
					case DO:
					case FOR:
					case FOREACH:
					case BREAK:
					case CONTINUE:
					case GOTO:
					case RETURN:
					case THROW:
					case TRY:
					case FINALLY:
					case LOCK:
					case USING:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
			}
			else if ((LA(1)==CATCH) && (LA(2)==LBRACE)) {
				general_catch_clause();
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_57_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  finally_clause() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			match(FINALLY);
			Block();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_48_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  specific_catch_clause() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			bool synPredMatched213 = false;
			if (((LA(1)==CATCH) && (LA(2)==LPAREN)))
			{
				int _m213 = mark();
				synPredMatched213 = true;
				inputState.guessing++;
				try {
					{
						match(CATCH);
						match(LPAREN);
						match(STRING);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched213 = false;
				}
				rewind(_m213);
				inputState.guessing--;
			}
			if ( synPredMatched213 )
			{
				match(CATCH);
				match(LPAREN);
				match(STRING);
				{
					switch ( LA(1) )
					{
					case IDENTIFIER:
					{
						match(IDENTIFIER);
						break;
					}
					case RPAREN:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				match(RPAREN);
				Block();
			}
			else {
				bool synPredMatched216 = false;
				if (((LA(1)==CATCH) && (LA(2)==LPAREN)))
				{
					int _m216 = mark();
					synPredMatched216 = true;
					inputState.guessing++;
					try {
						{
							match(CATCH);
							match(LPAREN);
							match(OBJECT);
						}
					}
					catch (RecognitionException)
					{
						synPredMatched216 = false;
					}
					rewind(_m216);
					inputState.guessing--;
				}
				if ( synPredMatched216 )
				{
					match(CATCH);
					match(LPAREN);
					match(OBJECT);
					{
						switch ( LA(1) )
						{
						case IDENTIFIER:
						{
							match(IDENTIFIER);
							break;
						}
						case RPAREN:
						{
							break;
						}
						default:
						{
							throw new NoViableAltException(LT(1), getFilename());
						}
						 }
					}
					match(RPAREN);
					Block();
				}
				else if ((LA(1)==CATCH) && (LA(2)==LPAREN)) {
					match(CATCH);
					match(LPAREN);
					type_name();
					{
						switch ( LA(1) )
						{
						case IDENTIFIER:
						{
							match(IDENTIFIER);
							break;
						}
						case RPAREN:
						{
							break;
						}
						default:
						{
							throw new NoViableAltException(LT(1), getFilename());
						}
						 }
					}
					match(RPAREN);
					Block();
				}
				else
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
			}
			catch (RecognitionException ex)
			{
				if (0 == inputState.guessing)
				{
					reportError(ex);
					consume();
					consumeUntil(tokenSet_58_);
				}
				else
				{
					throw;
				}
			}
			return t;
		}
		
	public StatementTree  general_catch_clause() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			match(CATCH);
			Block();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_57_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  checked_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			match(CHECKED);
			Block();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_59_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public StatementTree  unchecked_statement() //throws RecognitionException, TokenStreamException
{
		StatementTree t;
		
		
		t = new StatementTree();
		
		
		try {      // for error handling
			match(UNCHECKED);
			Block();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_59_);
			}
			else
			{
				throw;
			}
		}
		return t;
	}
	
	public void resource_acquisition() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched227 = false;
			if (((tokenSet_3_.member(LA(1))) && (LA(2)==IDENTIFIER||LA(2)==DOT||LA(2)==LBRACK)))
			{
				int _m227 = mark();
				synPredMatched227 = true;
				inputState.guessing++;
				try {
					{
						type();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched227 = false;
				}
				rewind(_m227);
				inputState.guessing--;
			}
			if ( synPredMatched227 )
			{
				local_variable_declaration();
			}
			else if ((tokenSet_12_.member(LA(1))) && (tokenSet_60_.member(LA(2)))) {
				expression();
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_16_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void compilation_unit() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==USING))
					{
						using_directive();
					}
					else
					{
						goto _loop230_breakloop;
					}
					
				}
_loop230_breakloop:				;
			}    // ( ... )*
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==LBRACK) && (LA(2)==IDENTIFIER))
					{
						global_attributes();
					}
					else
					{
						goto _loop232_breakloop;
					}
					
				}
_loop232_breakloop:				;
			}    // ( ... )*
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_61_.member(LA(1))))
					{
						namespace_member_declaration();
					}
					else
					{
						goto _loop234_breakloop;
					}
					
				}
_loop234_breakloop:				;
			}    // ( ... )*
			if (0==inputState.guessing)
			{
				
				Emit.EmitString ("\n");
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_59_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void using_directive() //throws RecognitionException, TokenStreamException
{
		
		Token  u1 = null;
		Token  id = null;
		Token  a = null;
		Token  s1 = null;
		Token  u2 = null;
		Token  s2 = null;
		
		string nn = "";
		
		
		try {      // for error handling
			bool synPredMatched247 = false;
			if (((LA(1)==USING) && (LA(2)==IDENTIFIER)))
			{
				int _m247 = mark();
				synPredMatched247 = true;
				inputState.guessing++;
				try {
					{
						match(USING);
						match(IDENTIFIER);
						match(ASSIGN);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched247 = false;
				}
				rewind(_m247);
				inputState.guessing--;
			}
			if ( synPredMatched247 )
			{
				u1 = LT(1);
				match(USING);
				id = LT(1);
				match(IDENTIFIER);
				a = LT(1);
				match(ASSIGN);
				if (0==inputState.guessing)
				{
					
					Emit.EmitToken (u1);
					Emit.EmitToken (id);
					Emit.EmitToken (a);
					
				}
				nn=namespace_or_type_name();
				s1 = LT(1);
				match(SEMI);
				if (0==inputState.guessing)
				{
					
					Emit.EmitString (nn);
					Emit.EmitToken (s1);
					
				}
			}
			else if ((LA(1)==USING) && (LA(2)==IDENTIFIER)) {
				u2 = LT(1);
				match(USING);
				if (0==inputState.guessing)
				{
					
					Emit.EmitToken (u2);
					
				}
				nn=namespace_name();
				s2 = LT(1);
				match(SEMI);
				if (0==inputState.guessing)
				{
					
					Emit.EmitString (nn);
					Emit.EmitToken (s2);
					
				}
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_62_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void global_attributes() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{ // ( ... )+
			int _cnt590=0;
			for (;;)
			{
				if ((LA(1)==LBRACK) && (LA(2)==IDENTIFIER))
				{
					global_attribute_section();
				}
				else
				{
					if (_cnt590 >= 1) { goto _loop590_breakloop; } else { throw new NoViableAltException(LT(1), getFilename());; }
				}
				
				_cnt590++;
			}
_loop590_breakloop:			;
			}    // ( ... )+
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_63_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void namespace_member_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NAMESPACE:
			{
				namespace_declaration();
				break;
			}
			case LBRACK:
			case NEW:
			case ENUM:
			case STRUCT:
			case INTERFACE:
			case CLASS:
			case PUBLIC:
			case PROTECTED:
			case INTERNAL:
			case PRIVATE:
			case SEALED:
			case ABSTRACT:
			case DELEGATE:
			{
				type_declaration();
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_64_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void namespace_declaration() //throws RecognitionException, TokenStreamException
{
		
		Token  ns = null;
		Token  s = null;
		
		string semi = "";
		string qi;
		
		
		try {      // for error handling
			ns = LT(1);
			match(NAMESPACE);
			qi=qualified_identifier();
			if (0==inputState.guessing)
			{
				
				Emit.EmitToken (ns);
				Emit.EmitString (qi);
				
			}
			namespace_body();
			{
				switch ( LA(1) )
				{
				case SEMI:
				{
					s = LT(1);
					match(SEMI);
					if (0==inputState.guessing)
					{
						semi = s.getText();
					}
					break;
				}
				case EOF:
				case LBRACK:
				case NEW:
				case RBRACE:
				case NAMESPACE:
				case ENUM:
				case STRUCT:
				case INTERFACE:
				case CLASS:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case SEALED:
				case ABSTRACT:
				case DELEGATE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			if (0==inputState.guessing)
			{
				
				Emit.EmitString (semi);
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_64_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  qualified_identifier() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  id1 = null;
		Token  id2 = null;
		
		return_string = "";
		
		
		try {      // for error handling
			id1 = LT(1);
			match(IDENTIFIER);
			if (0==inputState.guessing)
			{
				return_string = id1.getText();
			}
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==DOT))
					{
						match(DOT);
						id2 = LT(1);
						match(IDENTIFIER);
						if (0==inputState.guessing)
						{
							return_string += ("." + id2.getText());
						}
					}
					else
					{
						goto _loop239_breakloop;
					}
					
				}
_loop239_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_65_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void namespace_body() //throws RecognitionException, TokenStreamException
{
		
		Token  lb = null;
		Token  rb = null;
		
		try {      // for error handling
			lb = LT(1);
			match(LBRACE);
			if (0==inputState.guessing)
			{
				
				Emit.EmitToken (lb);
				
			}
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==USING))
					{
						using_directive();
					}
					else
					{
						goto _loop242_breakloop;
					}
					
				}
_loop242_breakloop:				;
			}    // ( ... )*
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_61_.member(LA(1))))
					{
						namespace_member_declaration();
					}
					else
					{
						goto _loop244_breakloop;
					}
					
				}
_loop244_breakloop:				;
			}    // ( ... )*
			rb = LT(1);
			match(RBRACE);
			if (0==inputState.guessing)
			{
				
				Emit.EmitToken (rb);
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_66_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void type_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched254 = false;
			if (((tokenSet_67_.member(LA(1))) && (tokenSet_68_.member(LA(2)))))
			{
				int _m254 = mark();
				synPredMatched254 = true;
				inputState.guessing++;
				try {
					{
						{
							switch ( LA(1) )
							{
							case LBRACK:
							{
								attributes();
								break;
							}
							case NEW:
							case ENUM:
							case PUBLIC:
							case PROTECTED:
							case INTERNAL:
							case PRIVATE:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							 }
						}
						{    // ( ... )*
							for (;;)
							{
								if ((tokenSet_69_.member(LA(1))))
								{
									enum_modifier();
								}
								else
								{
									goto _loop253_breakloop;
								}
								
							}
_loop253_breakloop:							;
						}    // ( ... )*
						match(ENUM);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched254 = false;
				}
				rewind(_m254);
				inputState.guessing--;
			}
			if ( synPredMatched254 )
			{
				enum_declaration();
			}
			else {
				bool synPredMatched259 = false;
				if (((tokenSet_70_.member(LA(1))) && (tokenSet_71_.member(LA(2)))))
				{
					int _m259 = mark();
					synPredMatched259 = true;
					inputState.guessing++;
					try {
						{
							{
								switch ( LA(1) )
								{
								case LBRACK:
								{
									attributes();
									break;
								}
								case NEW:
								case STRUCT:
								case PUBLIC:
								case PROTECTED:
								case INTERNAL:
								case PRIVATE:
								{
									break;
								}
								default:
								{
									throw new NoViableAltException(LT(1), getFilename());
								}
								 }
							}
							{    // ( ... )*
								for (;;)
								{
									if ((tokenSet_69_.member(LA(1))))
									{
										struct_modifier();
									}
									else
									{
										goto _loop258_breakloop;
									}
									
								}
_loop258_breakloop:								;
							}    // ( ... )*
							match(STRUCT);
						}
					}
					catch (RecognitionException)
					{
						synPredMatched259 = false;
					}
					rewind(_m259);
					inputState.guessing--;
				}
				if ( synPredMatched259 )
				{
					struct_declaration();
				}
				else {
					bool synPredMatched264 = false;
					if (((tokenSet_72_.member(LA(1))) && (tokenSet_73_.member(LA(2)))))
					{
						int _m264 = mark();
						synPredMatched264 = true;
						inputState.guessing++;
						try {
							{
								{
									switch ( LA(1) )
									{
									case LBRACK:
									{
										attributes();
										break;
									}
									case NEW:
									case INTERFACE:
									case PUBLIC:
									case PROTECTED:
									case INTERNAL:
									case PRIVATE:
									{
										break;
									}
									default:
									{
										throw new NoViableAltException(LT(1), getFilename());
									}
									 }
								}
								{    // ( ... )*
									for (;;)
									{
										if ((tokenSet_69_.member(LA(1))))
										{
											interface_modifier();
										}
										else
										{
											goto _loop263_breakloop;
										}
										
									}
_loop263_breakloop:									;
								}    // ( ... )*
								match(INTERFACE);
							}
						}
						catch (RecognitionException)
						{
							synPredMatched264 = false;
						}
						rewind(_m264);
						inputState.guessing--;
					}
					if ( synPredMatched264 )
					{
						interface_declaration();
					}
					else {
						bool synPredMatched269 = false;
						if (((tokenSet_74_.member(LA(1))) && (tokenSet_75_.member(LA(2)))))
						{
							int _m269 = mark();
							synPredMatched269 = true;
							inputState.guessing++;
							try {
								{
									{
										switch ( LA(1) )
										{
										case LBRACK:
										{
											attributes();
											break;
										}
										case NEW:
										case CLASS:
										case PUBLIC:
										case PROTECTED:
										case INTERNAL:
										case PRIVATE:
										case SEALED:
										case ABSTRACT:
										{
											break;
										}
										default:
										{
											throw new NoViableAltException(LT(1), getFilename());
										}
										 }
									}
									{    // ( ... )*
										for (;;)
										{
											if ((tokenSet_76_.member(LA(1))))
											{
												class_modifier();
											}
											else
											{
												goto _loop268_breakloop;
											}
											
										}
_loop268_breakloop:										;
									}    // ( ... )*
									match(CLASS);
									match(IDENTIFIER);
								}
							}
							catch (RecognitionException)
							{
								synPredMatched269 = false;
							}
							rewind(_m269);
							inputState.guessing--;
						}
						if ( synPredMatched269 )
						{
							class_declaration();
						}
						else if ((tokenSet_77_.member(LA(1))) && (tokenSet_78_.member(LA(2)))) {
							delegate_declaration();
						}
						else
						{
							throw new NoViableAltException(LT(1), getFilename());
						}
						}}}
					}
					catch (RecognitionException ex)
					{
						if (0 == inputState.guessing)
						{
							reportError(ex);
							consume();
							consumeUntil(tokenSet_79_);
						}
						else
						{
							throw;
						}
					}
				}
				
	public void attributes() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{ // ( ... )+
			int _cnt597=0;
			for (;;)
			{
				if ((LA(1)==LBRACK))
				{
					attribute_section();
				}
				else
				{
					if (_cnt597 >= 1) { goto _loop597_breakloop; } else { throw new NoViableAltException(LT(1), getFilename());; }
				}
				
				_cnt597++;
			}
_loop597_breakloop:			;
			}    // ( ... )+
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_80_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void enum_modifier() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NEW:
			{
				match(NEW);
				break;
			}
			case PUBLIC:
			{
				match(PUBLIC);
				break;
			}
			case PROTECTED:
			{
				match(PROTECTED);
				break;
			}
			case INTERNAL:
			{
				match(INTERNAL);
				break;
			}
			case PRIVATE:
			{
				match(PRIVATE);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_81_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void enum_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case NEW:
				case ENUM:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_69_.member(LA(1))))
					{
						enum_modifier();
					}
					else
					{
						goto _loop564_breakloop;
					}
					
				}
_loop564_breakloop:				;
			}    // ( ... )*
			match(ENUM);
			match(IDENTIFIER);
			{
				switch ( LA(1) )
				{
				case COLON:
				{
					enum_base();
					break;
				}
				case LBRACE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			enum_body();
			{
				switch ( LA(1) )
				{
				case SEMI:
				{
					match(SEMI);
					break;
				}
				case EOF:
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case LBRACK:
				case NEW:
				case VOID:
				case BNOT:
				case CONST:
				case RBRACE:
				case NAMESPACE:
				case ENUM:
				case STRUCT:
				case INTERFACE:
				case CLASS:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case SEALED:
				case ABSTRACT:
				case STATIC:
				case READONLY:
				case VOLATILE:
				case VIRTUAL:
				case OVERRIDE:
				case EXTERN:
				case EVENT:
				case ABSTARCT:
				case DELEGATE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_79_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void struct_modifier() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NEW:
			{
				match(NEW);
				break;
			}
			case PUBLIC:
			{
				match(PUBLIC);
				break;
			}
			case PROTECTED:
			{
				match(PROTECTED);
				break;
			}
			case INTERNAL:
			{
				match(INTERNAL);
				break;
			}
			case PRIVATE:
			{
				match(PRIVATE);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_82_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void struct_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case NEW:
				case STRUCT:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_69_.member(LA(1))))
					{
						struct_modifier();
					}
					else
					{
						goto _loop446_breakloop;
					}
					
				}
_loop446_breakloop:				;
			}    // ( ... )*
			match(STRUCT);
			match(IDENTIFIER);
			{
				switch ( LA(1) )
				{
				case COLON:
				{
					struct_interfaces();
					break;
				}
				case LBRACE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			struct_body();
			{
				switch ( LA(1) )
				{
				case SEMI:
				{
					match(SEMI);
					break;
				}
				case EOF:
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case LBRACK:
				case NEW:
				case VOID:
				case BNOT:
				case CONST:
				case RBRACE:
				case NAMESPACE:
				case ENUM:
				case STRUCT:
				case INTERFACE:
				case CLASS:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case SEALED:
				case ABSTRACT:
				case STATIC:
				case READONLY:
				case VOLATILE:
				case VIRTUAL:
				case OVERRIDE:
				case EXTERN:
				case EVENT:
				case ABSTARCT:
				case DELEGATE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_79_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void interface_modifier() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NEW:
			{
				match(NEW);
				break;
			}
			case PUBLIC:
			{
				match(PUBLIC);
				break;
			}
			case PROTECTED:
			{
				match(PROTECTED);
				break;
			}
			case INTERNAL:
			{
				match(INTERNAL);
				break;
			}
			case PRIVATE:
			{
				match(PRIVATE);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_83_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void interface_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case NEW:
				case INTERFACE:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_69_.member(LA(1))))
					{
						interface_modifier();
					}
					else
					{
						goto _loop507_breakloop;
					}
					
				}
_loop507_breakloop:				;
			}    // ( ... )*
			match(INTERFACE);
			match(IDENTIFIER);
			{
				switch ( LA(1) )
				{
				case COLON:
				{
					interface_base();
					break;
				}
				case LBRACE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			interface_body();
			{
				switch ( LA(1) )
				{
				case SEMI:
				{
					match(SEMI);
					break;
				}
				case EOF:
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case LBRACK:
				case NEW:
				case VOID:
				case BNOT:
				case CONST:
				case RBRACE:
				case NAMESPACE:
				case ENUM:
				case STRUCT:
				case INTERFACE:
				case CLASS:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case SEALED:
				case ABSTRACT:
				case STATIC:
				case READONLY:
				case VOLATILE:
				case VIRTUAL:
				case OVERRIDE:
				case EXTERN:
				case EVENT:
				case ABSTARCT:
				case DELEGATE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_79_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void class_modifier() //throws RecognitionException, TokenStreamException
{
		
		Token  cm1 = null;
		Token  cm2 = null;
		Token  cm3 = null;
		Token  cm4 = null;
		Token  cm5 = null;
		Token  cm6 = null;
		Token  cm7 = null;
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NEW:
			{
				cm1 = LT(1);
				match(NEW);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm1);
				}
				break;
			}
			case PUBLIC:
			{
				cm2 = LT(1);
				match(PUBLIC);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm2);
				}
				break;
			}
			case PROTECTED:
			{
				cm3 = LT(1);
				match(PROTECTED);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm3);
				}
				break;
			}
			case INTERNAL:
			{
				cm4 = LT(1);
				match(INTERNAL);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm4);
				}
				break;
			}
			case PRIVATE:
			{
				cm5 = LT(1);
				match(PRIVATE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm5);
				}
				break;
			}
			case SEALED:
			{
				cm6 = LT(1);
				match(SEALED);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm6);
				}
				break;
			}
			case ABSTRACT:
			{
				cm7 = LT(1);
				match(ABSTRACT);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm7);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_84_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void class_declaration() //throws RecognitionException, TokenStreamException
{
		
		Token  c = null;
		Token  id = null;
		Token  s = null;
		
		string semi = "";
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case NEW:
				case CLASS:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case SEALED:
				case ABSTRACT:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_76_.member(LA(1))))
					{
						class_modifier();
					}
					else
					{
						goto _loop273_breakloop;
					}
					
				}
_loop273_breakloop:				;
			}    // ( ... )*
			c = LT(1);
			match(CLASS);
			id = LT(1);
			match(IDENTIFIER);
			if (0==inputState.guessing)
			{
				
				Emit.EmitToken (c);
				Emit.EmitToken (id);
				
			}
			{
				switch ( LA(1) )
				{
				case COLON:
				{
					class_base();
					break;
				}
				case LBRACE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			class_body();
			{
				switch ( LA(1) )
				{
				case SEMI:
				{
					s = LT(1);
					match(SEMI);
					if (0==inputState.guessing)
					{
						semi = s.getText();
					}
					break;
				}
				case EOF:
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case LBRACK:
				case NEW:
				case VOID:
				case BNOT:
				case CONST:
				case RBRACE:
				case NAMESPACE:
				case ENUM:
				case STRUCT:
				case INTERFACE:
				case CLASS:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case SEALED:
				case ABSTRACT:
				case STATIC:
				case READONLY:
				case VOLATILE:
				case VIRTUAL:
				case OVERRIDE:
				case EXTERN:
				case EVENT:
				case ABSTARCT:
				case DELEGATE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			if (0==inputState.guessing)
			{
				
				Emit.EmitString (semi);
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_79_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void delegate_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case NEW:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case DELEGATE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_69_.member(LA(1))))
					{
						delegate_modifier();
					}
					else
					{
						goto _loop585_breakloop;
					}
					
				}
_loop585_breakloop:				;
			}    // ( ... )*
			match(DELEGATE);
			return_type();
			match(IDENTIFIER);
			match(LPAREN);
			{
				switch ( LA(1) )
				{
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case LBRACK:
				case REF:
				case OUT:
				case PARAMS:
				{
					formal_parameter_list();
					break;
				}
				case RPAREN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			match(RPAREN);
			match(SEMI);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_79_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void class_base() //throws RecognitionException, TokenStreamException
{
		
		Token  cl = null;
		Token  cm = null;
		
		string t1 = "";
		string t2 = "";
		
		
		try {      // for error handling
			cl = LT(1);
			match(COLON);
			if (0==inputState.guessing)
			{
				
				Emit.EmitToken (cl);
				
			}
			t1=type_name();
			if (0==inputState.guessing)
			{
				
				Emit.EmitString (t1);
				
			}
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						cm = LT(1);
						match(COMMA);
						if (0==inputState.guessing)
						{
							Emit.EmitToken (cm);
						}
						t2=type_name();
						if (0==inputState.guessing)
						{
							Emit.EmitString (t2);
						}
					}
					else
					{
						goto _loop279_breakloop;
					}
					
				}
_loop279_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_65_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void class_body() //throws RecognitionException, TokenStreamException
{
		
		Token  lb = null;
		Token  rb = null;
		
		try {      // for error handling
			lb = LT(1);
			match(LBRACE);
			if (0==inputState.guessing)
			{
				
				Emit.EmitToken (lb);
				
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_85_.member(LA(1))))
					{
						class_member_declaration();
					}
					else
					{
						goto _loop282_breakloop;
					}
					
				}
_loop282_breakloop:				;
			}    // ( ... )*
			rb = LT(1);
			match(RBRACE);
			if (0==inputState.guessing)
			{
				
				Emit.EmitToken (rb);
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_86_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void class_member_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched285 = false;
			if (((tokenSet_87_.member(LA(1))) && (tokenSet_88_.member(LA(2)))))
			{
				int _m285 = mark();
				synPredMatched285 = true;
				inputState.guessing++;
				try {
					{
						constant_declaration();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched285 = false;
				}
				rewind(_m285);
				inputState.guessing--;
			}
			if ( synPredMatched285 )
			{
				constant_declaration();
			}
			else {
				bool synPredMatched287 = false;
				if (((tokenSet_89_.member(LA(1))) && (tokenSet_90_.member(LA(2)))))
				{
					int _m287 = mark();
					synPredMatched287 = true;
					inputState.guessing++;
					try {
						{
							field_declaration();
						}
					}
					catch (RecognitionException)
					{
						synPredMatched287 = false;
					}
					rewind(_m287);
					inputState.guessing--;
				}
				if ( synPredMatched287 )
				{
					field_declaration();
				}
				else {
					bool synPredMatched289 = false;
					if (((tokenSet_91_.member(LA(1))) && (tokenSet_92_.member(LA(2)))))
					{
						int _m289 = mark();
						synPredMatched289 = true;
						inputState.guessing++;
						try {
							{
								method_declaration();
							}
						}
						catch (RecognitionException)
						{
							synPredMatched289 = false;
						}
						rewind(_m289);
						inputState.guessing--;
					}
					if ( synPredMatched289 )
					{
						method_declaration();
					}
					else {
						bool synPredMatched291 = false;
						if (((tokenSet_93_.member(LA(1))) && (tokenSet_94_.member(LA(2)))))
						{
							int _m291 = mark();
							synPredMatched291 = true;
							inputState.guessing++;
							try {
								{
									property_declaration();
								}
							}
							catch (RecognitionException)
							{
								synPredMatched291 = false;
							}
							rewind(_m291);
							inputState.guessing--;
						}
						if ( synPredMatched291 )
						{
							property_declaration();
						}
						else {
							bool synPredMatched293 = false;
							if (((tokenSet_95_.member(LA(1))) && (tokenSet_96_.member(LA(2)))))
							{
								int _m293 = mark();
								synPredMatched293 = true;
								inputState.guessing++;
								try {
									{
										event_declaration();
									}
								}
								catch (RecognitionException)
								{
									synPredMatched293 = false;
								}
								rewind(_m293);
								inputState.guessing--;
							}
							if ( synPredMatched293 )
							{
								event_declaration();
							}
							else {
								bool synPredMatched295 = false;
								if (((tokenSet_93_.member(LA(1))) && (tokenSet_97_.member(LA(2)))))
								{
									int _m295 = mark();
									synPredMatched295 = true;
									inputState.guessing++;
									try {
										{
											indexer_declaration();
										}
									}
									catch (RecognitionException)
									{
										synPredMatched295 = false;
									}
									rewind(_m295);
									inputState.guessing--;
								}
								if ( synPredMatched295 )
								{
									indexer_declaration();
								}
								else {
									bool synPredMatched297 = false;
									if (((tokenSet_98_.member(LA(1))) && (tokenSet_99_.member(LA(2)))))
									{
										int _m297 = mark();
										synPredMatched297 = true;
										inputState.guessing++;
										try {
											{
												operator_declaration();
											}
										}
										catch (RecognitionException)
										{
											synPredMatched297 = false;
										}
										rewind(_m297);
										inputState.guessing--;
									}
									if ( synPredMatched297 )
									{
										operator_declaration();
									}
									else {
										bool synPredMatched299 = false;
										if (((tokenSet_100_.member(LA(1))) && (tokenSet_101_.member(LA(2)))))
										{
											int _m299 = mark();
											synPredMatched299 = true;
											inputState.guessing++;
											try {
												{
													constructor_declaration();
												}
											}
											catch (RecognitionException)
											{
												synPredMatched299 = false;
											}
											rewind(_m299);
											inputState.guessing--;
										}
										if ( synPredMatched299 )
										{
											constructor_declaration();
										}
										else {
											bool synPredMatched301 = false;
											if (((LA(1)==LBRACK||LA(1)==BNOT||LA(1)==EXTERN) && (LA(2)==IDENTIFIER||LA(2)==BNOT)))
											{
												int _m301 = mark();
												synPredMatched301 = true;
												inputState.guessing++;
												try {
													{
														destructor_declaration();
													}
												}
												catch (RecognitionException)
												{
													synPredMatched301 = false;
												}
												rewind(_m301);
												inputState.guessing--;
											}
											if ( synPredMatched301 )
											{
												destructor_declaration();
											}
											else {
												bool synPredMatched303 = false;
												if (((LA(1)==LBRACK||LA(1)==STATIC||LA(1)==EXTERN) && (LA(2)==IDENTIFIER||LA(2)==STATIC||LA(2)==EXTERN)))
												{
													int _m303 = mark();
													synPredMatched303 = true;
													inputState.guessing++;
													try {
														{
															static_constructor_declaration();
														}
													}
													catch (RecognitionException)
													{
														synPredMatched303 = false;
													}
													rewind(_m303);
													inputState.guessing--;
												}
												if ( synPredMatched303 )
												{
													static_constructor_declaration();
												}
												else if ((tokenSet_102_.member(LA(1))) && (tokenSet_103_.member(LA(2)))) {
													type_declaration();
												}
												else
												{
													throw new NoViableAltException(LT(1), getFilename());
												}
												}}}}}}}}}
											}
											catch (RecognitionException ex)
											{
												if (0 == inputState.guessing)
												{
													reportError(ex);
													consume();
													consumeUntil(tokenSet_104_);
												}
												else
												{
													throw;
												}
											}
										}
										
	public void constant_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		string t = "";
		
		
		try {      // for error handling
			if (0==inputState.guessing)
			{
				Emit.BeginBuffer ();
			}
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case NEW:
				case CONST:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_69_.member(LA(1))))
					{
						constant_modifier();
					}
					else
					{
						goto _loop307_breakloop;
					}
					
				}
_loop307_breakloop:				;
			}    // ( ... )*
			if (0==inputState.guessing)
			{
				Emit.EndBuffer () ;
			}
			match(CONST);
			t=type();
			constant_declarator(false,Emit.Buffer,t);
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						match(COMMA);
						constant_declarator(true,Emit.Buffer,t);
					}
					else
					{
						goto _loop309_breakloop;
					}
					
				}
_loop309_breakloop:				;
			}    // ( ... )*
			match(SEMI);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void field_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		string t = "";
		bool is_readonly = false;
		bool ret;
		
		
		try {      // for error handling
			if (0==inputState.guessing)
			{
				Emit.BeginBuffer ();
			}
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case NEW:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case STATIC:
				case READONLY:
				case VOLATILE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_105_.member(LA(1))))
					{
						ret=field_modifier();
						if (0==inputState.guessing)
						{
							is_readonly = is_readonly || ret;
						}
					}
					else
					{
						goto _loop315_breakloop;
					}
					
				}
_loop315_breakloop:				;
			}    // ( ... )*
			if (0==inputState.guessing)
			{
				
				if (!is_readonly)
				Emit.EmitString(" mutable ");
				Emit.EndBuffer () ;
				
			}
			t=type();
			variable_declarator(false,Emit.Buffer,t);
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						match(COMMA);
						variable_declarator(true,Emit.Buffer,t);
					}
					else
					{
						goto _loop317_breakloop;
					}
					
				}
_loop317_breakloop:				;
			}    // ( ... )*
			match(SEMI);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void method_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			method_header();
			method_body();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void property_declaration() //throws RecognitionException, TokenStreamException
{
		
		Token  lb = null;
		Token  rb = null;
		
		string t="";
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case NEW:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case SEALED:
				case ABSTRACT:
				case STATIC:
				case VIRTUAL:
				case OVERRIDE:
				case EXTERN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_106_.member(LA(1))))
					{
						property_modifier();
					}
					else
					{
						goto _loop356_breakloop;
					}
					
				}
_loop356_breakloop:				;
			}    // ( ... )*
			t=type();
			member_name();
			if (0==inputState.guessing)
			{
				
				Emit.EmitString(" : " + t);
				
			}
			lb = LT(1);
			match(LBRACE);
			if (0==inputState.guessing)
			{
				Emit.EmitToken (lb);
			}
			accessor_declarations();
			rb = LT(1);
			match(RBRACE);
			if (0==inputState.guessing)
			{
				Emit.EmitToken (rb);
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  event_declaration() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched373 = false;
			if (((tokenSet_95_.member(LA(1))) && (tokenSet_96_.member(LA(2)))))
			{
				int _m373 = mark();
				synPredMatched373 = true;
				inputState.guessing++;
				try {
					{
						{
							switch ( LA(1) )
							{
							case LBRACK:
							{
								attributes();
								break;
							}
							case NEW:
							case PUBLIC:
							case PROTECTED:
							case INTERNAL:
							case PRIVATE:
							case SEALED:
							case STATIC:
							case VIRTUAL:
							case OVERRIDE:
							case EXTERN:
							case EVENT:
							case ABSTARCT:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							 }
						}
						{    // ( ... )*
							for (;;)
							{
								if ((tokenSet_107_.member(LA(1))))
								{
									event_modifier();
								}
								else
								{
									goto _loop372_breakloop;
								}
								
							}
_loop372_breakloop:							;
						}    // ( ... )*
						match(EVENT);
						type();
						member_name();
						match(RBRACE);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched373 = false;
				}
				rewind(_m373);
				inputState.guessing--;
			}
			if ( synPredMatched373 )
			{
				{
					switch ( LA(1) )
					{
					case LBRACK:
					{
						attributes();
						break;
					}
					case NEW:
					case PUBLIC:
					case PROTECTED:
					case INTERNAL:
					case PRIVATE:
					case SEALED:
					case STATIC:
					case VIRTUAL:
					case OVERRIDE:
					case EXTERN:
					case EVENT:
					case ABSTARCT:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				{    // ( ... )*
					for (;;)
					{
						if ((tokenSet_107_.member(LA(1))))
						{
							event_modifier();
						}
						else
						{
							goto _loop376_breakloop;
						}
						
					}
_loop376_breakloop:					;
				}    // ( ... )*
				match(EVENT);
				type();
				member_name();
				match(RBRACE);
				event_accessor_declarations();
				match(RBRACE);
			}
			else if ((tokenSet_95_.member(LA(1))) && (tokenSet_96_.member(LA(2)))) {
				{
					switch ( LA(1) )
					{
					case LBRACK:
					{
						attributes();
						break;
					}
					case NEW:
					case PUBLIC:
					case PROTECTED:
					case INTERNAL:
					case PRIVATE:
					case SEALED:
					case STATIC:
					case VIRTUAL:
					case OVERRIDE:
					case EXTERN:
					case EVENT:
					case ABSTARCT:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				{    // ( ... )*
					for (;;)
					{
						if ((tokenSet_107_.member(LA(1))))
						{
							event_modifier();
						}
						else
						{
							goto _loop379_breakloop;
						}
						
					}
_loop379_breakloop:					;
				}    // ( ... )*
				match(EVENT);
				type();
				variable_declarator(false,"","");
				{    // ( ... )*
					for (;;)
					{
						if ((LA(1)==COMMA))
						{
							match(COMMA);
							variable_declarator(false,"","");
						}
						else
						{
							goto _loop381_breakloop;
						}
						
					}
_loop381_breakloop:					;
				}    // ( ... )*
				match(SEMI);
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void indexer_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case NEW:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case SEALED:
				case ABSTRACT:
				case STATIC:
				case VIRTUAL:
				case OVERRIDE:
				case EXTERN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_106_.member(LA(1))))
					{
						indexer_modifier();
					}
					else
					{
						goto _loop394_breakloop;
					}
					
				}
_loop394_breakloop:				;
			}    // ( ... )*
			indexer_declarator();
			match(LBRACE);
			accessor_declarations();
			match(RBRACE);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  operator_declaration() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case PUBLIC:
				case STATIC:
				case EXTERN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{ // ( ... )+
			int _cnt402=0;
			for (;;)
			{
				if ((LA(1)==PUBLIC||LA(1)==STATIC||LA(1)==EXTERN))
				{
					operator_modifier();
				}
				else
				{
					if (_cnt402 >= 1) { goto _loop402_breakloop; } else { throw new NoViableAltException(LT(1), getFilename());; }
				}
				
				_cnt402++;
			}
_loop402_breakloop:			;
			}    // ( ... )+
			operator_declarator();
			operator_body();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void constructor_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		string cd = "";
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case IDENTIFIER:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case EXTERN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_108_.member(LA(1))))
					{
						constructor_modifier();
					}
					else
					{
						goto _loop418_breakloop;
					}
					
				}
_loop418_breakloop:				;
			}    // ( ... )*
			cd=constructor_declarator();
			constructor_body(cd);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  destructor_declaration() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case BNOT:
				case EXTERN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{
				switch ( LA(1) )
				{
				case EXTERN:
				{
					match(EXTERN);
					break;
				}
				case BNOT:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			match(BNOT);
			match(IDENTIFIER);
			match(LPAREN);
			match(RPAREN);
			destructor_body();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  static_constructor_declaration() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case STATIC:
				case EXTERN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			static_constructor_modifiers();
			match(IDENTIFIER);
			match(LPAREN);
			match(RPAREN);
			static_constructor_body();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void constant_modifier() //throws RecognitionException, TokenStreamException
{
		
		Token  cm1 = null;
		Token  cm2 = null;
		Token  cm3 = null;
		Token  cm4 = null;
		Token  cm5 = null;
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NEW:
			{
				cm1 = LT(1);
				match(NEW);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm1);
				}
				break;
			}
			case PUBLIC:
			{
				cm2 = LT(1);
				match(PUBLIC);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm2);
				}
				break;
			}
			case PROTECTED:
			{
				cm3 = LT(1);
				match(PROTECTED);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm3);
				}
				break;
			}
			case INTERNAL:
			{
				cm4 = LT(1);
				match(INTERNAL);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm4);
				}
				break;
			}
			case PRIVATE:
			{
				cm5 = LT(1);
				match(PRIVATE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm5);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_109_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void constant_declarator(
		bool emit_prefix,string prefix,string t
	) //throws RecognitionException, TokenStreamException
{
		
		Token  id = null;
		Token  a = null;
		
		string ce = "";
		
		
		try {      // for error handling
			if (0==inputState.guessing)
			{
				
				if(emit_prefix)
				Emit.EmitString (prefix); 
				
			}
			id = LT(1);
			match(IDENTIFIER);
			a = LT(1);
			match(ASSIGN);
			if (0==inputState.guessing)
			{
				
				if(!prefix.EndsWith(" ") && !id.getText().StartsWith(" "))
				Emit.EmitString (" " + id.getText());
				else
				Emit.EmitToken (id);
				Emit.EmitString (" : ");
				Emit.EmitString (t);            
				Emit.EmitToken (a);
				
			}
			ce=constant_expression();
			if (0==inputState.guessing)
			{
				
				Emit.EmitString (ce);
				Emit.EmitString (";");
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_52_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public bool  field_modifier() //throws RecognitionException, TokenStreamException
{
		bool is_readonly;
		
		Token  fm1 = null;
		Token  fm2 = null;
		Token  fm3 = null;
		Token  fm4 = null;
		Token  fm5 = null;
		Token  fm6 = null;
		Token  fm7 = null;
		Token  fm8 = null;
		
		is_readonly = false;
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NEW:
			{
				fm1 = LT(1);
				match(NEW);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (fm1);
				}
				break;
			}
			case PUBLIC:
			{
				fm2 = LT(1);
				match(PUBLIC);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (fm2);
				}
				break;
			}
			case PROTECTED:
			{
				fm3 = LT(1);
				match(PROTECTED);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (fm3);
				}
				break;
			}
			case INTERNAL:
			{
				fm4 = LT(1);
				match(INTERNAL);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (fm4);
				}
				break;
			}
			case PRIVATE:
			{
				fm5 = LT(1);
				match(PRIVATE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (fm5);
				}
				break;
			}
			case STATIC:
			{
				fm6 = LT(1);
				match(STATIC);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (fm6);
				}
				break;
			}
			case READONLY:
			{
				fm7 = LT(1);
				match(READONLY);
				if (0==inputState.guessing)
				{
					is_readonly = true;
				}
				break;
			}
			case VOLATILE:
			{
				fm8 = LT(1);
				match(VOLATILE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (fm8);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_110_);
			}
			else
			{
				throw;
			}
		}
		return is_readonly;
	}
	
	public void variable_declarator(
		bool emit_prefix,string prefix,string t
	) //throws RecognitionException, TokenStreamException
{
		
		Token  id = null;
		Token  a = null;
		
		try {      // for error handling
			if (0==inputState.guessing)
			{
				
				if(emit_prefix)
				Emit.EmitString(prefix);
				
			}
			id = LT(1);
			match(IDENTIFIER);
			if (0==inputState.guessing)
			{
				
				Emit.EmitToken (id);
				Emit.EmitString (" : ");
				Emit.EmitString (t);
				
			}
			{
				switch ( LA(1) )
				{
				case ASSIGN:
				{
					a = LT(1);
					match(ASSIGN);
					if (0==inputState.guessing)
					{
						Emit.EmitToken (a);
					}
					variable_initializer();
					break;
				}
				case COMMA:
				case SEMI:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			if (0==inputState.guessing)
			{
				Emit.EmitString (";");
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_52_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void variable_initializer() //throws RecognitionException, TokenStreamException
{
		
		
		string return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case LBRACE:
			{
				array_initializer();
				break;
			}
			case INTEGER_LITERAL:
			case HEXADECIMAL_INTEGER_LITERAL:
			case REAL_LITERAL:
			case CHARACTER_LITERAL:
			case NULL:
			case TRUE:
			case FALSE:
			case REGULAR_STRING_LITERAL:
			case VERBATIM_STRING_LITERAL:
			case IDENTIFIER:
			case OBJECT:
			case STRING:
			case DECIMAL:
			case CHAR:
			case INT:
			case LONG:
			case SBYTE:
			case BYTE:
			case SHORT:
			case UINT:
			case ULONG:
			case USHORT:
			case FLOAT:
			case DOUBLE:
			case DEC:
			case INC:
			case NEW:
			case LPAREN:
			case THIS:
			case BASE:
			case TYPEOF:
			case CHECKED:
			case UNCHECKED:
			case PLUS:
			case MINUS:
			case LNOT:
			case BNOT:
			case STAR:
			{
				return_string=expression();
				if (0==inputState.guessing)
				{
					Emit.EmitString (return_string);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_111_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void method_header() //throws RecognitionException, TokenStreamException
{
		
		Token  lp = null;
		Token  rp = null;
		
		string rt = "";
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case NEW:
				case VOID:
				case PUBLIC:
				case PROTECTED:
				case INTERNAL:
				case PRIVATE:
				case SEALED:
				case ABSTRACT:
				case STATIC:
				case VIRTUAL:
				case OVERRIDE:
				case EXTERN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_106_.member(LA(1))))
					{
						method_modifier();
					}
					else
					{
						goto _loop328_breakloop;
					}
					
				}
_loop328_breakloop:				;
			}    // ( ... )*
			rt=return_type();
			member_name();
			lp = LT(1);
			match(LPAREN);
			if (0==inputState.guessing)
			{
				Emit.EmitToken (lp);
			}
			{
				switch ( LA(1) )
				{
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case LBRACK:
				case REF:
				case OUT:
				case PARAMS:
				{
					formal_parameter_list();
					break;
				}
				case RPAREN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			rp = LT(1);
			match(RPAREN);
			if (0==inputState.guessing)
			{
				
				Emit.EmitToken (rp);
				Emit.EmitString (" : ");
				Emit.EmitString (rt);
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_112_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void method_body() //throws RecognitionException, TokenStreamException
{
		
		Token  s = null;
		
		StatementTree t = new StatementTree ();
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case LBRACE:
			{
				t=Block();
				if (0==inputState.guessing)
				{
					
					Emit.EmitString ( t.ToString () );
					
				}
				break;
			}
			case SEMI:
			{
				s = LT(1);
				match(SEMI);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (s);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void method_modifier() //throws RecognitionException, TokenStreamException
{
		
		Token  cm1 = null;
		Token  cm2 = null;
		Token  cm3 = null;
		Token  cm4 = null;
		Token  cm5 = null;
		Token  cm6 = null;
		Token  cm7 = null;
		Token  cm8 = null;
		Token  cm9 = null;
		Token  cm10 = null;
		Token  cm11 = null;
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NEW:
			{
				cm1 = LT(1);
				match(NEW);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm1);
				}
				break;
			}
			case PUBLIC:
			{
				cm2 = LT(1);
				match(PUBLIC);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm2);
				}
				break;
			}
			case PROTECTED:
			{
				cm3 = LT(1);
				match(PROTECTED);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm3);
				}
				break;
			}
			case INTERNAL:
			{
				cm4 = LT(1);
				match(INTERNAL);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm4);
				}
				break;
			}
			case PRIVATE:
			{
				cm5 = LT(1);
				match(PRIVATE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm5);
				}
				break;
			}
			case STATIC:
			{
				cm6 = LT(1);
				match(STATIC);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm6);
				}
				break;
			}
			case VIRTUAL:
			{
				cm7 = LT(1);
				match(VIRTUAL);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm7);
				}
				break;
			}
			case SEALED:
			{
				cm8 = LT(1);
				match(SEALED);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm8);
				}
				break;
			}
			case OVERRIDE:
			{
				cm9 = LT(1);
				match(OVERRIDE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm9);
				}
				break;
			}
			case ABSTRACT:
			{
				cm10 = LT(1);
				match(ABSTRACT);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm10);
				}
				break;
			}
			case EXTERN:
			{
				cm11 = LT(1);
				match(EXTERN);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm11);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_113_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  return_type() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  v = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case IDENTIFIER:
			case OBJECT:
			case STRING:
			case BOOL:
			case DECIMAL:
			case CHAR:
			case INT:
			case LONG:
			case SBYTE:
			case BYTE:
			case SHORT:
			case UINT:
			case ULONG:
			case USHORT:
			case FLOAT:
			case DOUBLE:
			{
				return_string=type();
				break;
			}
			case VOID:
			{
				v = LT(1);
				match(VOID);
				if (0==inputState.guessing)
				{
					return_string = v.getText();
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_114_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void member_name() //throws RecognitionException, TokenStreamException
{
		
		
		string mn = "";
		
		
		try {      // for error handling
			mn=type_name();
			if (0==inputState.guessing)
			{
				
				Emit.EmitString (mn);
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_115_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void formal_parameter_list() //throws RecognitionException, TokenStreamException
{
		
		Token  c1 = null;
		Token  c2 = null;
		
		try {      // for error handling
			bool synPredMatched338 = false;
			if (((tokenSet_116_.member(LA(1))) && ((LA(2) >= IDENTIFIER && LA(2) <= LBRACK))))
			{
				int _m338 = mark();
				synPredMatched338 = true;
				inputState.guessing++;
				try {
					{
						fixed_parameter();
						{    // ( ... )*
							for (;;)
							{
								if ((LA(1)==COMMA) && (tokenSet_116_.member(LA(2))))
								{
									match(COMMA);
									fixed_parameter();
								}
								else
								{
									goto _loop337_breakloop;
								}
								
							}
_loop337_breakloop:							;
						}    // ( ... )*
						match(COMMA);
						parameter_array();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched338 = false;
				}
				rewind(_m338);
				inputState.guessing--;
			}
			if ( synPredMatched338 )
			{
				fixed_parameter();
				{    // ( ... )*
					for (;;)
					{
						if ((LA(1)==COMMA) && (tokenSet_116_.member(LA(2))))
						{
							c1 = LT(1);
							match(COMMA);
							if (0==inputState.guessing)
							{
								Emit.EmitToken (c1);
							}
							fixed_parameter();
						}
						else
						{
							goto _loop340_breakloop;
						}
						
					}
_loop340_breakloop:					;
				}    // ( ... )*
				match(COMMA);
				parameter_array();
			}
			else {
				bool synPredMatched344 = false;
				if (((tokenSet_116_.member(LA(1))) && ((LA(2) >= IDENTIFIER && LA(2) <= LBRACK))))
				{
					int _m344 = mark();
					synPredMatched344 = true;
					inputState.guessing++;
					try {
						{
							fixed_parameter();
							{    // ( ... )*
								for (;;)
								{
									if ((LA(1)==COMMA))
									{
										match(COMMA);
										fixed_parameter();
									}
									else
									{
										goto _loop343_breakloop;
									}
									
								}
_loop343_breakloop:								;
							}    // ( ... )*
						}
					}
					catch (RecognitionException)
					{
						synPredMatched344 = false;
					}
					rewind(_m344);
					inputState.guessing--;
				}
				if ( synPredMatched344 )
				{
					fixed_parameter();
					{    // ( ... )*
						for (;;)
						{
							if ((LA(1)==COMMA))
							{
								c2 = LT(1);
								match(COMMA);
								if (0==inputState.guessing)
								{
									Emit.EmitToken (c2);
								}
								fixed_parameter();
							}
							else
							{
								goto _loop346_breakloop;
							}
							
						}
_loop346_breakloop:						;
					}    // ( ... )*
				}
				else if ((LA(1)==LBRACK||LA(1)==PARAMS) && (tokenSet_3_.member(LA(2)))) {
					parameter_array();
				}
				else
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
			}
			catch (RecognitionException ex)
			{
				if (0 == inputState.guessing)
				{
					reportError(ex);
					consume();
					consumeUntil(tokenSet_117_);
				}
				else
				{
					throw;
				}
			}
		}
		
	public void fixed_parameter() //throws RecognitionException, TokenStreamException
{
		
		Token  id = null;
		
		string p = "";
		string t = "";
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case REF:
				case OUT:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{
				switch ( LA(1) )
				{
				case REF:
				case OUT:
				{
					p=parameter_modifier();
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			t=type();
			id = LT(1);
			match(IDENTIFIER);
			if (0==inputState.guessing)
			{
				
				Emit.EmitString ( id.getText () + " : " + p + t );
				
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_118_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  parameter_array() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case PARAMS:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			match(PARAMS);
			array_type();
			match(IDENTIFIER);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_117_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  parameter_modifier() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  r = null;
		Token  o = null;
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case REF:
			{
				r = LT(1);
				match(REF);
				if (0==inputState.guessing)
				{
					return_string = r.getText();
				}
				break;
			}
			case OUT:
			{
				o = LT(1);
				match(OUT);
				if (0==inputState.guessing)
				{
					return_string = o.getText();
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_3_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void property_modifier() //throws RecognitionException, TokenStreamException
{
		
		Token  cm1 = null;
		Token  cm2 = null;
		Token  cm3 = null;
		Token  cm4 = null;
		Token  cm5 = null;
		Token  cm6 = null;
		Token  cm7 = null;
		Token  cm8 = null;
		Token  cm9 = null;
		Token  cm10 = null;
		Token  cm11 = null;
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NEW:
			{
				cm1 = LT(1);
				match(NEW);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm1);
				}
				break;
			}
			case PUBLIC:
			{
				cm2 = LT(1);
				match(PUBLIC);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm2);
				}
				break;
			}
			case PROTECTED:
			{
				cm3 = LT(1);
				match(PROTECTED);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm3);
				}
				break;
			}
			case INTERNAL:
			{
				cm4 = LT(1);
				match(INTERNAL);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm4);
				}
				break;
			}
			case PRIVATE:
			{
				cm5 = LT(1);
				match(PRIVATE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm5);
				}
				break;
			}
			case STATIC:
			{
				cm6 = LT(1);
				match(STATIC);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm6);
				}
				break;
			}
			case VIRTUAL:
			{
				cm7 = LT(1);
				match(VIRTUAL);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm7);
				}
				break;
			}
			case SEALED:
			{
				cm8 = LT(1);
				match(SEALED);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm8);
				}
				break;
			}
			case OVERRIDE:
			{
				cm9 = LT(1);
				match(OVERRIDE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm9);
				}
				break;
			}
			case ABSTRACT:
			{
				cm10 = LT(1);
				match(ABSTRACT);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm10);
				}
				break;
			}
			case EXTERN:
			{
				cm11 = LT(1);
				match(EXTERN);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm11);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_119_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void accessor_declarations() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched360 = false;
			if (((LA(1)==LBRACK||LA(1)==GET) && (LA(2)==IDENTIFIER||LA(2)==SEMI||LA(2)==LBRACE)))
			{
				int _m360 = mark();
				synPredMatched360 = true;
				inputState.guessing++;
				try {
					{
						get_accessor_declaration();
					}
				}
				catch (RecognitionException)
				{
					synPredMatched360 = false;
				}
				rewind(_m360);
				inputState.guessing--;
			}
			if ( synPredMatched360 )
			{
				get_accessor_declaration();
				{
					switch ( LA(1) )
					{
					case LBRACK:
					case SET:
					{
						set_accessor_declaration();
						break;
					}
					case RBRACE:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
			}
			else if ((LA(1)==LBRACK||LA(1)==SET) && (LA(2)==IDENTIFIER||LA(2)==SEMI||LA(2)==LBRACE)) {
				set_accessor_declaration();
				{
					switch ( LA(1) )
					{
					case LBRACK:
					case GET:
					{
						get_accessor_declaration();
						break;
					}
					case RBRACE:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_120_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void get_accessor_declaration() //throws RecognitionException, TokenStreamException
{
		
		Token  g = null;
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case GET:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			g = LT(1);
			match(GET);
			if (0==inputState.guessing)
			{
				Emit.EmitToken(g);
			}
			accessor_body();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_121_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void set_accessor_declaration() //throws RecognitionException, TokenStreamException
{
		
		Token  s = null;
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case SET:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			s = LT(1);
			match(SET);
			if (0==inputState.guessing)
			{
				Emit.EmitToken(s);
			}
			accessor_body();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_122_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void accessor_body() //throws RecognitionException, TokenStreamException
{
		
		Token  s = null;
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case LBRACE:
			{
				Block();
				break;
			}
			case SEMI:
			{
				s = LT(1);
				match(SEMI);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(s);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_123_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  event_modifier() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NEW:
			{
				match(NEW);
				break;
			}
			case PUBLIC:
			{
				match(PUBLIC);
				break;
			}
			case PROTECTED:
			{
				match(PROTECTED);
				break;
			}
			case INTERNAL:
			{
				match(INTERNAL);
				break;
			}
			case PRIVATE:
			{
				match(PRIVATE);
				break;
			}
			case STATIC:
			{
				match(STATIC);
				break;
			}
			case VIRTUAL:
			{
				match(VIRTUAL);
				break;
			}
			case SEALED:
			{
				match(SEALED);
				break;
			}
			case OVERRIDE:
			{
				match(OVERRIDE);
				break;
			}
			case ABSTARCT:
			{
				match(ABSTARCT);
				break;
			}
			case EXTERN:
			{
				match(EXTERN);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_124_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  event_accessor_declarations() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched386 = false;
			if (((LA(1)==IDENTIFIER||LA(1)==LBRACK) && (LA(2)==IDENTIFIER||LA(2)==LBRACE)))
			{
				int _m386 = mark();
				synPredMatched386 = true;
				inputState.guessing++;
				try {
					{
						{
							switch ( LA(1) )
							{
							case LBRACK:
							{
								attributes();
								break;
							}
							case ADD:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							 }
						}
						match(ADD);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched386 = false;
				}
				rewind(_m386);
				inputState.guessing--;
			}
			if ( synPredMatched386 )
			{
				add_accessor_declaration();
				remove_accessor_declaration();
			}
			else if ((LA(1)==IDENTIFIER||LA(1)==LBRACK) && (LA(2)==IDENTIFIER||LA(2)==LBRACE)) {
				remove_accessor_declaration();
				add_accessor_declaration();
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_120_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  add_accessor_declaration() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  idaad = null;
		
		return_string = "";
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case IDENTIFIER:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			idaad = LT(1);
			match(IDENTIFIER);
			if (!(idaad.getText()=="add"))
			  throw new SemanticException("idaad.getText()==\"add\"");
			Block();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_125_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  remove_accessor_declaration() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  idrad = null;
		
		return_string = "";
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case IDENTIFIER:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			idrad = LT(1);
			match(IDENTIFIER);
			if (!(idrad.getText()=="remove"))
			  throw new SemanticException("idrad.getText()==\"remove\"");
			Block();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_125_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void indexer_modifier() //throws RecognitionException, TokenStreamException
{
		
		Token  cm1 = null;
		Token  cm2 = null;
		Token  cm3 = null;
		Token  cm4 = null;
		Token  cm5 = null;
		Token  cm6 = null;
		Token  cm7 = null;
		Token  cm8 = null;
		Token  cm9 = null;
		Token  cm10 = null;
		Token  cm11 = null;
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NEW:
			{
				cm1 = LT(1);
				match(NEW);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm1);
				}
				break;
			}
			case PUBLIC:
			{
				cm2 = LT(1);
				match(PUBLIC);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm2);
				}
				break;
			}
			case PROTECTED:
			{
				cm3 = LT(1);
				match(PROTECTED);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm3);
				}
				break;
			}
			case INTERNAL:
			{
				cm4 = LT(1);
				match(INTERNAL);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm4);
				}
				break;
			}
			case PRIVATE:
			{
				cm5 = LT(1);
				match(PRIVATE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm5);
				}
				break;
			}
			case STATIC:
			{
				cm6 = LT(1);
				match(STATIC);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm6);
				}
				break;
			}
			case VIRTUAL:
			{
				cm7 = LT(1);
				match(VIRTUAL);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm7);
				}
				break;
			}
			case SEALED:
			{
				cm8 = LT(1);
				match(SEALED);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm8);
				}
				break;
			}
			case OVERRIDE:
			{
				cm9 = LT(1);
				match(OVERRIDE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm9);
				}
				break;
			}
			case ABSTRACT:
			{
				cm10 = LT(1);
				match(ABSTRACT);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm10);
				}
				break;
			}
			case EXTERN:
			{
				cm11 = LT(1);
				match(EXTERN);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (cm11);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_119_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  indexer_declarator() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched398 = false;
			if (((tokenSet_3_.member(LA(1))) && (LA(2)==DOT||LA(2)==LBRACK||LA(2)==THIS)))
			{
				int _m398 = mark();
				synPredMatched398 = true;
				inputState.guessing++;
				try {
					{
						type();
						match(THIS);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched398 = false;
				}
				rewind(_m398);
				inputState.guessing--;
			}
			if ( synPredMatched398 )
			{
				type();
				match(THIS);
				match(LBRACK);
				formal_parameter_list();
				match(RBRACK);
			}
			else if ((tokenSet_3_.member(LA(1))) && (LA(2)==IDENTIFIER||LA(2)==DOT||LA(2)==LBRACK)) {
				type();
				type_name();
				match(DOT);
				match(THIS);
				match(LBRACK);
				formal_parameter_list();
				match(RBRACK);
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_65_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  operator_modifier() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case PUBLIC:
			{
				match(PUBLIC);
				break;
			}
			case STATIC:
			{
				match(STATIC);
				break;
			}
			case EXTERN:
			{
				match(EXTERN);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_99_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  operator_declarator() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched406 = false;
			if (((tokenSet_3_.member(LA(1))) && (LA(2)==DOT||LA(2)==LBRACK||LA(2)==OPERATOR)))
			{
				int _m406 = mark();
				synPredMatched406 = true;
				inputState.guessing++;
				try {
					{
						type();
						match(OPERATOR);
						overloadable_unary_operator();
						match(LPAREN);
						type();
						match(IDENTIFIER);
						match(RPAREN);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched406 = false;
				}
				rewind(_m406);
				inputState.guessing--;
			}
			if ( synPredMatched406 )
			{
				unary_operator_declarator();
			}
			else if ((tokenSet_3_.member(LA(1))) && (LA(2)==DOT||LA(2)==LBRACK||LA(2)==OPERATOR)) {
				binary_operator_declarator();
			}
			else if ((LA(1)==IMPLICIT||LA(1)==EXPLICIT)) {
				conversion_operator_declarator();
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_112_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  operator_body() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case LBRACE:
			{
				Block();
				break;
			}
			case SEMI:
			{
				match(SEMI);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  overloadable_unary_operator() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case PLUS:
			{
				match(PLUS);
				break;
			}
			case MINUS:
			{
				match(MINUS);
				break;
			}
			case LNOT:
			{
				match(LNOT);
				break;
			}
			case BNOT:
			{
				match(BNOT);
				break;
			}
			case INC:
			{
				match(INC);
				break;
			}
			case DEC:
			{
				match(DEC);
				break;
			}
			case TRUE:
			{
				match(TRUE);
				break;
			}
			case FALSE:
			{
				match(FALSE);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_126_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  unary_operator_declarator() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			type();
			match(OPERATOR);
			overloadable_unary_operator();
			match(LPAREN);
			type();
			match(IDENTIFIER);
			match(RPAREN);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_112_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  binary_operator_declarator() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			type();
			match(OPERATOR);
			overloadable_binary_operator();
			match(LPAREN);
			type();
			match(IDENTIFIER);
			match(COMMA);
			type();
			match(IDENTIFIER);
			match(RPAREN);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_112_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  conversion_operator_declarator() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case IMPLICIT:
			{
				match(IMPLICIT);
				match(OPERATOR);
				type();
				match(LPAREN);
				type();
				match(IDENTIFIER);
				match(RPAREN);
				break;
			}
			case EXPLICIT:
			{
				match(EXPLICIT);
				match(OPERATOR);
				type();
				match(LPAREN);
				type();
				match(IDENTIFIER);
				match(RPAREN);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_112_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  overloadable_binary_operator() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case PLUS:
			{
				match(PLUS);
				break;
			}
			case MINUS:
			{
				match(MINUS);
				break;
			}
			case STAR:
			{
				match(STAR);
				break;
			}
			case DIV:
			{
				match(DIV);
				break;
			}
			case MOD:
			{
				match(MOD);
				break;
			}
			case BAND:
			{
				match(BAND);
				break;
			}
			case BOR:
			{
				match(BOR);
				break;
			}
			case BXOR:
			{
				match(BXOR);
				break;
			}
			case SL:
			{
				match(SL);
				break;
			}
			case SR:
			{
				match(SR);
				break;
			}
			case EQUAL:
			{
				match(EQUAL);
				break;
			}
			case NOT_EQUAL:
			{
				match(NOT_EQUAL);
				break;
			}
			case GTHAN:
			{
				match(GTHAN);
				break;
			}
			case LTHAN:
			{
				match(LTHAN);
				break;
			}
			case LE:
			{
				match(LE);
				break;
			}
			case GE:
			{
				match(GE);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_126_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void constructor_modifier() //throws RecognitionException, TokenStreamException
{
		
		Token  cm1 = null;
		Token  cm2 = null;
		Token  cm3 = null;
		Token  cm4 = null;
		Token  cm5 = null;
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case PUBLIC:
			{
				cm1 = LT(1);
				match(PUBLIC);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm1);
				}
				break;
			}
			case PROTECTED:
			{
				cm2 = LT(1);
				match(PROTECTED);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm2);
				}
				break;
			}
			case INTERNAL:
			{
				cm3 = LT(1);
				match(INTERNAL);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm3);
				}
				break;
			}
			case PRIVATE:
			{
				cm4 = LT(1);
				match(PRIVATE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm4);
				}
				break;
			}
			case EXTERN:
			{
				cm5 = LT(1);
				match(EXTERN);
				if (0==inputState.guessing)
				{
					Emit.EmitToken(cm5);
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_127_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  constructor_declarator() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  id = null;
		Token  lp = null;
		Token  rp = null;
		
		return_string ="";
		
		
		try {      // for error handling
			id = LT(1);
			match(IDENTIFIER);
			if (0==inputState.guessing)
			{
				
				Emit.EmitString ( ((ExtendedToken)id).GetWhitespaces () + "this");           
				
			}
			lp = LT(1);
			match(LPAREN);
			if (0==inputState.guessing)
			{
				Emit.EmitToken(lp);
			}
			{
				switch ( LA(1) )
				{
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case LBRACK:
				case REF:
				case OUT:
				case PARAMS:
				{
					formal_parameter_list();
					break;
				}
				case RPAREN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			rp = LT(1);
			match(RPAREN);
			if (0==inputState.guessing)
			{
				Emit.EmitToken(rp);
			}
			{
				switch ( LA(1) )
				{
				case COLON:
				{
					return_string=constructor_initializer();
					break;
				}
				case SEMI:
				case LBRACE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_112_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void constructor_body(
		string ctor_initializer
	) //throws RecognitionException, TokenStreamException
{
		
		Token  lb = null;
		Token  rb = null;
		Token  s = null;
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case LBRACE:
			{
				lb = LT(1);
				match(LBRACE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (lb);
				}
				if (0==inputState.guessing)
				{
					
					if(ctor_initializer != "")
					Emit.EmitString(ctor_initializer + ";");
					
				}
				{    // ( ... )*
					for (;;)
					{
						if ((tokenSet_49_.member(LA(1))))
						{
							statement();
						}
						else
						{
							goto _loop430_breakloop;
						}
						
					}
_loop430_breakloop:					;
				}    // ( ... )*
				rb = LT(1);
				match(RBRACE);
				if (0==inputState.guessing)
				{
					Emit.EmitToken (rb);
				}
				break;
			}
			case SEMI:
			{
				s = LT(1);
				match(SEMI);
				if (0==inputState.guessing)
				{
					
					if(ctor_initializer != "")
					Emit.EmitString( ((ExtendedToken)s).GetWhitespaces () + "{ " + ctor_initializer + " }");
					else
					Emit.EmitToken (s);
					
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public string  constructor_initializer() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		Token  c1 = null;
		Token  b = null;
		Token  lp1 = null;
		Token  rp1 = null;
		Token  c2 = null;
		Token  t = null;
		Token  lp2 = null;
		Token  rp2 = null;
		
		string al = "";
		return_string ="";
		
		
		try {      // for error handling
			bool synPredMatched425 = false;
			if (((LA(1)==COLON) && (LA(2)==BASE)))
			{
				int _m425 = mark();
				synPredMatched425 = true;
				inputState.guessing++;
				try {
					{
						match(COLON);
						match(BASE);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched425 = false;
				}
				rewind(_m425);
				inputState.guessing--;
			}
			if ( synPredMatched425 )
			{
				c1 = LT(1);
				match(COLON);
				b = LT(1);
				match(BASE);
				lp1 = LT(1);
				match(LPAREN);
				{
					switch ( LA(1) )
					{
					case INTEGER_LITERAL:
					case HEXADECIMAL_INTEGER_LITERAL:
					case REAL_LITERAL:
					case CHARACTER_LITERAL:
					case NULL:
					case TRUE:
					case FALSE:
					case REGULAR_STRING_LITERAL:
					case VERBATIM_STRING_LITERAL:
					case IDENTIFIER:
					case OBJECT:
					case STRING:
					case DECIMAL:
					case CHAR:
					case INT:
					case LONG:
					case SBYTE:
					case BYTE:
					case SHORT:
					case UINT:
					case ULONG:
					case USHORT:
					case FLOAT:
					case DOUBLE:
					case REF:
					case OUT:
					case DEC:
					case INC:
					case NEW:
					case LPAREN:
					case THIS:
					case BASE:
					case TYPEOF:
					case CHECKED:
					case UNCHECKED:
					case PLUS:
					case MINUS:
					case LNOT:
					case BNOT:
					case STAR:
					{
						al=argument_list();
						break;
					}
					case RPAREN:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				rp1 = LT(1);
				match(RPAREN);
				if (0==inputState.guessing)
				{
					
					return_string = ((ExtendedToken)c1).GetWhitespaces () + b.getText () + lp1.getText () + al + rp1.getText();
					
				}
			}
			else if ((LA(1)==COLON) && (LA(2)==THIS)) {
				c2 = LT(1);
				match(COLON);
				t = LT(1);
				match(THIS);
				lp2 = LT(1);
				match(LPAREN);
				{
					switch ( LA(1) )
					{
					case INTEGER_LITERAL:
					case HEXADECIMAL_INTEGER_LITERAL:
					case REAL_LITERAL:
					case CHARACTER_LITERAL:
					case NULL:
					case TRUE:
					case FALSE:
					case REGULAR_STRING_LITERAL:
					case VERBATIM_STRING_LITERAL:
					case IDENTIFIER:
					case OBJECT:
					case STRING:
					case DECIMAL:
					case CHAR:
					case INT:
					case LONG:
					case SBYTE:
					case BYTE:
					case SHORT:
					case UINT:
					case ULONG:
					case USHORT:
					case FLOAT:
					case DOUBLE:
					case REF:
					case OUT:
					case DEC:
					case INC:
					case NEW:
					case LPAREN:
					case THIS:
					case BASE:
					case TYPEOF:
					case CHECKED:
					case UNCHECKED:
					case PLUS:
					case MINUS:
					case LNOT:
					case BNOT:
					case STAR:
					{
						al=argument_list();
						break;
					}
					case RPAREN:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				rp2 = LT(1);
				match(RPAREN);
				if (0==inputState.guessing)
				{
					
					return_string = ((ExtendedToken)c2).GetWhitespaces () + t.getText () + lp2.getText () + al + rp2.getText();
					
				}
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_112_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  static_constructor_modifiers() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			bool synPredMatched435 = false;
			if (((LA(1)==STATIC) && (LA(2)==IDENTIFIER||LA(2)==EXTERN)))
			{
				int _m435 = mark();
				synPredMatched435 = true;
				inputState.guessing++;
				try {
					{
						match(STATIC);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched435 = false;
				}
				rewind(_m435);
				inputState.guessing--;
			}
			if ( synPredMatched435 )
			{
				match(STATIC);
				{
					switch ( LA(1) )
					{
					case EXTERN:
					{
						match(EXTERN);
						break;
					}
					case IDENTIFIER:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
			}
			else if ((LA(1)==STATIC||LA(1)==EXTERN) && (LA(2)==IDENTIFIER||LA(2)==STATIC)) {
				{
					switch ( LA(1) )
					{
					case EXTERN:
					{
						match(EXTERN);
						break;
					}
					case STATIC:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				match(STATIC);
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_114_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  static_constructor_body() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case LBRACE:
			{
				Block();
				break;
			}
			case SEMI:
			{
				match(SEMI);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public string  destructor_body() //throws RecognitionException, TokenStreamException
{
		string return_string;
		
		
		return_string = "";
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case LBRACE:
			{
				Block();
				break;
			}
			case SEMI:
			{
				match(SEMI);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_104_);
			}
			else
			{
				throw;
			}
		}
		return return_string;
	}
	
	public void struct_interfaces() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			match(COLON);
			type_name();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						match(COMMA);
						type_name();
					}
					else
					{
						goto _loop452_breakloop;
					}
					
				}
_loop452_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_65_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void struct_body() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			match(LBRACE);
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_128_.member(LA(1))))
					{
						struct_member_declaration();
					}
					else
					{
						goto _loop455_breakloop;
					}
					
				}
_loop455_breakloop:				;
			}    // ( ... )*
			match(RBRACE);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_86_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void struct_member_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched461 = false;
			if (((tokenSet_87_.member(LA(1))) && (tokenSet_88_.member(LA(2)))))
			{
				int _m461 = mark();
				synPredMatched461 = true;
				inputState.guessing++;
				try {
					{
						{
							switch ( LA(1) )
							{
							case LBRACK:
							{
								attributes();
								break;
							}
							case NEW:
							case CONST:
							case PUBLIC:
							case PROTECTED:
							case INTERNAL:
							case PRIVATE:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							 }
						}
						{    // ( ... )*
							for (;;)
							{
								if ((tokenSet_69_.member(LA(1))))
								{
									constant_modifier();
								}
								else
								{
									goto _loop460_breakloop;
								}
								
							}
_loop460_breakloop:							;
						}    // ( ... )*
						match(CONST);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched461 = false;
				}
				rewind(_m461);
				inputState.guessing--;
			}
			if ( synPredMatched461 )
			{
				constant_declaration();
			}
			else {
				bool synPredMatched466 = false;
				if (((tokenSet_89_.member(LA(1))) && (tokenSet_90_.member(LA(2)))))
				{
					int _m466 = mark();
					synPredMatched466 = true;
					inputState.guessing++;
					try {
						{
							{
								switch ( LA(1) )
								{
								case LBRACK:
								{
									attributes();
									break;
								}
								case IDENTIFIER:
								case OBJECT:
								case STRING:
								case BOOL:
								case DECIMAL:
								case CHAR:
								case INT:
								case LONG:
								case SBYTE:
								case BYTE:
								case SHORT:
								case UINT:
								case ULONG:
								case USHORT:
								case FLOAT:
								case DOUBLE:
								case NEW:
								case PUBLIC:
								case PROTECTED:
								case INTERNAL:
								case PRIVATE:
								case STATIC:
								case READONLY:
								case VOLATILE:
								{
									break;
								}
								default:
								{
									throw new NoViableAltException(LT(1), getFilename());
								}
								 }
							}
							{    // ( ... )*
								for (;;)
								{
									if ((tokenSet_105_.member(LA(1))))
									{
										field_modifier();
									}
									else
									{
										goto _loop465_breakloop;
									}
									
								}
_loop465_breakloop:								;
							}    // ( ... )*
							type();
							variable_declarator(false,"","");
						}
					}
					catch (RecognitionException)
					{
						synPredMatched466 = false;
					}
					rewind(_m466);
					inputState.guessing--;
				}
				if ( synPredMatched466 )
				{
					field_declaration();
				}
				else {
					bool synPredMatched471 = false;
					if (((tokenSet_91_.member(LA(1))) && (tokenSet_92_.member(LA(2)))))
					{
						int _m471 = mark();
						synPredMatched471 = true;
						inputState.guessing++;
						try {
							{
								{
									switch ( LA(1) )
									{
									case LBRACK:
									{
										attributes();
										break;
									}
									case IDENTIFIER:
									case OBJECT:
									case STRING:
									case BOOL:
									case DECIMAL:
									case CHAR:
									case INT:
									case LONG:
									case SBYTE:
									case BYTE:
									case SHORT:
									case UINT:
									case ULONG:
									case USHORT:
									case FLOAT:
									case DOUBLE:
									case NEW:
									case VOID:
									case PUBLIC:
									case PROTECTED:
									case INTERNAL:
									case PRIVATE:
									case SEALED:
									case ABSTRACT:
									case STATIC:
									case VIRTUAL:
									case OVERRIDE:
									case EXTERN:
									{
										break;
									}
									default:
									{
										throw new NoViableAltException(LT(1), getFilename());
									}
									 }
								}
								{    // ( ... )*
									for (;;)
									{
										if ((tokenSet_106_.member(LA(1))))
										{
											method_modifier();
										}
										else
										{
											goto _loop470_breakloop;
										}
										
									}
_loop470_breakloop:									;
								}    // ( ... )*
								return_type();
								member_name();
								match(LPAREN);
							}
						}
						catch (RecognitionException)
						{
							synPredMatched471 = false;
						}
						rewind(_m471);
						inputState.guessing--;
					}
					if ( synPredMatched471 )
					{
						method_declaration();
					}
					else {
						bool synPredMatched476 = false;
						if (((tokenSet_93_.member(LA(1))) && (tokenSet_94_.member(LA(2)))))
						{
							int _m476 = mark();
							synPredMatched476 = true;
							inputState.guessing++;
							try {
								{
									{
										switch ( LA(1) )
										{
										case LBRACK:
										{
											attributes();
											break;
										}
										case IDENTIFIER:
										case OBJECT:
										case STRING:
										case BOOL:
										case DECIMAL:
										case CHAR:
										case INT:
										case LONG:
										case SBYTE:
										case BYTE:
										case SHORT:
										case UINT:
										case ULONG:
										case USHORT:
										case FLOAT:
										case DOUBLE:
										case NEW:
										case PUBLIC:
										case PROTECTED:
										case INTERNAL:
										case PRIVATE:
										case SEALED:
										case ABSTRACT:
										case STATIC:
										case VIRTUAL:
										case OVERRIDE:
										case EXTERN:
										{
											break;
										}
										default:
										{
											throw new NoViableAltException(LT(1), getFilename());
										}
										 }
									}
									{    // ( ... )*
										for (;;)
										{
											if ((tokenSet_106_.member(LA(1))))
											{
												property_modifier();
											}
											else
											{
												goto _loop475_breakloop;
											}
											
										}
_loop475_breakloop:										;
									}    // ( ... )*
									type();
									member_name();
									match(LBRACE);
								}
							}
							catch (RecognitionException)
							{
								synPredMatched476 = false;
							}
							rewind(_m476);
							inputState.guessing--;
						}
						if ( synPredMatched476 )
						{
							property_declaration();
						}
						else {
							bool synPredMatched481 = false;
							if (((tokenSet_95_.member(LA(1))) && (tokenSet_96_.member(LA(2)))))
							{
								int _m481 = mark();
								synPredMatched481 = true;
								inputState.guessing++;
								try {
									{
										{
											switch ( LA(1) )
											{
											case LBRACK:
											{
												attributes();
												break;
											}
											case NEW:
											case PUBLIC:
											case PROTECTED:
											case INTERNAL:
											case PRIVATE:
											case SEALED:
											case STATIC:
											case VIRTUAL:
											case OVERRIDE:
											case EXTERN:
											case EVENT:
											case ABSTARCT:
											{
												break;
											}
											default:
											{
												throw new NoViableAltException(LT(1), getFilename());
											}
											 }
										}
										{    // ( ... )*
											for (;;)
											{
												if ((tokenSet_107_.member(LA(1))))
												{
													event_modifier();
												}
												else
												{
													goto _loop480_breakloop;
												}
												
											}
_loop480_breakloop:											;
										}    // ( ... )*
										match(EVENT);
									}
								}
								catch (RecognitionException)
								{
									synPredMatched481 = false;
								}
								rewind(_m481);
								inputState.guessing--;
							}
							if ( synPredMatched481 )
							{
								event_declaration();
							}
							else {
								bool synPredMatched486 = false;
								if (((tokenSet_93_.member(LA(1))) && (tokenSet_97_.member(LA(2)))))
								{
									int _m486 = mark();
									synPredMatched486 = true;
									inputState.guessing++;
									try {
										{
											{
												switch ( LA(1) )
												{
												case LBRACK:
												{
													attributes();
													break;
												}
												case IDENTIFIER:
												case OBJECT:
												case STRING:
												case BOOL:
												case DECIMAL:
												case CHAR:
												case INT:
												case LONG:
												case SBYTE:
												case BYTE:
												case SHORT:
												case UINT:
												case ULONG:
												case USHORT:
												case FLOAT:
												case DOUBLE:
												case NEW:
												case PUBLIC:
												case PROTECTED:
												case INTERNAL:
												case PRIVATE:
												case SEALED:
												case ABSTRACT:
												case STATIC:
												case VIRTUAL:
												case OVERRIDE:
												case EXTERN:
												{
													break;
												}
												default:
												{
													throw new NoViableAltException(LT(1), getFilename());
												}
												 }
											}
											{    // ( ... )*
												for (;;)
												{
													if ((tokenSet_106_.member(LA(1))))
													{
														indexer_modifier();
													}
													else
													{
														goto _loop485_breakloop;
													}
													
												}
_loop485_breakloop:												;
											}    // ( ... )*
											indexer_declarator();
											match(RBRACE);
										}
									}
									catch (RecognitionException)
									{
										synPredMatched486 = false;
									}
									rewind(_m486);
									inputState.guessing--;
								}
								if ( synPredMatched486 )
								{
									indexer_declaration();
								}
								else {
									bool synPredMatched491 = false;
									if (((tokenSet_98_.member(LA(1))) && (tokenSet_99_.member(LA(2)))))
									{
										int _m491 = mark();
										synPredMatched491 = true;
										inputState.guessing++;
										try {
											{
												{
													switch ( LA(1) )
													{
													case LBRACK:
													{
														attributes();
														break;
													}
													case PUBLIC:
													case STATIC:
													case EXTERN:
													{
														break;
													}
													default:
													{
														throw new NoViableAltException(LT(1), getFilename());
													}
													 }
												}
												{ // ( ... )+
												int _cnt490=0;
												for (;;)
												{
													if ((LA(1)==PUBLIC||LA(1)==STATIC||LA(1)==EXTERN))
													{
														operator_modifier();
													}
													else
													{
														if (_cnt490 >= 1) { goto _loop490_breakloop; } else { throw new NoViableAltException(LT(1), getFilename());; }
													}
													
													_cnt490++;
												}
_loop490_breakloop:												;
												}    // ( ... )+
												operator_declarator();
												operator_body();
											}
										}
										catch (RecognitionException)
										{
											synPredMatched491 = false;
										}
										rewind(_m491);
										inputState.guessing--;
									}
									if ( synPredMatched491 )
									{
										operator_declaration();
									}
									else {
										bool synPredMatched493 = false;
										if (((tokenSet_100_.member(LA(1))) && (tokenSet_101_.member(LA(2)))))
										{
											int _m493 = mark();
											synPredMatched493 = true;
											inputState.guessing++;
											try {
												{
													constructor_declaration();
												}
											}
											catch (RecognitionException)
											{
												synPredMatched493 = false;
											}
											rewind(_m493);
											inputState.guessing--;
										}
										if ( synPredMatched493 )
										{
											constructor_declaration();
										}
										else {
											bool synPredMatched496 = false;
											if (((LA(1)==LBRACK||LA(1)==STATIC||LA(1)==EXTERN) && (LA(2)==IDENTIFIER||LA(2)==STATIC||LA(2)==EXTERN)))
											{
												int _m496 = mark();
												synPredMatched496 = true;
												inputState.guessing++;
												try {
													{
														{
															switch ( LA(1) )
															{
															case LBRACK:
															{
																attributes();
																break;
															}
															case STATIC:
															case EXTERN:
															{
																break;
															}
															default:
															{
																throw new NoViableAltException(LT(1), getFilename());
															}
															 }
														}
														static_constructor_modifiers();
													}
												}
												catch (RecognitionException)
												{
													synPredMatched496 = false;
												}
												rewind(_m496);
												inputState.guessing--;
											}
											if ( synPredMatched496 )
											{
												static_constructor_declaration();
											}
											else if ((tokenSet_102_.member(LA(1))) && (tokenSet_103_.member(LA(2)))) {
												type_declaration();
											}
											else
											{
												throw new NoViableAltException(LT(1), getFilename());
											}
											}}}}}}}}
										}
										catch (RecognitionException ex)
										{
											if (0 == inputState.guessing)
											{
												reportError(ex);
												consume();
												consumeUntil(tokenSet_129_);
											}
											else
											{
												throw;
											}
										}
									}
									
	public void variable_initializer_list() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			variable_initializer();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA) && (tokenSet_25_.member(LA(2))))
					{
						match(COMMA);
						variable_initializer();
					}
					else
					{
						goto _loop503_breakloop;
					}
					
				}
_loop503_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_130_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void interface_base() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			match(COLON);
			type_name();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						match(COMMA);
						type_name();
					}
					else
					{
						goto _loop513_breakloop;
					}
					
				}
_loop513_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_65_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void interface_body() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			match(LBRACE);
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_131_.member(LA(1))))
					{
						interface_member_declaration();
					}
					else
					{
						goto _loop516_breakloop;
					}
					
				}
_loop516_breakloop:				;
			}    // ( ... )*
			match(RBRACE);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_86_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void interface_member_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched521 = false;
			if (((tokenSet_132_.member(LA(1))) && (tokenSet_133_.member(LA(2)))))
			{
				int _m521 = mark();
				synPredMatched521 = true;
				inputState.guessing++;
				try {
					{
						{
							switch ( LA(1) )
							{
							case LBRACK:
							{
								attributes();
								break;
							}
							case IDENTIFIER:
							case OBJECT:
							case STRING:
							case BOOL:
							case DECIMAL:
							case CHAR:
							case INT:
							case LONG:
							case SBYTE:
							case BYTE:
							case SHORT:
							case UINT:
							case ULONG:
							case USHORT:
							case FLOAT:
							case DOUBLE:
							case NEW:
							case VOID:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							 }
						}
						{
							switch ( LA(1) )
							{
							case NEW:
							{
								match(NEW);
								break;
							}
							case IDENTIFIER:
							case OBJECT:
							case STRING:
							case BOOL:
							case DECIMAL:
							case CHAR:
							case INT:
							case LONG:
							case SBYTE:
							case BYTE:
							case SHORT:
							case UINT:
							case ULONG:
							case USHORT:
							case FLOAT:
							case DOUBLE:
							case VOID:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							 }
						}
						return_type();
						match(IDENTIFIER);
						match(LPAREN);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched521 = false;
				}
				rewind(_m521);
				inputState.guessing--;
			}
			if ( synPredMatched521 )
			{
				interface_method_declaration();
			}
			else {
				bool synPredMatched525 = false;
				if (((tokenSet_134_.member(LA(1))) && ((LA(2) >= IDENTIFIER && LA(2) <= LBRACK))))
				{
					int _m525 = mark();
					synPredMatched525 = true;
					inputState.guessing++;
					try {
						{
							{
								switch ( LA(1) )
								{
								case LBRACK:
								{
									attributes();
									break;
								}
								case IDENTIFIER:
								case OBJECT:
								case STRING:
								case BOOL:
								case DECIMAL:
								case CHAR:
								case INT:
								case LONG:
								case SBYTE:
								case BYTE:
								case SHORT:
								case UINT:
								case ULONG:
								case USHORT:
								case FLOAT:
								case DOUBLE:
								case NEW:
								{
									break;
								}
								default:
								{
									throw new NoViableAltException(LT(1), getFilename());
								}
								 }
							}
							{
								switch ( LA(1) )
								{
								case NEW:
								{
									match(NEW);
									break;
								}
								case IDENTIFIER:
								case OBJECT:
								case STRING:
								case BOOL:
								case DECIMAL:
								case CHAR:
								case INT:
								case LONG:
								case SBYTE:
								case BYTE:
								case SHORT:
								case UINT:
								case ULONG:
								case USHORT:
								case FLOAT:
								case DOUBLE:
								{
									break;
								}
								default:
								{
									throw new NoViableAltException(LT(1), getFilename());
								}
								 }
							}
							type();
							match(IDENTIFIER);
							match(LBRACE);
						}
					}
					catch (RecognitionException)
					{
						synPredMatched525 = false;
					}
					rewind(_m525);
					inputState.guessing--;
				}
				if ( synPredMatched525 )
				{
					interface_property_declaration();
				}
				else {
					bool synPredMatched529 = false;
					if (((LA(1)==LBRACK||LA(1)==NEW||LA(1)==EVENT) && (tokenSet_135_.member(LA(2)))))
					{
						int _m529 = mark();
						synPredMatched529 = true;
						inputState.guessing++;
						try {
							{
								{
									switch ( LA(1) )
									{
									case LBRACK:
									{
										attributes();
										break;
									}
									case NEW:
									case EVENT:
									{
										break;
									}
									default:
									{
										throw new NoViableAltException(LT(1), getFilename());
									}
									 }
								}
								{
									switch ( LA(1) )
									{
									case NEW:
									{
										match(NEW);
										break;
									}
									case EVENT:
									{
										break;
									}
									default:
									{
										throw new NoViableAltException(LT(1), getFilename());
									}
									 }
								}
								match(EVENT);
							}
						}
						catch (RecognitionException)
						{
							synPredMatched529 = false;
						}
						rewind(_m529);
						inputState.guessing--;
					}
					if ( synPredMatched529 )
					{
						interface_event_declaration();
					}
					else if ((tokenSet_134_.member(LA(1))) && (tokenSet_136_.member(LA(2)))) {
						interface_indexer_declaration();
					}
					else
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					}}
				}
				catch (RecognitionException ex)
				{
					if (0 == inputState.guessing)
					{
						reportError(ex);
						consume();
						consumeUntil(tokenSet_137_);
					}
					else
					{
						throw;
					}
				}
			}
			
	public void interface_method_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case NEW:
				case VOID:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{
				switch ( LA(1) )
				{
				case NEW:
				{
					match(NEW);
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case VOID:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			return_type();
			match(IDENTIFIER);
			match(LPAREN);
			{
				switch ( LA(1) )
				{
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case LBRACK:
				case REF:
				case OUT:
				case PARAMS:
				{
					formal_parameter_list();
					break;
				}
				case RPAREN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			match(RPAREN);
			match(SEMI);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_137_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void interface_property_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case NEW:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{
				switch ( LA(1) )
				{
				case NEW:
				{
					match(NEW);
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			type();
			match(IDENTIFIER);
			match(LBRACE);
			interface_accessors();
			match(RBRACE);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_137_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void interface_event_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case NEW:
				case EVENT:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{
				switch ( LA(1) )
				{
				case NEW:
				{
					match(NEW);
					break;
				}
				case EVENT:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			match(EVENT);
			type();
			match(IDENTIFIER);
			match(SEMI);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_137_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void interface_indexer_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			{
				switch ( LA(1) )
				{
				case LBRACK:
				{
					attributes();
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				case NEW:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			{
				switch ( LA(1) )
				{
				case NEW:
				{
					match(NEW);
					break;
				}
				case IDENTIFIER:
				case OBJECT:
				case STRING:
				case BOOL:
				case DECIMAL:
				case CHAR:
				case INT:
				case LONG:
				case SBYTE:
				case BYTE:
				case SHORT:
				case UINT:
				case ULONG:
				case USHORT:
				case FLOAT:
				case DOUBLE:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			type();
			match(THIS);
			match(LBRACK);
			formal_parameter_list();
			match(RBRACK);
			match(LBRACE);
			interface_accessors();
			match(RBRACE);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_137_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void interface_accessors() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched541 = false;
			if (((LA(1)==LBRACK||LA(1)==GET) && (LA(2)==IDENTIFIER||LA(2)==SEMI)))
			{
				int _m541 = mark();
				synPredMatched541 = true;
				inputState.guessing++;
				try {
					{
						{
							switch ( LA(1) )
							{
							case LBRACK:
							{
								attributes();
								break;
							}
							case GET:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							 }
						}
						match(GET);
						match(SEMI);
						{
							switch ( LA(1) )
							{
							case LBRACK:
							{
								attributes();
								break;
							}
							case SET:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							 }
						}
						match(SET);
						match(SEMI);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched541 = false;
				}
				rewind(_m541);
				inputState.guessing--;
			}
			if ( synPredMatched541 )
			{
				{
					switch ( LA(1) )
					{
					case LBRACK:
					{
						attributes();
						break;
					}
					case GET:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				match(GET);
				match(SEMI);
				{
					switch ( LA(1) )
					{
					case LBRACK:
					{
						attributes();
						break;
					}
					case SET:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				match(SET);
				match(SEMI);
			}
			else {
				bool synPredMatched547 = false;
				if (((LA(1)==LBRACK||LA(1)==SET) && (LA(2)==IDENTIFIER||LA(2)==SEMI)))
				{
					int _m547 = mark();
					synPredMatched547 = true;
					inputState.guessing++;
					try {
						{
							{
								switch ( LA(1) )
								{
								case LBRACK:
								{
									attributes();
									break;
								}
								case SET:
								{
									break;
								}
								default:
								{
									throw new NoViableAltException(LT(1), getFilename());
								}
								 }
							}
							match(SET);
							match(SEMI);
							{
								switch ( LA(1) )
								{
								case LBRACK:
								{
									attributes();
									break;
								}
								case GET:
								{
									break;
								}
								default:
								{
									throw new NoViableAltException(LT(1), getFilename());
								}
								 }
							}
							match(GET);
							match(SEMI);
						}
					}
					catch (RecognitionException)
					{
						synPredMatched547 = false;
					}
					rewind(_m547);
					inputState.guessing--;
				}
				if ( synPredMatched547 )
				{
					{
						switch ( LA(1) )
						{
						case LBRACK:
						{
							attributes();
							break;
						}
						case SET:
						{
							break;
						}
						default:
						{
							throw new NoViableAltException(LT(1), getFilename());
						}
						 }
					}
					match(SET);
					match(SEMI);
					{
						switch ( LA(1) )
						{
						case LBRACK:
						{
							attributes();
							break;
						}
						case GET:
						{
							break;
						}
						default:
						{
							throw new NoViableAltException(LT(1), getFilename());
						}
						 }
					}
					match(GET);
					match(SEMI);
				}
				else {
					bool synPredMatched552 = false;
					if (((LA(1)==LBRACK||LA(1)==SET) && (LA(2)==IDENTIFIER||LA(2)==SEMI)))
					{
						int _m552 = mark();
						synPredMatched552 = true;
						inputState.guessing++;
						try {
							{
								{
									switch ( LA(1) )
									{
									case LBRACK:
									{
										attributes();
										break;
									}
									case SET:
									{
										break;
									}
									default:
									{
										throw new NoViableAltException(LT(1), getFilename());
									}
									 }
								}
								match(SET);
							}
						}
						catch (RecognitionException)
						{
							synPredMatched552 = false;
						}
						rewind(_m552);
						inputState.guessing--;
					}
					if ( synPredMatched552 )
					{
						{
							switch ( LA(1) )
							{
							case LBRACK:
							{
								attributes();
								break;
							}
							case SET:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							 }
						}
						match(SET);
						match(SEMI);
					}
					else if ((LA(1)==LBRACK||LA(1)==GET) && (LA(2)==IDENTIFIER||LA(2)==SEMI)) {
						{
							switch ( LA(1) )
							{
							case LBRACK:
							{
								attributes();
								break;
							}
							case GET:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							 }
						}
						match(GET);
						match(SEMI);
					}
					else
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					}}
				}
				catch (RecognitionException ex)
				{
					if (0 == inputState.guessing)
					{
						reportError(ex);
						consume();
						consumeUntil(tokenSet_120_);
					}
					else
					{
						throw;
					}
				}
			}
			
	public void enum_base() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			match(COLON);
			integral_type();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_65_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void enum_body() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched570 = false;
			if (((LA(1)==LBRACE) && (LA(2)==IDENTIFIER||LA(2)==LBRACK)))
			{
				int _m570 = mark();
				synPredMatched570 = true;
				inputState.guessing++;
				try {
					{
						match(LBRACE);
						enum_member_declarations();
						match(COMMA);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched570 = false;
				}
				rewind(_m570);
				inputState.guessing--;
			}
			if ( synPredMatched570 )
			{
				match(LBRACE);
				enum_member_declarations();
				match(COMMA);
				match(RBRACE);
			}
			else if ((LA(1)==LBRACE) && (LA(2)==IDENTIFIER||LA(2)==LBRACK||LA(2)==RBRACE)) {
				match(LBRACE);
				{
					switch ( LA(1) )
					{
					case IDENTIFIER:
					case LBRACK:
					{
						enum_member_declarations();
						break;
					}
					case RBRACE:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				match(RBRACE);
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_86_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void enum_member_declarations() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			enum_member_declaration();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA) && (LA(2)==IDENTIFIER||LA(2)==LBRACK))
					{
						match(COMMA);
						enum_member_declaration();
					}
					else
					{
						goto _loop575_breakloop;
					}
					
				}
_loop575_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_130_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void enum_member_declaration() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched579 = false;
			if (((LA(1)==IDENTIFIER||LA(1)==LBRACK) && (LA(2)==IDENTIFIER||LA(2)==ASSIGN)))
			{
				int _m579 = mark();
				synPredMatched579 = true;
				inputState.guessing++;
				try {
					{
						{
							switch ( LA(1) )
							{
							case LBRACK:
							{
								attributes();
								break;
							}
							case IDENTIFIER:
							{
								break;
							}
							default:
							{
								throw new NoViableAltException(LT(1), getFilename());
							}
							 }
						}
						match(IDENTIFIER);
						match(ASSIGN);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched579 = false;
				}
				rewind(_m579);
				inputState.guessing--;
			}
			if ( synPredMatched579 )
			{
				{
					switch ( LA(1) )
					{
					case LBRACK:
					{
						attributes();
						break;
					}
					case IDENTIFIER:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				match(IDENTIFIER);
				match(ASSIGN);
				constant_expression();
			}
			else if ((LA(1)==IDENTIFIER||LA(1)==LBRACK) && (LA(2)==IDENTIFIER||LA(2)==COMMA||LA(2)==RBRACE)) {
				{
					switch ( LA(1) )
					{
					case LBRACK:
					{
						attributes();
						break;
					}
					case IDENTIFIER:
					{
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					 }
				}
				match(IDENTIFIER);
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_130_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void delegate_modifier() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			switch ( LA(1) )
			{
			case NEW:
			{
				match(NEW);
				break;
			}
			case PUBLIC:
			{
				match(PUBLIC);
				break;
			}
			case PROTECTED:
			{
				match(PROTECTED);
				break;
			}
			case INTERNAL:
			{
				match(INTERNAL);
				break;
			}
			case PRIVATE:
			{
				match(PRIVATE);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			 }
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_138_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void global_attribute_section() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched593 = false;
			if (((LA(1)==LBRACK) && (LA(2)==IDENTIFIER)))
			{
				int _m593 = mark();
				synPredMatched593 = true;
				inputState.guessing++;
				try {
					{
						match(LBRACK);
						global_attribute_target_specifier();
						attribute_list();
						match(COMMA);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched593 = false;
				}
				rewind(_m593);
				inputState.guessing--;
			}
			if ( synPredMatched593 )
			{
				match(LBRACK);
				global_attribute_target_specifier();
				attribute_list();
				match(COMMA);
				match(RBRACK);
			}
			else if ((LA(1)==LBRACK) && (LA(2)==IDENTIFIER)) {
				match(LBRACK);
				global_attribute_target_specifier();
				attribute_list();
				match(RBRACK);
			}
			else
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_63_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void global_attribute_target_specifier() //throws RecognitionException, TokenStreamException
{
		
		Token  idgat = null;
		
		try {      // for error handling
			idgat = LT(1);
			match(IDENTIFIER);
			if (!(idgat.getText()=="assembly" || idgat.getText()=="module"))
			  throw new SemanticException("idgat.getText()==\"assembly\" || idgat.getText()==\"module\"");
			match(COLON);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_114_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void attribute_list() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			attribute();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA) && (LA(2)==IDENTIFIER))
					{
						match(COMMA);
						attribute();
					}
					else
					{
						goto _loop605_breakloop;
					}
					
				}
_loop605_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_139_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void attribute_section() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			match(LBRACK);
			{
				if ((LA(1)==IDENTIFIER) && (LA(2)==COLON))
				{
					attribute_target_specifier();
				}
				else if ((LA(1)==IDENTIFIER) && (tokenSet_140_.member(LA(2)))) {
				}
				else
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				
			}
			attribute_list();
			{
				switch ( LA(1) )
				{
				case COMMA:
				{
					match(COMMA);
					break;
				}
				case RBRACK:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
			match(RBRACK);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_141_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void attribute_target_specifier() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			attribute_target();
			match(COLON);
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_114_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void attribute_target() //throws RecognitionException, TokenStreamException
{
		
		Token  idat = null;
		
		try {      // for error handling
			idat = LT(1);
			match(IDENTIFIER);
			if (!(idat.getText()=="field"    || //FIELD
        idat.getText()=="event"    || //EVENT
        idat.getText()=="method"   || //METHOD
        idat.getText()=="param"    || //PARAM
        idat.getText()=="property" || //PROPERTY
        idat.getText()=="return"   || //RETURN
        idat.getText()=="type" ))
			  throw new SemanticException("idat.getText()==\"field\"    || //FIELD\n        idat.getText()==\"event\"    || //EVENT\n        idat.getText()==\"method\"   || //METHOD\n        idat.getText()==\"param\"    || //PARAM\n        idat.getText()==\"property\" || //PROPERTY\n        idat.getText()==\"return\"   || //RETURN\n        idat.getText()==\"type\" ");
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_142_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void attribute() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			attribute_name();
			{
				switch ( LA(1) )
				{
				case LPAREN:
				{
					attribute_arguments();
					break;
				}
				case COMMA:
				case RBRACK:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				 }
			}
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_139_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void attribute_name() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			type_name();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_143_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void attribute_arguments() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			bool synPredMatched611 = false;
			if (((LA(1)==LPAREN) && (tokenSet_12_.member(LA(2)))))
			{
				int _m611 = mark();
				synPredMatched611 = true;
				inputState.guessing++;
				try {
					{
						match(LPAREN);
						positional_argument_list();
						match(COMMA);
						named_argument_list();
						match(RPAREN);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched611 = false;
				}
				rewind(_m611);
				inputState.guessing--;
			}
			if ( synPredMatched611 )
			{
				match(LPAREN);
				positional_argument_list();
				match(COMMA);
				named_argument_list();
				match(RPAREN);
			}
			else {
				bool synPredMatched613 = false;
				if (((LA(1)==LPAREN) && (LA(2)==IDENTIFIER)))
				{
					int _m613 = mark();
					synPredMatched613 = true;
					inputState.guessing++;
					try {
						{
							match(LPAREN);
							named_argument_list();
							match(RPAREN);
						}
					}
					catch (RecognitionException)
					{
						synPredMatched613 = false;
					}
					rewind(_m613);
					inputState.guessing--;
				}
				if ( synPredMatched613 )
				{
					match(LPAREN);
					named_argument_list();
					match(RPAREN);
				}
				else if ((LA(1)==LPAREN) && (tokenSet_144_.member(LA(2)))) {
					match(LPAREN);
					{
						switch ( LA(1) )
						{
						case INTEGER_LITERAL:
						case HEXADECIMAL_INTEGER_LITERAL:
						case REAL_LITERAL:
						case CHARACTER_LITERAL:
						case NULL:
						case TRUE:
						case FALSE:
						case REGULAR_STRING_LITERAL:
						case VERBATIM_STRING_LITERAL:
						case IDENTIFIER:
						case OBJECT:
						case STRING:
						case DECIMAL:
						case CHAR:
						case INT:
						case LONG:
						case SBYTE:
						case BYTE:
						case SHORT:
						case UINT:
						case ULONG:
						case USHORT:
						case FLOAT:
						case DOUBLE:
						case DEC:
						case INC:
						case NEW:
						case LPAREN:
						case THIS:
						case BASE:
						case TYPEOF:
						case CHECKED:
						case UNCHECKED:
						case PLUS:
						case MINUS:
						case LNOT:
						case BNOT:
						case STAR:
						{
							positional_argument_list();
							break;
						}
						case RPAREN:
						{
							break;
						}
						default:
						{
							throw new NoViableAltException(LT(1), getFilename());
						}
						 }
					}
					match(RPAREN);
				}
				else
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
			}
			catch (RecognitionException ex)
			{
				if (0 == inputState.guessing)
				{
					reportError(ex);
					consume();
					consumeUntil(tokenSet_139_);
				}
				else
				{
					throw;
				}
			}
		}
		
	public void positional_argument_list() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			attribute_argument_expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA) && (tokenSet_12_.member(LA(2))))
					{
						match(COMMA);
						attribute_argument_expression();
					}
					else
					{
						goto _loop617_breakloop;
					}
					
				}
_loop617_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_11_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void named_argument_list() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			match(IDENTIFIER);
			match(ASSIGN);
			attribute_argument_expression();
			{    // ( ... )*
				for (;;)
				{
					if ((LA(1)==COMMA))
					{
						match(COMMA);
						match(IDENTIFIER);
						match(ASSIGN);
						attribute_argument_expression();
					}
					else
					{
						goto _loop620_breakloop;
					}
					
				}
_loop620_breakloop:				;
			}    // ( ... )*
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_16_);
			}
			else
			{
				throw;
			}
		}
	}
	
	public void attribute_argument_expression() //throws RecognitionException, TokenStreamException
{
		
		
		try {      // for error handling
			expression();
		}
		catch (RecognitionException ex)
		{
			if (0 == inputState.guessing)
			{
				reportError(ex);
				consume();
				consumeUntil(tokenSet_11_);
			}
			else
			{
				throw;
			}
		}
	}
	
	private void initializeFactory()
	{
	}
	
	public static readonly string[] tokenNames_ = new string[] {
		@"""<0>""",
		@"""EOF""",
		@"""<2>""",
		@"""NULL_TREE_LOOKAHEAD""",
		@"""INTEGER_LITERAL""",
		@"""HEXADECIMAL_INTEGER_LITERAL""",
		@"""REAL_LITERAL""",
		@"""CHARACTER_LITERAL""",
		@"""null""",
		@"""true""",
		@"""false""",
		@"""REGULAR_STRING_LITERAL""",
		@"""VERBATIM_STRING_LITERAL""",
		@"""IDENTIFIER""",
		@"""DOT""",
		@"""object""",
		@"""string""",
		@"""bool""",
		@"""decimal""",
		@"""char""",
		@"""int""",
		@"""long""",
		@"""sbyte""",
		@"""byte""",
		@"""short""",
		@"""uint""",
		@"""ulong""",
		@"""ushort""",
		@"""float""",
		@"""double""",
		@"""LBRACK""",
		@"""COMMA""",
		@"""RBRACK""",
		@"""ref""",
		@"""out""",
		@"""DEC""",
		@"""INC""",
		@"""new""",
		@"""LPAREN""",
		@"""RPAREN""",
		@"""this""",
		@"""base""",
		@"""typeof""",
		@"""void""",
		@"""checked""",
		@"""unchecked""",
		@"""PLUS""",
		@"""MINUS""",
		@"""LNOT""",
		@"""BNOT""",
		@"""STAR""",
		@"""DIV""",
		@"""MOD""",
		@"""SL""",
		@"""SR""",
		@"""is""",
		@"""as""",
		@"""LTHAN""",
		@"""GTHAN""",
		@"""LE""",
		@"""GE""",
		@"""EQUAL""",
		@"""NOT_EQUAL""",
		@"""BAND""",
		@"""BXOR""",
		@"""BOR""",
		@"""LAND""",
		@"""LOR""",
		@"""QUESTION""",
		@"""COLON""",
		@"""ASSIGN""",
		@"""PLUS_ASN""",
		@"""MINUS_ASN""",
		@"""STAR_ASN""",
		@"""DIV_ASN""",
		@"""MOD_ASN""",
		@"""BAND_ASN""",
		@"""BOR_ASN""",
		@"""BXOR_ASN""",
		@"""SL_ASN""",
		@"""SR_ASN""",
		@"""const""",
		@"""SEMI""",
		@"""LBRACE""",
		@"""RBRACE""",
		@"""if""",
		@"""else""",
		@"""switch""",
		@"""case""",
		@"""default""",
		@"""while""",
		@"""do""",
		@"""for""",
		@"""foreach""",
		@"""in""",
		@"""break""",
		@"""continue""",
		@"""goto""",
		@"""return""",
		@"""throw""",
		@"""try""",
		@"""catch""",
		@"""finally""",
		@"""lock""",
		@"""using""",
		@"""namespace""",
		@"""enum""",
		@"""struct""",
		@"""interface""",
		@"""class""",
		@"""public""",
		@"""protected""",
		@"""internal""",
		@"""private""",
		@"""sealed""",
		@"""abstract""",
		@"""static""",
		@"""readonly""",
		@"""VOLATILE""",
		@"""virtual""",
		@"""override""",
		@"""extern""",
		@"""params""",
		@"""get""",
		@"""set""",
		@"""event""",
		@"""ABSTARCT""",
		@"""ADD""",
		@"""operator""",
		@"""implicit""",
		@"""explicit""",
		@"""delegate""",
		@"""sizeof""",
		@"""stackalloc""",
		@"""fixed""",
		@"""unsafe""",
		@"""NEW_LINE""",
		@"""WHITESPACE""",
		@"""NEW_LINE_CHARACTER""",
		@"""NOT_NEW_LINE""",
		@"""SINGLE_LINE_COMMENT""",
		@"""DELIMITED_COMMENT""",
		@"""UNICODE_ESCAPE_SEQUENCE""",
		@"""IDENTIFIER_START_CHARACTER""",
		@"""IDENTIFIER_PART_CHARACTER""",
		@"""DECIMAL_DIGIT""",
		@"""HEX_DIGIT""",
		@"""INTEGER_TYPE_SUFFIX""",
		@"""NUMERIC_LITERAL""",
		@"""EXPONENT_PART""",
		@"""SIGN""",
		@"""CHARACTER""",
		@"""SIMPLE_CHARACTER""",
		@"""SIMPLE_ESCAPE_SEQUENCE""",
		@"""HEXADECIMAL_ESCAPE_SEQUENCE""",
		@"""REGULAR_STRING_LITERAL_CHARACTER""",
		@"""SINGLE_REGULAR_STRING_LITERAL_CHARCACTER""",
		@"""HASH""",
		@"""QUOTE""",
		@"""PP_DIRECTIVE""",
		@"""PP_WHITESPACE""",
		@"""PP_NEW_LINE""",
		@"""PP_EXPRESSION""",
		@"""PP_OR_EXPRESSION""",
		@"""PP_AND_EXPRESSION""",
		@"""PP_EQUALITY_EXPRESSION""",
		@"""EQUALITY_OP""",
		@"""PP_UNARY_EXPRESSION""",
		@"""PP_PRIMARY_EXPRESSION""",
		@"""PP_CONDITIONAL""",
		@"""PP_IF_SECTION""",
		@"""PP_ELIF_SECTION""",
		@"""PP_ELSE_SECTION""",
		@"""PP_ENDIF""",
		@"""PP_DECLARATION""",
		@"""CONDITIONAL_SYMBOL""",
		@"""PP_LINE""",
		@"""LINE_INDICATOR""",
		@"""FILE_NAME""",
		@"""FILE_NAME_CHARACTER""",
		@"""PP_DIAGNOSTIC""",
		@"""PP_START_REGION""",
		@"""PP_END_REGION""",
		@"""PP_MESSAGE"""
	};
	
	private static long[] mk_tokenSet_0_()
	{
		long[] data = { -913858445164544L, 1441791L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_0_ = new BitSet(mk_tokenSet_0_());
	private static long[] mk_tokenSet_1_()
	{
		long[] data = { 0L, 262144L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_1_ = new BitSet(mk_tokenSet_1_());
	private static long[] mk_tokenSet_2_()
	{
		long[] data = { -2197754686495236096L, 1835071L, 1L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_2_ = new BitSet(mk_tokenSet_2_());
	private static long[] mk_tokenSet_3_()
	{
		long[] data = { 1073717248L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_3_ = new BitSet(mk_tokenSet_3_());
	private static long[] mk_tokenSet_4_()
	{
		long[] data = { -2197754687568994304L, 1310783L, 1L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_4_ = new BitSet(mk_tokenSet_4_());
	private static long[] mk_tokenSet_5_()
	{
		long[] data = { -2197754687568977920L, 1310783L, 1L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_5_ = new BitSet(mk_tokenSet_5_());
	private static long[] mk_tokenSet_6_()
	{
		long[] data = { -2197754687568994304L, 1835071L, 1L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_6_ = new BitSet(mk_tokenSet_6_());
	private static long[] mk_tokenSet_7_()
	{
		long[] data = { -2197754686495252480L, 1310783L, 1L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_7_ = new BitSet(mk_tokenSet_7_());
	private static long[] mk_tokenSet_8_()
	{
		long[] data = { -2197754686495252480L, 1835071L, 1L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_8_ = new BitSet(mk_tokenSet_8_());
	private static long[] mk_tokenSet_9_()
	{
		long[] data = { 1073741824L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_9_ = new BitSet(mk_tokenSet_9_());
	private static long[] mk_tokenSet_10_()
	{
		long[] data = { -912758933528576L, 1966079L, 1L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_10_ = new BitSet(mk_tokenSet_10_());
	private static long[] mk_tokenSet_11_()
	{
		long[] data = { 551903297536L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_11_ = new BitSet(mk_tokenSet_11_());
	private static long[] mk_tokenSet_12_()
	{
		long[] data = { 2242420678705136L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_12_ = new BitSet(mk_tokenSet_12_());
	private static long[] mk_tokenSet_13_()
	{
		long[] data = { -8821862826000L, 1310783L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_13_ = new BitSet(mk_tokenSet_13_());
	private static long[] mk_tokenSet_14_()
	{
		long[] data = { 2242421752594416L, 131008L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_14_ = new BitSet(mk_tokenSet_14_());
	private static long[] mk_tokenSet_15_()
	{
		long[] data = { 556198264832L, 1310752L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_15_ = new BitSet(mk_tokenSet_15_());
	private static long[] mk_tokenSet_16_()
	{
		long[] data = { 549755813888L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_16_ = new BitSet(mk_tokenSet_16_());
	private static long[] mk_tokenSet_17_()
	{
		long[] data = { 377957138432L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_17_ = new BitSet(mk_tokenSet_17_());
	private static long[] mk_tokenSet_18_()
	{
		long[] data = { 60886529982448L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_18_ = new BitSet(mk_tokenSet_18_());
	private static long[] mk_tokenSet_19_()
	{
		long[] data = { -8821862826000L, 1441791L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_19_ = new BitSet(mk_tokenSet_19_());
	private static long[] mk_tokenSet_20_()
	{
		long[] data = { 379030880256L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_20_ = new BitSet(mk_tokenSet_20_());
	private static long[] mk_tokenSet_21_()
	{
		long[] data = { -914237476044800L, 1441791L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_21_ = new BitSet(mk_tokenSet_21_());
	private static long[] mk_tokenSet_22_()
	{
		long[] data = { -913859518906368L, 1441791L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_22_ = new BitSet(mk_tokenSet_22_());
	private static long[] mk_tokenSet_23_()
	{
		long[] data = { 16384L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_23_ = new BitSet(mk_tokenSet_23_());
	private static long[] mk_tokenSet_24_()
	{
		long[] data = { 4294967296L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_24_ = new BitSet(mk_tokenSet_24_());
	private static long[] mk_tokenSet_25_()
	{
		long[] data = { 2242420678705136L, 524288L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_25_ = new BitSet(mk_tokenSet_25_());
	private static long[] mk_tokenSet_26_()
	{
		long[] data = { 2242420678705136L, 1572864L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_26_ = new BitSet(mk_tokenSet_26_());
	private static long[] mk_tokenSet_27_()
	{
		long[] data = { -8795536823943168L, 1310783L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_27_ = new BitSet(mk_tokenSet_27_());
	private static long[] mk_tokenSet_28_()
	{
		long[] data = { -9006643056476160L, 1310783L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_28_ = new BitSet(mk_tokenSet_28_());
	private static long[] mk_tokenSet_29_()
	{
		long[] data = { -36028240820699136L, 1310783L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_29_ = new BitSet(mk_tokenSet_29_());
	private static long[] mk_tokenSet_30_()
	{
		long[] data = { 2197747240095711216L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_30_ = new BitSet(mk_tokenSet_30_());
	private static long[] mk_tokenSet_31_()
	{
		long[] data = { 72048215976837104L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_31_ = new BitSet(mk_tokenSet_31_());
	private static long[] mk_tokenSet_32_()
	{
		long[] data = { 108077012995801072L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_32_ = new BitSet(mk_tokenSet_32_());
	private static long[] mk_tokenSet_33_()
	{
		long[] data = { -2269823034057555984L, 1310783L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_33_ = new BitSet(mk_tokenSet_33_());
	private static long[] mk_tokenSet_34_()
	{
		long[] data = { -2305842453015429120L, 1310783L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_34_ = new BitSet(mk_tokenSet_34_());
	private static long[] mk_tokenSet_35_()
	{
		long[] data = { -9223371480656510976L, 1310783L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_35_ = new BitSet(mk_tokenSet_35_());
	private static long[] mk_tokenSet_36_()
	{
		long[] data = { 556198264832L, 1310783L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_36_ = new BitSet(mk_tokenSet_36_());
	private static long[] mk_tokenSet_37_()
	{
		long[] data = { 556198264832L, 1310782L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_37_ = new BitSet(mk_tokenSet_37_());
	private static long[] mk_tokenSet_38_()
	{
		long[] data = { 556198264832L, 1310780L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_38_ = new BitSet(mk_tokenSet_38_());
	private static long[] mk_tokenSet_39_()
	{
		long[] data = { 556198264832L, 1310776L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_39_ = new BitSet(mk_tokenSet_39_());
	private static long[] mk_tokenSet_40_()
	{
		long[] data = { 556198264832L, 1310768L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_40_ = new BitSet(mk_tokenSet_40_());
	private static long[] mk_tokenSet_41_()
	{
		long[] data = { 2147483648L, 1310752L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_41_ = new BitSet(mk_tokenSet_41_());
	private static long[] mk_tokenSet_42_()
	{
		long[] data = { 549755813888L, 262144L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_42_ = new BitSet(mk_tokenSet_42_());
	private static long[] mk_tokenSet_43_()
	{
		long[] data = { 1073717248L, 131072L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_43_ = new BitSet(mk_tokenSet_43_());
	private static long[] mk_tokenSet_44_()
	{
		long[] data = { 2242420678705136L, 1785576816640L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_44_ = new BitSet(mk_tokenSet_44_());
	private static long[] mk_tokenSet_45_()
	{
		long[] data = { 2242421752594416L, 1785628458944L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_45_ = new BitSet(mk_tokenSet_45_());
	private static long[] mk_tokenSet_46_()
	{
		long[] data = { 2242420678836208L, 1785628327936L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_46_ = new BitSet(mk_tokenSet_46_());
	private static long[] mk_tokenSet_47_()
	{
		long[] data = { 2242421752594416L, 393152L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_47_ = new BitSet(mk_tokenSet_47_());
	private static long[] mk_tokenSet_48_()
	{
		long[] data = { 2242420678836208L, 1785632522240L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_48_ = new BitSet(mk_tokenSet_48_());
	private static long[] mk_tokenSet_49_()
	{
		long[] data = { 2242420678836208L, 1785576947712L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_49_ = new BitSet(mk_tokenSet_49_());
	private static long[] mk_tokenSet_50_()
	{
		long[] data = { 2251217845600242L, 8935139460605935616L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_50_ = new BitSet(mk_tokenSet_50_());
	private static long[] mk_tokenSet_51_()
	{
		long[] data = { 551903297536L, 262144L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_51_ = new BitSet(mk_tokenSet_51_());
	private static long[] mk_tokenSet_52_()
	{
		long[] data = { 2147483648L, 262144L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_52_ = new BitSet(mk_tokenSet_52_());
	private static long[] mk_tokenSet_53_()
	{
		long[] data = { 2242973655891952L, 262144L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_53_ = new BitSet(mk_tokenSet_53_());
	private static long[] mk_tokenSet_54_()
	{
		long[] data = { 0L, 51380224L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_54_ = new BitSet(mk_tokenSet_54_());
	private static long[] mk_tokenSet_55_()
	{
		long[] data = { 2242420678836208L, 1785627279360L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_55_ = new BitSet(mk_tokenSet_55_());
	private static long[] mk_tokenSet_56_()
	{
		long[] data = { 2242423900078064L, 393152L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_56_ = new BitSet(mk_tokenSet_56_());
	private static long[] mk_tokenSet_57_()
	{
		long[] data = { 2242420678836208L, 2060510429184L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_57_ = new BitSet(mk_tokenSet_57_());
	private static long[] mk_tokenSet_58_()
	{
		long[] data = { 2242420678836208L, 2197949382656L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_58_ = new BitSet(mk_tokenSet_58_());
	private static long[] mk_tokenSet_59_()
	{
		long[] data = { 2L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_59_ = new BitSet(mk_tokenSet_59_());
	private static long[] mk_tokenSet_60_()
	{
		long[] data = { -8828305276944L, 131039L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_60_ = new BitSet(mk_tokenSet_60_());
	private static long[] mk_tokenSet_61_()
	{
		long[] data = { 138512695296L, 4501400604114944L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_61_ = new BitSet(mk_tokenSet_61_());
	private static long[] mk_tokenSet_62_()
	{
		long[] data = { 138512695298L, 4502500116791296L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_62_ = new BitSet(mk_tokenSet_62_());
	private static long[] mk_tokenSet_63_()
	{
		long[] data = { 138512695298L, 4501400604114944L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_63_ = new BitSet(mk_tokenSet_63_());
	private static long[] mk_tokenSet_64_()
	{
		long[] data = { 138512695298L, 4501400605163520L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_64_ = new BitSet(mk_tokenSet_64_());
	private static long[] mk_tokenSet_65_()
	{
		long[] data = { 0L, 524288L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_65_ = new BitSet(mk_tokenSet_65_());
	private static long[] mk_tokenSet_66_()
	{
		long[] data = { 138512695298L, 4501400605425664L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_66_ = new BitSet(mk_tokenSet_66_());
	private static long[] mk_tokenSet_67_()
	{
		long[] data = { 138512695296L, 1059929209176064L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_67_ = new BitSet(mk_tokenSet_67_());
	private static long[] mk_tokenSet_68_()
	{
		long[] data = { 137438961664L, 1059929209176064L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_68_ = new BitSet(mk_tokenSet_68_());
	private static long[] mk_tokenSet_69_()
	{
		long[] data = { 137438953472L, 1055531162664960L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_69_ = new BitSet(mk_tokenSet_69_());
	private static long[] mk_tokenSet_70_()
	{
		long[] data = { 138512695296L, 1064327255687168L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_70_ = new BitSet(mk_tokenSet_70_());
	private static long[] mk_tokenSet_71_()
	{
		long[] data = { 137438961664L, 1064327255687168L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_71_ = new BitSet(mk_tokenSet_71_());
	private static long[] mk_tokenSet_72_()
	{
		long[] data = { 138512695296L, 1073123348709376L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_72_ = new BitSet(mk_tokenSet_72_());
	private static long[] mk_tokenSet_73_()
	{
		long[] data = { 137438961664L, 1073123348709376L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_73_ = new BitSet(mk_tokenSet_73_());
	private static long[] mk_tokenSet_74_()
	{
		long[] data = { 138512695296L, 4468415255281664L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_74_ = new BitSet(mk_tokenSet_74_());
	private static long[] mk_tokenSet_75_()
	{
		long[] data = { 137438961664L, 4468415255281664L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_75_ = new BitSet(mk_tokenSet_75_());
	private static long[] mk_tokenSet_76_()
	{
		long[] data = { 137438953472L, 4433230883192832L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_76_ = new BitSet(mk_tokenSet_76_());
	private static long[] mk_tokenSet_77_()
	{
		long[] data = { 138512695296L, 1055531162664960L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_77_ = new BitSet(mk_tokenSet_77_());
	private static long[] mk_tokenSet_78_()
	{
		long[] data = { 8934605692928L, 1055531162664960L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_78_ = new BitSet(mk_tokenSet_78_());
	private static long[] mk_tokenSet_79_()
	{
		long[] data = { 571885632856066L, 7205757204770717696L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_79_ = new BitSet(mk_tokenSet_79_());
	private static long[] mk_tokenSet_80_()
	{
		long[] data = { 571910328918016L, 9223367638808395776L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_80_ = new BitSet(mk_tokenSet_80_());
	private static long[] mk_tokenSet_81_()
	{
		long[] data = { 137438953472L, 1059929209176064L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_81_ = new BitSet(mk_tokenSet_81_());
	private static long[] mk_tokenSet_82_()
	{
		long[] data = { 137438953472L, 1064327255687168L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_82_ = new BitSet(mk_tokenSet_82_());
	private static long[] mk_tokenSet_83_()
	{
		long[] data = { 137438953472L, 1073123348709376L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_83_ = new BitSet(mk_tokenSet_83_());
	private static long[] mk_tokenSet_84_()
	{
		long[] data = { 137438953472L, 4468415255281664L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_84_ = new BitSet(mk_tokenSet_84_());
	private static long[] mk_tokenSet_85_()
	{
		long[] data = { 571885632856064L, 7205755005746413568L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_85_ = new BitSet(mk_tokenSet_85_());
	private static long[] mk_tokenSet_86_()
	{
		long[] data = { 571885632856066L, 7205757204770979840L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_86_ = new BitSet(mk_tokenSet_86_());
	private static long[] mk_tokenSet_87_()
	{
		long[] data = { 138512695296L, 1055531162796032L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_87_ = new BitSet(mk_tokenSet_87_());
	private static long[] mk_tokenSet_88_()
	{
		long[] data = { 138512670720L, 1055531162796032L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_88_ = new BitSet(mk_tokenSet_88_());
	private static long[] mk_tokenSet_89_()
	{
		long[] data = { 139586412544L, 32580728554258432L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_89_ = new BitSet(mk_tokenSet_89_());
	private static long[] mk_tokenSet_90_()
	{
		long[] data = { 139586428928L, 32580728554258432L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_90_ = new BitSet(mk_tokenSet_90_());
	private static long[] mk_tokenSet_91_()
	{
		long[] data = { 8935679434752L, 261138409643311104L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_91_ = new BitSet(mk_tokenSet_91_());
	private static long[] mk_tokenSet_92_()
	{
		long[] data = { 8935679451136L, 261138409643311104L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_92_ = new BitSet(mk_tokenSet_92_());
	private static long[] mk_tokenSet_93_()
	{
		long[] data = { 139586412544L, 261138409643311104L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_93_ = new BitSet(mk_tokenSet_93_());
	private static long[] mk_tokenSet_94_()
	{
		long[] data = { 139586428928L, 261138409643311104L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_94_ = new BitSet(mk_tokenSet_94_());
	private static long[] mk_tokenSet_95_()
	{
		long[] data = { 138512695296L, 7176415637470707712L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_95_ = new BitSet(mk_tokenSet_95_());
	private static long[] mk_tokenSet_96_()
	{
		long[] data = { 138512670720L, 7176415637470707712L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_96_ = new BitSet(mk_tokenSet_96_());
	private static long[] mk_tokenSet_97_()
	{
		long[] data = { 1239098056704L, 261138409643311104L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_97_ = new BitSet(mk_tokenSet_97_());
	private static long[] mk_tokenSet_98_()
	{
		long[] data = { 1073741824L, 148689156447404032L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_98_ = new BitSet(mk_tokenSet_98_());
	private static long[] mk_tokenSet_99_()
	{
		long[] data = { 1073717248L, 148689156447404032L, 6L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_99_ = new BitSet(mk_tokenSet_99_());
	private static long[] mk_tokenSet_100_()
	{
		long[] data = { 1073750016L, 145170719238520832L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_100_ = new BitSet(mk_tokenSet_100_());
	private static long[] mk_tokenSet_101_()
	{
		long[] data = { 274877915136L, 145170719238520832L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_101_ = new BitSet(mk_tokenSet_101_());
	private static long[] mk_tokenSet_102_()
	{
		long[] data = { 138512695296L, 4499201580859392L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_102_ = new BitSet(mk_tokenSet_102_());
	private static long[] mk_tokenSet_103_()
	{
		long[] data = { 8934605692928L, 4499201580859392L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_103_ = new BitSet(mk_tokenSet_103_());
	private static long[] mk_tokenSet_104_()
	{
		long[] data = { 571885632856064L, 7205755005747462144L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_104_ = new BitSet(mk_tokenSet_104_());
	private static long[] mk_tokenSet_105_()
	{
		long[] data = { 137438953472L, 32580728554258432L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_105_ = new BitSet(mk_tokenSet_105_());
	private static long[] mk_tokenSet_106_()
	{
		long[] data = { 137438953472L, 261138409643311104L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_106_ = new BitSet(mk_tokenSet_106_());
	private static long[] mk_tokenSet_107_()
	{
		long[] data = { 137438953472L, 4870572628257013760L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_107_ = new BitSet(mk_tokenSet_107_());
	private static long[] mk_tokenSet_108_()
	{
		long[] data = { 0L, 145170719238520832L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_108_ = new BitSet(mk_tokenSet_108_());
	private static long[] mk_tokenSet_109_()
	{
		long[] data = { 137438953472L, 1055531162796032L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_109_ = new BitSet(mk_tokenSet_109_());
	private static long[] mk_tokenSet_110_()
	{
		long[] data = { 138512670720L, 32580728554258432L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_110_ = new BitSet(mk_tokenSet_110_());
	private static long[] mk_tokenSet_111_()
	{
		long[] data = { 2147483648L, 1310720L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_111_ = new BitSet(mk_tokenSet_111_());
	private static long[] mk_tokenSet_112_()
	{
		long[] data = { 0L, 786432L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_112_ = new BitSet(mk_tokenSet_112_());
	private static long[] mk_tokenSet_113_()
	{
		long[] data = { 8934605692928L, 261138409643311104L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_113_ = new BitSet(mk_tokenSet_113_());
	private static long[] mk_tokenSet_114_()
	{
		long[] data = { 8192L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_114_ = new BitSet(mk_tokenSet_114_());
	private static long[] mk_tokenSet_115_()
	{
		long[] data = { 274877906944L, 1572864L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_115_ = new BitSet(mk_tokenSet_115_());
	private static long[] mk_tokenSet_116_()
	{
		long[] data = { 27917262848L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_116_ = new BitSet(mk_tokenSet_116_());
	private static long[] mk_tokenSet_117_()
	{
		long[] data = { 554050781184L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_117_ = new BitSet(mk_tokenSet_117_());
	private static long[] mk_tokenSet_118_()
	{
		long[] data = { 556198264832L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_118_ = new BitSet(mk_tokenSet_118_());
	private static long[] mk_tokenSet_119_()
	{
		long[] data = { 138512670720L, 261138409643311104L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_119_ = new BitSet(mk_tokenSet_119_());
	private static long[] mk_tokenSet_120_()
	{
		long[] data = { 0L, 1048576L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_120_ = new BitSet(mk_tokenSet_120_());
	private static long[] mk_tokenSet_121_()
	{
		long[] data = { 1073741824L, 1152921504607895552L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_121_ = new BitSet(mk_tokenSet_121_());
	private static long[] mk_tokenSet_122_()
	{
		long[] data = { 1073741824L, 576460752304472064L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_122_ = new BitSet(mk_tokenSet_122_());
	private static long[] mk_tokenSet_123_()
	{
		long[] data = { 1073741824L, 1729382256911319040L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_123_ = new BitSet(mk_tokenSet_123_());
	private static long[] mk_tokenSet_124_()
	{
		long[] data = { 137438953472L, 7176415637470707712L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_124_ = new BitSet(mk_tokenSet_124_());
	private static long[] mk_tokenSet_125_()
	{
		long[] data = { 1073750016L, 1048576L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_125_ = new BitSet(mk_tokenSet_125_());
	private static long[] mk_tokenSet_126_()
	{
		long[] data = { 274877906944L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_126_ = new BitSet(mk_tokenSet_126_());
	private static long[] mk_tokenSet_127_()
	{
		long[] data = { 8192L, 145170719238520832L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_127_ = new BitSet(mk_tokenSet_127_());
	private static long[] mk_tokenSet_128_()
	{
		long[] data = { 8935679434752L, 7205755005746413568L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_128_ = new BitSet(mk_tokenSet_128_());
	private static long[] mk_tokenSet_129_()
	{
		long[] data = { 8935679434752L, 7205755005747462144L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_129_ = new BitSet(mk_tokenSet_129_());
	private static long[] mk_tokenSet_130_()
	{
		long[] data = { 2147483648L, 1048576L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_130_ = new BitSet(mk_tokenSet_130_());
	private static long[] mk_tokenSet_131_()
	{
		long[] data = { 8935679434752L, 2305843009213693952L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_131_ = new BitSet(mk_tokenSet_131_());
	private static long[] mk_tokenSet_132_()
	{
		long[] data = { 8935679434752L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_132_ = new BitSet(mk_tokenSet_132_());
	private static long[] mk_tokenSet_133_()
	{
		long[] data = { 8798240497664L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_133_ = new BitSet(mk_tokenSet_133_());
	private static long[] mk_tokenSet_134_()
	{
		long[] data = { 139586412544L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_134_ = new BitSet(mk_tokenSet_134_());
	private static long[] mk_tokenSet_135_()
	{
		long[] data = { 1073717248L, 2305843009213693952L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_135_ = new BitSet(mk_tokenSet_135_());
	private static long[] mk_tokenSet_136_()
	{
		long[] data = { 1101659103232L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_136_ = new BitSet(mk_tokenSet_136_());
	private static long[] mk_tokenSet_137_()
	{
		long[] data = { 8935679434752L, 2305843009214742528L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_137_ = new BitSet(mk_tokenSet_137_());
	private static long[] mk_tokenSet_138_()
	{
		long[] data = { 137438953472L, 1055531162664960L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_138_ = new BitSet(mk_tokenSet_138_());
	private static long[] mk_tokenSet_139_()
	{
		long[] data = { 6442450944L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_139_ = new BitSet(mk_tokenSet_139_());
	private static long[] mk_tokenSet_140_()
	{
		long[] data = { 281320374272L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_140_ = new BitSet(mk_tokenSet_140_());
	private static long[] mk_tokenSet_141_()
	{
		long[] data = { 571911402659840L, 9223367638808395776L, 8L, 0L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_141_ = new BitSet(mk_tokenSet_141_());
	private static long[] mk_tokenSet_142_()
	{
		long[] data = { 0L, 32L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_142_ = new BitSet(mk_tokenSet_142_());
	private static long[] mk_tokenSet_143_()
	{
		long[] data = { 281320357888L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_143_ = new BitSet(mk_tokenSet_143_());
	private static long[] mk_tokenSet_144_()
	{
		long[] data = { 2242970434519024L, 0L, 0L};
		return data;
	}
	public static readonly BitSet tokenSet_144_ = new BitSet(mk_tokenSet_144_());
	
}
}
