// $ANTLR 2.7.4: "csharpgrammar.g" -> "CSharpLexer.cs"$

    using System.Collections;
    using Nemerle.Collections;

namespace Nemerle.CSharp
{
	// Generate header specific to lexer CSharp file
	using System;
	using Stream                          = System.IO.Stream;
	using TextReader                      = System.IO.TextReader;
	using Hashtable                       = System.Collections.Hashtable;
	using Comparer                        = System.Collections.Comparer;
	
	using TokenStreamException            = antlr.TokenStreamException;
	using TokenStreamIOException          = antlr.TokenStreamIOException;
	using TokenStreamRecognitionException = antlr.TokenStreamRecognitionException;
	using CharStreamException             = antlr.CharStreamException;
	using CharStreamIOException           = antlr.CharStreamIOException;
	using ANTLRException                  = antlr.ANTLRException;
	using CharScanner                     = antlr.CharScanner;
	using InputBuffer                     = antlr.InputBuffer;
	using ByteBuffer                      = antlr.ByteBuffer;
	using CharBuffer                      = antlr.CharBuffer;
	using Token                           = antlr.Token;
	using CommonToken                     = antlr.CommonToken;
	using SemanticException               = antlr.SemanticException;
	using RecognitionException            = antlr.RecognitionException;
	using NoViableAltForCharException     = antlr.NoViableAltForCharException;
	using MismatchedCharException         = antlr.MismatchedCharException;
	using TokenStream                     = antlr.TokenStream;
	using LexerSharedInputState           = antlr.LexerSharedInputState;
	using BitSet                          = antlr.collections.impl.BitSet;
	
	public 	class CSharpLexer : antlr.CharScanner	, TokenStream
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
		
		public CSharpLexer(Stream ins) : this(new ByteBuffer(ins))
		{
		}
		
		public CSharpLexer(TextReader r) : this(new CharBuffer(r))
		{
		}
		
		public CSharpLexer(InputBuffer ib)		 : this(new LexerSharedInputState(ib))
		{
		}
		
		public CSharpLexer(LexerSharedInputState state) : base(state)
		{
			initialize();
		}
		private void initialize()
		{
			caseSensitiveLiterals = true;
			setCaseSensitive(true);
			literals = new Hashtable(100, (float) 0.4, null, Comparer.Default);
			literals.Add("byte", 23);
			literals.Add("extern", 121);
			literals.Add("public", 110);
			literals.Add("uint", 25);
			literals.Add("namespace", 105);
			literals.Add("case", 88);
			literals.Add("short", 24);
			literals.Add("while", 90);
			literals.Add("break", 95);
			literals.Add("new", 37);
			literals.Add("sealed", 114);
			literals.Add("object", 15);
			literals.Add("sbyte", 22);
			literals.Add("readonly", 117);
			literals.Add("checked", 44);
			literals.Add("stackalloc", 133);
			literals.Add("decimal", 18);
			literals.Add("typeof", 42);
			literals.Add("lock", 103);
			literals.Add("const", 81);
			literals.Add("unchecked", 45);
			literals.Add("float", 28);
			literals.Add("return", 98);
			literals.Add("foreach", 93);
			literals.Add("throw", 99);
			literals.Add("using", 104);
			literals.Add("operator", 128);
			literals.Add("fixed", 134);
			literals.Add("null", 8);
			literals.Add("sizeof", 132);
			literals.Add("unsafe", 135);
			literals.Add("protected", 111);
			literals.Add("ref", 33);
			literals.Add("class", 109);
			literals.Add("base", 41);
			literals.Add("do", 91);
			literals.Add("event", 125);
			literals.Add("out", 34);
			literals.Add("set", 124);
			literals.Add("ushort", 27);
			literals.Add("interface", 108);
			literals.Add("is", 55);
			literals.Add("internal", 112);
			literals.Add("explicit", 130);
			literals.Add("ulong", 26);
			literals.Add("if", 85);
			literals.Add("double", 29);
			literals.Add("override", 120);
			literals.Add("as", 56);
			literals.Add("delegate", 131);
			literals.Add("implicit", 129);
			literals.Add("catch", 101);
			literals.Add("try", 100);
			literals.Add("params", 122);
			literals.Add("goto", 97);
			literals.Add("enum", 106);
			literals.Add("int", 20);
			literals.Add("for", 92);
			literals.Add("char", 19);
			literals.Add("private", 113);
			literals.Add("string", 16);
			literals.Add("default", 89);
			literals.Add("false", 10);
			literals.Add("this", 40);
			literals.Add("static", 116);
			literals.Add("abstract", 115);
			literals.Add("get", 123);
			literals.Add("continue", 96);
			literals.Add("bool", 17);
			literals.Add("struct", 107);
			literals.Add("finally", 102);
			literals.Add("else", 86);
			literals.Add("in", 94);
			literals.Add("void", 43);
			literals.Add("switch", 87);
			literals.Add("true", 9);
			literals.Add("long", 21);
			literals.Add("virtual", 119);
		}
		
		override public Token nextToken()			//throws TokenStreamException
		{
			Token theRetToken = null;
tryAgain:
			for (;;)
			{
				Token _token = null;
				int _ttype = Token.INVALID_TYPE;
				resetText();
				try     // for char stream error handling
				{
					try     // for lexical error handling
					{
						switch ( LA(1) )
						{
						case '\t':  case '\n':  case '\u000b':  case '\u000c':
						case '\r':  case ' ':  case '\u2028':  case '\u2029':
						{
							mWHITESPACE(true);
							theRetToken = returnToken_;
							break;
						}
						case '\\':
						{
							mUNICODE_ESCAPE_SEQUENCE(true);
							theRetToken = returnToken_;
							break;
						}
						case '\'':
						{
							mCHARACTER_LITERAL(true);
							theRetToken = returnToken_;
							break;
						}
						case '{':
						{
							mLBRACE(true);
							theRetToken = returnToken_;
							break;
						}
						case '}':
						{
							mRBRACE(true);
							theRetToken = returnToken_;
							break;
						}
						case '[':
						{
							mLBRACK(true);
							theRetToken = returnToken_;
							break;
						}
						case ']':
						{
							mRBRACK(true);
							theRetToken = returnToken_;
							break;
						}
						case '(':
						{
							mLPAREN(true);
							theRetToken = returnToken_;
							break;
						}
						case ')':
						{
							mRPAREN(true);
							theRetToken = returnToken_;
							break;
						}
						case '~':
						{
							mBNOT(true);
							theRetToken = returnToken_;
							break;
						}
						case ',':
						{
							mCOMMA(true);
							theRetToken = returnToken_;
							break;
						}
						case ':':
						{
							mCOLON(true);
							theRetToken = returnToken_;
							break;
						}
						case ';':
						{
							mSEMI(true);
							theRetToken = returnToken_;
							break;
						}
						case '?':
						{
							mQUESTION(true);
							theRetToken = returnToken_;
							break;
						}
						default:
							if ((LA(1)=='<') && (LA(2)=='<') && (LA(3)=='='))
							{
								mSL_ASN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='>') && (LA(2)=='>') && (LA(3)=='=')) {
								mSR_ASN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='/') && (LA(2)=='/')) {
								mSINGLE_LINE_COMMENT(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='/') && (LA(2)=='*')) {
								mDELIMITED_COMMENT(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='0') && (LA(2)=='X'||LA(2)=='x')) {
								mHEXADECIMAL_INTEGER_LITERAL(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='"') && (tokenSet_0_.member(LA(2)))) {
								mREGULAR_STRING_LITERAL(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='@') && (LA(2)=='"')) {
								mVERBATIM_STRING_LITERAL(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='+') && (LA(2)=='=')) {
								mPLUS_ASN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='-') && (LA(2)=='=')) {
								mMINUS_ASN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='*') && (LA(2)=='=')) {
								mSTAR_ASN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='/') && (LA(2)=='=')) {
								mDIV_ASN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='%') && (LA(2)=='=')) {
								mMOD_ASN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='+') && (LA(2)=='+')) {
								mINC(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='-') && (LA(2)=='-')) {
								mDEC(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='<') && (LA(2)=='<') && (true)) {
								mSL(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='>') && (LA(2)=='>') && (true)) {
								mSR(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='&') && (LA(2)=='=')) {
								mBAND_ASN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='|') && (LA(2)=='=')) {
								mBOR_ASN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='^') && (LA(2)=='=')) {
								mBXOR_ASN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='=') && (LA(2)=='=')) {
								mEQUAL(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='<') && (LA(2)=='=')) {
								mLE(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='>') && (LA(2)=='=')) {
								mGE(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='!') && (LA(2)=='=')) {
								mNOT_EQUAL(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='|') && (LA(2)=='|')) {
								mLOR(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='&') && (LA(2)=='&')) {
								mLAND(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='#') && (tokenSet_1_.member(LA(2)))) {
								mPP_DIRECTIVE(true);
								theRetToken = returnToken_;
							}
							else if ((tokenSet_2_.member(LA(1))) && (true)) {
								mIDENTIFIER(true);
								theRetToken = returnToken_;
							}
							else if ((tokenSet_3_.member(LA(1))) && (true)) {
								mNUMERIC_LITERAL(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='+') && (true)) {
								mPLUS(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='-') && (true)) {
								mMINUS(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='*') && (true)) {
								mSTAR(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='/') && (true)) {
								mDIV(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='%') && (true)) {
								mMOD(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='&') && (true)) {
								mBAND(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='|') && (true)) {
								mBOR(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='^') && (true)) {
								mBXOR(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='=') && (true)) {
								mASSIGN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='<') && (true)) {
								mLTHAN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='>') && (true)) {
								mGTHAN(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='!') && (true)) {
								mLNOT(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='#') && (true)) {
								mHASH(true);
								theRetToken = returnToken_;
							}
							else if ((LA(1)=='"') && (true)) {
								mQUOTE(true);
								theRetToken = returnToken_;
							}
						else
						{
							if (LA(1)==EOF_CHAR) { uponEOF(); returnToken_ = makeToken(Token.EOF_TYPE); }
				else {throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());}
						}
						break; }
						if ( null==returnToken_ ) goto tryAgain; // found SKIP token
						_ttype = returnToken_.Type;
						_ttype = testLiteralsTable(_ttype);
						returnToken_.Type = _ttype;
						return returnToken_;
					}
					catch (RecognitionException e) {
							throw new TokenStreamRecognitionException(e);
					}
				}
				catch (CharStreamException cse) {
					if ( cse is CharStreamIOException ) {
						throw new TokenStreamIOException(((CharStreamIOException)cse).io);
					}
					else {
						throw new TokenStreamException(cse.Message);
					}
				}
			}
		}
		
	protected void mNEW_LINE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = NEW_LINE;
		
		switch ( LA(1) )
		{
		case '\n':
		{
			match('\u000A');
			if (0==inputState.guessing)
			{
				newline();
			}
			break;
		}
		case '\u2028':
		{
			match('\u2028');
			if (0==inputState.guessing)
			{
				newline();
			}
			break;
		}
		case '\u2029':
		{
			match('\u2029');
			if (0==inputState.guessing)
			{
				newline();
			}
			break;
		}
		default:
			bool synPredMatched625 = false;
			if (((LA(1)=='\r') && (LA(2)=='\n') && (true) && (true)))
			{
				int _m625 = mark();
				synPredMatched625 = true;
				inputState.guessing++;
				try {
					{
						match('\u000D');
						match('\u000A');
					}
				}
				catch (RecognitionException)
				{
					synPredMatched625 = false;
				}
				rewind(_m625);
				inputState.guessing--;
			}
			if ( synPredMatched625 )
			{
				match('\u000D');
				match('\u000A');
				if (0==inputState.guessing)
				{
					newline();
				}
			}
			else if ((LA(1)=='\r') && (true) && (true) && (true)) {
				match('\u000D');
				if (0==inputState.guessing)
				{
					newline();
				}
			}
		else
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		break; }
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mWHITESPACE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = WHITESPACE;
		
		{ // ( ... )+
		int _cnt628=0;
		for (;;)
		{
			switch ( LA(1) )
			{
			case ' ':
			{
				match(' ');
				break;
			}
			case '\t':
			{
				match('\u0009');
				break;
			}
			case '\u000b':
			{
				match('\u000B');
				break;
			}
			case '\u000c':
			{
				match('\u000C');
				break;
			}
			case '\n':  case '\r':  case '\u2028':  case '\u2029':
			{
				mNEW_LINE(false);
				break;
			}
			default:
			{
				if (_cnt628 >= 1) { goto _loop628_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
			}
			break; }
			_cnt628++;
		}
_loop628_breakloop:		;
		}    // ( ... )+
		if (0==inputState.guessing)
		{
			
			_ttype = Token.SKIP;
			ExtendedToken.AddToWhitespaces (text.ToString(_begin, text.Length-_begin)) ;            
			
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mNEW_LINE_CHARACTER(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = NEW_LINE_CHARACTER;
		
		{
			switch ( LA(1) )
			{
			case '\r':
			{
				match('\u000D');
				break;
			}
			case '\n':
			{
				match('\u000A');
				break;
			}
			case '\u2028':
			{
				match('\u2028');
				break;
			}
			case '\u2029':
			{
				match('\u2029');
				break;
			}
			default:
			{
				throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
			}
			 }
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mNOT_NEW_LINE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = NOT_NEW_LINE;
		
		{
			match(tokenSet_0_);
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mSINGLE_LINE_COMMENT(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = SINGLE_LINE_COMMENT;
		
		match("//");
		{    // ( ... )*
			for (;;)
			{
				if ((tokenSet_0_.member(LA(1))))
				{
					mNOT_NEW_LINE(false);
				}
				else
				{
					goto _loop635_breakloop;
				}
				
			}
_loop635_breakloop:			;
		}    // ( ... )*
		{
			mNEW_LINE(false);
		}
		if (0==inputState.guessing)
		{
			
			_ttype = Token.SKIP;
			ExtendedToken.AddToWhitespaces (text.ToString(_begin, text.Length-_begin));
			
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mDELIMITED_COMMENT(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = DELIMITED_COMMENT;
		
		match("/*");
		{    // ( ... )*
			for (;;)
			{
				switch ( LA(1) )
				{
				case '\n':  case '\r':  case '\u2028':  case '\u2029':
				{
					mNEW_LINE(false);
					break;
				}
				case '\t':  case '\u000b':  case '\u000c':  case ' ':
				case '!':  case '"':  case '#':  case '$':
				case '%':  case '&':  case '\'':  case '(':
				case ')':  case '+':  case ',':  case '-':
				case '.':  case '/':  case '0':  case '1':
				case '2':  case '3':  case '4':  case '5':
				case '6':  case '7':  case '8':  case '9':
				case ':':  case ';':  case '<':  case '=':
				case '>':  case '?':  case '@':  case 'A':
				case 'B':  case 'C':  case 'D':  case 'E':
				case 'F':  case 'G':  case 'H':  case 'I':
				case 'J':  case 'K':  case 'L':  case 'M':
				case 'N':  case 'O':  case 'P':  case 'Q':
				case 'R':  case 'S':  case 'T':  case 'U':
				case 'V':  case 'W':  case 'X':  case 'Y':
				case 'Z':  case '[':  case '\\':  case ']':
				case '^':  case '_':  case 'a':  case 'b':
				case 'c':  case 'd':  case 'e':  case 'f':
				case 'g':  case 'h':  case 'i':  case 'j':
				case 'k':  case 'l':  case 'm':  case 'n':
				case 'o':  case 'p':  case 'q':  case 'r':
				case 's':  case 't':  case 'u':  case 'v':
				case 'w':  case 'x':  case 'y':  case 'z':
				case '{':  case '|':  case '}':  case '~':
				{
					{
						match(tokenSet_4_);
					}
					break;
				}
				default:
					if (((LA(1)=='*') && (tokenSet_5_.member(LA(2))) && (tokenSet_5_.member(LA(3))))&&( LA(2)!='/' ))
					{
						match('*');
					}
				else
				{
					goto _loop640_breakloop;
				}
				break; }
			}
_loop640_breakloop:			;
		}    // ( ... )*
		match("*/");
		if (0==inputState.guessing)
		{
			
			_ttype = Token.SKIP;
			ExtendedToken.AddToWhitespaces (text.ToString(_begin, text.Length-_begin));
			
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mUNICODE_ESCAPE_SEQUENCE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = UNICODE_ESCAPE_SEQUENCE;
		
		if ((LA(1)=='\\') && (LA(2)=='u'))
		{
			{
				match('\\');
				match('u');
				mHEX_DIGIT(false);
				mHEX_DIGIT(false);
				mHEX_DIGIT(false);
				mHEX_DIGIT(false);
			}
		}
		else if ((LA(1)=='\\') && (LA(2)=='U')) {
			{
				match('\\');
				match('U');
				mHEX_DIGIT(false);
				mHEX_DIGIT(false);
				mHEX_DIGIT(false);
				mHEX_DIGIT(false);
				mHEX_DIGIT(false);
				mHEX_DIGIT(false);
				mHEX_DIGIT(false);
				mHEX_DIGIT(false);
			}
		}
		else
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mHEX_DIGIT(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = HEX_DIGIT;
		
		switch ( LA(1) )
		{
		case '0':
		{
			match('0');
			break;
		}
		case '1':
		{
			match('1');
			break;
		}
		case '2':
		{
			match('2');
			break;
		}
		case '3':
		{
			match('3');
			break;
		}
		case '4':
		{
			match('4');
			break;
		}
		case '5':
		{
			match('5');
			break;
		}
		case '6':
		{
			match('6');
			break;
		}
		case '7':
		{
			match('7');
			break;
		}
		case '8':
		{
			match('8');
			break;
		}
		case '9':
		{
			match('9');
			break;
		}
		case 'A':
		{
			match('A');
			break;
		}
		case 'B':
		{
			match('B');
			break;
		}
		case 'C':
		{
			match('C');
			break;
		}
		case 'D':
		{
			match('D');
			break;
		}
		case 'E':
		{
			match('E');
			break;
		}
		case 'F':
		{
			match('F');
			break;
		}
		case 'a':
		{
			match('a');
			break;
		}
		case 'b':
		{
			match('b');
			break;
		}
		case 'c':
		{
			match('c');
			break;
		}
		case 'd':
		{
			match('d');
			break;
		}
		case 'e':
		{
			match('e');
			break;
		}
		case 'f':
		{
			match('f');
			break;
		}
		default:
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		 }
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mIDENTIFIER(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = IDENTIFIER;
		
		switch ( LA(1) )
		{
		case '$':  case 'A':  case 'B':  case 'C':
		case 'D':  case 'E':  case 'F':  case 'G':
		case 'H':  case 'I':  case 'J':  case 'K':
		case 'L':  case 'M':  case 'N':  case 'O':
		case 'P':  case 'Q':  case 'R':  case 'S':
		case 'T':  case 'U':  case 'V':  case 'W':
		case 'X':  case 'Y':  case 'Z':  case '_':
		case 'a':  case 'b':  case 'c':  case 'd':
		case 'e':  case 'f':  case 'g':  case 'h':
		case 'i':  case 'j':  case 'k':  case 'l':
		case 'm':  case 'n':  case 'o':  case 'p':
		case 'q':  case 'r':  case 's':  case 't':
		case 'u':  case 'v':  case 'w':  case 'x':
		case 'y':  case 'z':
		{
			mIDENTIFIER_START_CHARACTER(false);
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_6_.member(LA(1))))
					{
						mIDENTIFIER_PART_CHARACTER(false);
					}
					else
					{
						goto _loop646_breakloop;
					}
					
				}
_loop646_breakloop:				;
			}    // ( ... )*
			break;
		}
		case '@':
		{
			match('@');
			mIDENTIFIER_START_CHARACTER(false);
			{    // ( ... )*
				for (;;)
				{
					if ((tokenSet_6_.member(LA(1))))
					{
						mIDENTIFIER_PART_CHARACTER(false);
					}
					else
					{
						goto _loop648_breakloop;
					}
					
				}
_loop648_breakloop:				;
			}    // ( ... )*
			break;
		}
		default:
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		 }
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mIDENTIFIER_START_CHARACTER(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = IDENTIFIER_START_CHARACTER;
		
		{
			switch ( LA(1) )
			{
			case 'a':  case 'b':  case 'c':  case 'd':
			case 'e':  case 'f':  case 'g':  case 'h':
			case 'i':  case 'j':  case 'k':  case 'l':
			case 'm':  case 'n':  case 'o':  case 'p':
			case 'q':  case 'r':  case 's':  case 't':
			case 'u':  case 'v':  case 'w':  case 'x':
			case 'y':  case 'z':
			{
				matchRange('a','z');
				break;
			}
			case 'A':  case 'B':  case 'C':  case 'D':
			case 'E':  case 'F':  case 'G':  case 'H':
			case 'I':  case 'J':  case 'K':  case 'L':
			case 'M':  case 'N':  case 'O':  case 'P':
			case 'Q':  case 'R':  case 'S':  case 'T':
			case 'U':  case 'V':  case 'W':  case 'X':
			case 'Y':  case 'Z':
			{
				matchRange('A','Z');
				break;
			}
			case '_':
			{
				match('_');
				break;
			}
			case '$':
			{
				match('$');
				break;
			}
			default:
			{
				throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
			}
			 }
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mIDENTIFIER_PART_CHARACTER(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = IDENTIFIER_PART_CHARACTER;
		
		{
			switch ( LA(1) )
			{
			case 'a':  case 'b':  case 'c':  case 'd':
			case 'e':  case 'f':  case 'g':  case 'h':
			case 'i':  case 'j':  case 'k':  case 'l':
			case 'm':  case 'n':  case 'o':  case 'p':
			case 'q':  case 'r':  case 's':  case 't':
			case 'u':  case 'v':  case 'w':  case 'x':
			case 'y':  case 'z':
			{
				matchRange('a','z');
				break;
			}
			case 'A':  case 'B':  case 'C':  case 'D':
			case 'E':  case 'F':  case 'G':  case 'H':
			case 'I':  case 'J':  case 'K':  case 'L':
			case 'M':  case 'N':  case 'O':  case 'P':
			case 'Q':  case 'R':  case 'S':  case 'T':
			case 'U':  case 'V':  case 'W':  case 'X':
			case 'Y':  case 'Z':
			{
				matchRange('A','Z');
				break;
			}
			case '_':
			{
				match('_');
				break;
			}
			case '0':  case '1':  case '2':  case '3':
			case '4':  case '5':  case '6':  case '7':
			case '8':  case '9':
			{
				matchRange('0','9');
				break;
			}
			case '$':
			{
				match('$');
				break;
			}
			default:
			{
				throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
			}
			 }
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mDECIMAL_DIGIT(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = DECIMAL_DIGIT;
		
		switch ( LA(1) )
		{
		case '0':
		{
			match('0');
			break;
		}
		case '1':
		{
			match('1');
			break;
		}
		case '2':
		{
			match('2');
			break;
		}
		case '3':
		{
			match('3');
			break;
		}
		case '4':
		{
			match('4');
			break;
		}
		case '5':
		{
			match('5');
			break;
		}
		case '6':
		{
			match('6');
			break;
		}
		case '7':
		{
			match('7');
			break;
		}
		case '8':
		{
			match('8');
			break;
		}
		case '9':
		{
			match('9');
			break;
		}
		default:
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		 }
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mHEXADECIMAL_INTEGER_LITERAL(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = HEXADECIMAL_INTEGER_LITERAL;
		
		if ((LA(1)=='0') && (LA(2)=='x'))
		{
			match("0x");
			{ // ( ... )+
			int _cnt656=0;
			for (;;)
			{
				if ((tokenSet_7_.member(LA(1))))
				{
					mHEX_DIGIT(false);
				}
				else
				{
					if (_cnt656 >= 1) { goto _loop656_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
				}
				
				_cnt656++;
			}
_loop656_breakloop:			;
			}    // ( ... )+
			{
				if ((tokenSet_8_.member(LA(1))))
				{
					mINTEGER_TYPE_SUFFIX(false);
				}
				else {
				}
				
			}
		}
		else if ((LA(1)=='0') && (LA(2)=='X')) {
			match("0X");
			{ // ( ... )+
			int _cnt659=0;
			for (;;)
			{
				if ((tokenSet_7_.member(LA(1))))
				{
					mHEX_DIGIT(false);
				}
				else
				{
					if (_cnt659 >= 1) { goto _loop659_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
				}
				
				_cnt659++;
			}
_loop659_breakloop:			;
			}    // ( ... )+
			{
				if ((tokenSet_8_.member(LA(1))))
				{
					mINTEGER_TYPE_SUFFIX(false);
				}
				else {
				}
				
			}
		}
		else
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mINTEGER_TYPE_SUFFIX(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = INTEGER_TYPE_SUFFIX;
		
		if ((LA(1)=='U') && (LA(2)=='L'))
		{
			match("UL");
		}
		else if ((LA(1)=='U') && (LA(2)=='l')) {
			match("Ul");
		}
		else if ((LA(1)=='u') && (LA(2)=='L')) {
			match("uL");
		}
		else if ((LA(1)=='u') && (LA(2)=='l')) {
			match("ul");
		}
		else if ((LA(1)=='L') && (LA(2)=='U')) {
			match("LU");
		}
		else if ((LA(1)=='L') && (LA(2)=='u')) {
			match("Lu");
		}
		else if ((LA(1)=='l') && (LA(2)=='U')) {
			match("lU");
		}
		else if ((LA(1)=='l') && (LA(2)=='u')) {
			match("lu");
		}
		else if ((LA(1)=='U') && (true)) {
			match('U');
		}
		else if ((LA(1)=='u') && (true)) {
			match('u');
		}
		else if ((LA(1)=='L') && (true)) {
			match('L');
		}
		else if ((LA(1)=='l') && (true)) {
			match('l');
		}
		else
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mNUMERIC_LITERAL(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = NUMERIC_LITERAL;
		
		bool synPredMatched674 = false;
		if ((((LA(1) >= '0' && LA(1) <= '9')) && (tokenSet_3_.member(LA(2))) && (tokenSet_3_.member(LA(3))) && (true)))
		{
			int _m674 = mark();
			synPredMatched674 = true;
			inputState.guessing++;
			try {
				{
					{ // ( ... )+
					int _cnt673=0;
					for (;;)
					{
						if (((LA(1) >= '0' && LA(1) <= '9')))
						{
							mDECIMAL_DIGIT(false);
						}
						else
						{
							if (_cnt673 >= 1) { goto _loop673_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
						}
						
						_cnt673++;
					}
_loop673_breakloop:					;
					}    // ( ... )+
					match(".");
				}
			}
			catch (RecognitionException)
			{
				synPredMatched674 = false;
			}
			rewind(_m674);
			inputState.guessing--;
		}
		if ( synPredMatched674 )
		{
			{ // ( ... )+
			int _cnt676=0;
			for (;;)
			{
				if (((LA(1) >= '0' && LA(1) <= '9')))
				{
					mDECIMAL_DIGIT(false);
				}
				else
				{
					if (_cnt676 >= 1) { goto _loop676_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
				}
				
				_cnt676++;
			}
_loop676_breakloop:			;
			}    // ( ... )+
			match(".");
			{ // ( ... )+
			int _cnt678=0;
			for (;;)
			{
				if (((LA(1) >= '0' && LA(1) <= '9')))
				{
					mDECIMAL_DIGIT(false);
				}
				else
				{
					if (_cnt678 >= 1) { goto _loop678_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
				}
				
				_cnt678++;
			}
_loop678_breakloop:			;
			}    // ( ... )+
			{
				if ((LA(1)=='E'||LA(1)=='e'))
				{
					mEXPONENT_PART(false);
				}
				else {
				}
				
			}
			{
				if ((tokenSet_9_.member(LA(1))))
				{
					{
						switch ( LA(1) )
						{
						case 'F':
						{
							match("F");
							break;
						}
						case 'f':
						{
							match("f");
							break;
						}
						case 'D':
						{
							match("D");
							break;
						}
						case 'd':
						{
							match("d");
							break;
						}
						case 'M':
						{
							match("M");
							break;
						}
						case 'm':
						{
							match("m");
							break;
						}
						default:
						{
							throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
						}
						 }
					}
				}
				else {
				}
				
			}
			if (0==inputState.guessing)
			{
				_ttype = REAL_LITERAL;
			}
		}
		else {
			bool synPredMatched685 = false;
			if ((((LA(1) >= '0' && LA(1) <= '9')) && (tokenSet_10_.member(LA(2))) && (tokenSet_11_.member(LA(3))) && (true)))
			{
				int _m685 = mark();
				synPredMatched685 = true;
				inputState.guessing++;
				try {
					{
						{ // ( ... )+
						int _cnt684=0;
						for (;;)
						{
							if (((LA(1) >= '0' && LA(1) <= '9')))
							{
								mDECIMAL_DIGIT(false);
							}
							else
							{
								if (_cnt684 >= 1) { goto _loop684_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
							}
							
							_cnt684++;
						}
_loop684_breakloop:						;
						}    // ( ... )+
						mEXPONENT_PART(false);
					}
				}
				catch (RecognitionException)
				{
					synPredMatched685 = false;
				}
				rewind(_m685);
				inputState.guessing--;
			}
			if ( synPredMatched685 )
			{
				{ // ( ... )+
				int _cnt687=0;
				for (;;)
				{
					if (((LA(1) >= '0' && LA(1) <= '9')))
					{
						mDECIMAL_DIGIT(false);
					}
					else
					{
						if (_cnt687 >= 1) { goto _loop687_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
					}
					
					_cnt687++;
				}
_loop687_breakloop:				;
				}    // ( ... )+
				mEXPONENT_PART(false);
				{
					if ((tokenSet_9_.member(LA(1))))
					{
						{
							switch ( LA(1) )
							{
							case 'F':
							{
								match("F");
								break;
							}
							case 'f':
							{
								match("f");
								break;
							}
							case 'D':
							{
								match("D");
								break;
							}
							case 'd':
							{
								match("d");
								break;
							}
							case 'M':
							{
								match("M");
								break;
							}
							case 'm':
							{
								match("m");
								break;
							}
							default:
							{
								throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
							}
							 }
						}
					}
					else {
					}
					
				}
				if (0==inputState.guessing)
				{
					_ttype = REAL_LITERAL;
				}
			}
			else {
				bool synPredMatched665 = false;
				if (((LA(1)=='.') && ((LA(2) >= '0' && LA(2) <= '9'))))
				{
					int _m665 = mark();
					synPredMatched665 = true;
					inputState.guessing++;
					try {
						{
							match(".");
						}
					}
					catch (RecognitionException)
					{
						synPredMatched665 = false;
					}
					rewind(_m665);
					inputState.guessing--;
				}
				if ( synPredMatched665 )
				{
					match(".");
					{ // ( ... )+
					int _cnt667=0;
					for (;;)
					{
						if (((LA(1) >= '0' && LA(1) <= '9')))
						{
							mDECIMAL_DIGIT(false);
						}
						else
						{
							if (_cnt667 >= 1) { goto _loop667_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
						}
						
						_cnt667++;
					}
_loop667_breakloop:					;
					}    // ( ... )+
					{
						if ((LA(1)=='E'||LA(1)=='e'))
						{
							mEXPONENT_PART(false);
						}
						else {
						}
						
					}
					{
						if ((tokenSet_9_.member(LA(1))))
						{
							{
								switch ( LA(1) )
								{
								case 'F':
								{
									match("F");
									break;
								}
								case 'f':
								{
									match("f");
									break;
								}
								case 'D':
								{
									match("D");
									break;
								}
								case 'd':
								{
									match("d");
									break;
								}
								case 'M':
								{
									match("M");
									break;
								}
								case 'm':
								{
									match("m");
									break;
								}
								default:
								{
									throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
								}
								 }
							}
						}
						else {
						}
						
					}
					if (0==inputState.guessing)
					{
						_ttype = REAL_LITERAL;
					}
				}
				else {
					bool synPredMatched694 = false;
					if ((((LA(1) >= '0' && LA(1) <= '9')) && (tokenSet_12_.member(LA(2))) && (true) && (true)))
					{
						int _m694 = mark();
						synPredMatched694 = true;
						inputState.guessing++;
						try {
							{
								{ // ( ... )+
								int _cnt692=0;
								for (;;)
								{
									if (((LA(1) >= '0' && LA(1) <= '9')))
									{
										mDECIMAL_DIGIT(false);
									}
									else
									{
										if (_cnt692 >= 1) { goto _loop692_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
									}
									
									_cnt692++;
								}
_loop692_breakloop:								;
								}    // ( ... )+
								{
									switch ( LA(1) )
									{
									case 'F':
									{
										match("F");
										break;
									}
									case 'f':
									{
										match("f");
										break;
									}
									case 'D':
									{
										match("D");
										break;
									}
									case 'd':
									{
										match("d");
										break;
									}
									case 'M':
									{
										match("M");
										break;
									}
									case 'm':
									{
										match("m");
										break;
									}
									default:
									{
										throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
									}
									 }
								}
							}
						}
						catch (RecognitionException)
						{
							synPredMatched694 = false;
						}
						rewind(_m694);
						inputState.guessing--;
					}
					if ( synPredMatched694 )
					{
						{ // ( ... )+
						int _cnt696=0;
						for (;;)
						{
							if (((LA(1) >= '0' && LA(1) <= '9')))
							{
								mDECIMAL_DIGIT(false);
							}
							else
							{
								if (_cnt696 >= 1) { goto _loop696_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
							}
							
							_cnt696++;
						}
_loop696_breakloop:						;
						}    // ( ... )+
						{
							switch ( LA(1) )
							{
							case 'F':
							{
								match("F");
								break;
							}
							case 'f':
							{
								match("f");
								break;
							}
							case 'D':
							{
								match("D");
								break;
							}
							case 'd':
							{
								match("d");
								break;
							}
							case 'M':
							{
								match("M");
								break;
							}
							case 'm':
							{
								match("m");
								break;
							}
							default:
							{
								throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
							}
							 }
						}
						if (0==inputState.guessing)
						{
							_ttype = REAL_LITERAL;
						}
					}
					else if (((LA(1) >= '0' && LA(1) <= '9')) && (true) && (true) && (true)) {
						{ // ( ... )+
						int _cnt699=0;
						for (;;)
						{
							if (((LA(1) >= '0' && LA(1) <= '9')))
							{
								mDECIMAL_DIGIT(false);
							}
							else
							{
								if (_cnt699 >= 1) { goto _loop699_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
							}
							
							_cnt699++;
						}
_loop699_breakloop:						;
						}    // ( ... )+
						{
							if ((tokenSet_8_.member(LA(1))))
							{
								mINTEGER_TYPE_SUFFIX(false);
							}
							else {
							}
							
						}
						if (0==inputState.guessing)
						{
							_ttype = INTEGER_LITERAL;
						}
					}
					else if ((LA(1)=='.') && (true)) {
						match('.');
						if (0==inputState.guessing)
						{
							_ttype = DOT;
						}
					}
					else
					{
						throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
					}
					}}}
					if (_createToken && (null == _token) && (_ttype != Token.SKIP))
					{
						_token = makeToken(_ttype);
						_token.setText(text.ToString(_begin, text.Length-_begin));
					}
					returnToken_ = _token;
				}
				
	protected void mEXPONENT_PART(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = EXPONENT_PART;
		
		switch ( LA(1) )
		{
		case 'e':
		{
			match("e");
			{
				switch ( LA(1) )
				{
				case '+':  case '-':
				{
					mSIGN(false);
					break;
				}
				case '0':  case '1':  case '2':  case '3':
				case '4':  case '5':  case '6':  case '7':
				case '8':  case '9':
				{
					break;
				}
				default:
				{
					throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
				}
				 }
			}
			{ // ( ... )+
			int _cnt709=0;
			for (;;)
			{
				if (((LA(1) >= '0' && LA(1) <= '9')))
				{
					mDECIMAL_DIGIT(false);
				}
				else
				{
					if (_cnt709 >= 1) { goto _loop709_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
				}
				
				_cnt709++;
			}
_loop709_breakloop:			;
			}    // ( ... )+
			break;
		}
		case 'E':
		{
			match("E");
			{
				switch ( LA(1) )
				{
				case '+':  case '-':
				{
					mSIGN(false);
					break;
				}
				case '0':  case '1':  case '2':  case '3':
				case '4':  case '5':  case '6':  case '7':
				case '8':  case '9':
				{
					break;
				}
				default:
				{
					throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
				}
				 }
			}
			{ // ( ... )+
			int _cnt712=0;
			for (;;)
			{
				if (((LA(1) >= '0' && LA(1) <= '9')))
				{
					mDECIMAL_DIGIT(false);
				}
				else
				{
					if (_cnt712 >= 1) { goto _loop712_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
				}
				
				_cnt712++;
			}
_loop712_breakloop:			;
			}    // ( ... )+
			break;
		}
		default:
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		 }
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mSIGN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = SIGN;
		
		switch ( LA(1) )
		{
		case '+':
		{
			mPLUS(false);
			break;
		}
		case '-':
		{
			mMINUS(false);
			break;
		}
		default:
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		 }
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mPLUS(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PLUS;
		
		match('+');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mMINUS(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = MINUS;
		
		match('-');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mCHARACTER_LITERAL(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = CHARACTER_LITERAL;
		
		match("'");
		mCHARACTER(false);
		match("'");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mCHARACTER(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = CHARACTER;
		
		if ((LA(1)=='\\') && (tokenSet_13_.member(LA(2))))
		{
			mSIMPLE_ESCAPE_SEQUENCE(false);
		}
		else if ((LA(1)=='\\') && (LA(2)=='x')) {
			mHEXADECIMAL_ESCAPE_SEQUENCE(false);
		}
		else if ((LA(1)=='\\') && (LA(2)=='U'||LA(2)=='u')) {
			mUNICODE_ESCAPE_SEQUENCE(false);
		}
		else if ((tokenSet_14_.member(LA(1)))) {
			mSIMPLE_CHARACTER(false);
		}
		else
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mSIMPLE_CHARACTER(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = SIMPLE_CHARACTER;
		
		{
			match(tokenSet_14_);
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mSIMPLE_ESCAPE_SEQUENCE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = SIMPLE_ESCAPE_SEQUENCE;
		
		if ((LA(1)=='\\') && (LA(2)=='\''))
		{
			match("\\'");
		}
		else if ((LA(1)=='\\') && (LA(2)=='"')) {
			match("\\\"");
		}
		else if ((LA(1)=='\\') && (LA(2)=='\\')) {
			match("\\\\");
		}
		else if ((LA(1)=='\\') && (LA(2)=='0')) {
			match("\\0");
		}
		else if ((LA(1)=='\\') && (LA(2)=='a')) {
			match("\\a");
		}
		else if ((LA(1)=='\\') && (LA(2)=='b')) {
			match("\\b");
		}
		else if ((LA(1)=='\\') && (LA(2)=='f')) {
			match("\\f");
		}
		else if ((LA(1)=='\\') && (LA(2)=='n')) {
			match("\\n");
		}
		else if ((LA(1)=='\\') && (LA(2)=='r')) {
			match("\\r");
		}
		else if ((LA(1)=='\\') && (LA(2)=='t')) {
			match("\\t");
		}
		else if ((LA(1)=='\\') && (LA(2)=='v')) {
			match("\\v");
		}
		else
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mHEXADECIMAL_ESCAPE_SEQUENCE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = HEXADECIMAL_ESCAPE_SEQUENCE;
		
		{
			match('\\');
			match('x');
			mHEX_DIGIT(false);
		}
		{
			if ((tokenSet_7_.member(LA(1))) && (tokenSet_0_.member(LA(2))) && (true) && (true))
			{
				mHEX_DIGIT(false);
				{
					if ((tokenSet_7_.member(LA(1))) && (tokenSet_0_.member(LA(2))) && (true) && (true))
					{
						mHEX_DIGIT(false);
						{
							if ((tokenSet_7_.member(LA(1))) && (tokenSet_0_.member(LA(2))) && (true) && (true))
							{
								mHEX_DIGIT(false);
							}
							else if ((tokenSet_0_.member(LA(1))) && (true) && (true) && (true)) {
							}
							else
							{
								throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
							}
							
						}
					}
					else if ((tokenSet_0_.member(LA(1))) && (true) && (true) && (true)) {
					}
					else
					{
						throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
					}
					
				}
			}
			else if ((tokenSet_0_.member(LA(1))) && (true) && (true) && (true)) {
			}
			else
			{
				throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
			}
			
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mREGULAR_STRING_LITERAL(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = REGULAR_STRING_LITERAL;
		
		match('\"');
		{    // ( ... )*
			for (;;)
			{
				if ((tokenSet_15_.member(LA(1))))
				{
					mREGULAR_STRING_LITERAL_CHARACTER(false);
				}
				else
				{
					goto _loop726_breakloop;
				}
				
			}
_loop726_breakloop:			;
		}    // ( ... )*
		match('\"');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mREGULAR_STRING_LITERAL_CHARACTER(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = REGULAR_STRING_LITERAL_CHARACTER;
		
		if ((LA(1)=='\\') && (tokenSet_13_.member(LA(2))))
		{
			mSIMPLE_ESCAPE_SEQUENCE(false);
		}
		else if ((LA(1)=='\\') && (LA(2)=='x')) {
			mHEXADECIMAL_ESCAPE_SEQUENCE(false);
		}
		else if ((LA(1)=='\\') && (LA(2)=='U'||LA(2)=='u')) {
			mUNICODE_ESCAPE_SEQUENCE(false);
		}
		else if ((tokenSet_16_.member(LA(1)))) {
			mSINGLE_REGULAR_STRING_LITERAL_CHARCACTER(false);
		}
		else
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mSINGLE_REGULAR_STRING_LITERAL_CHARCACTER(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = SINGLE_REGULAR_STRING_LITERAL_CHARCACTER;
		
		{
			match(tokenSet_16_);
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mVERBATIM_STRING_LITERAL(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = VERBATIM_STRING_LITERAL;
		char  ch = '\0';
		string s="";
		
		match('@');
		match("\"");
		{    // ( ... )*
			for (;;)
			{
				switch ( LA(1) )
				{
				case '\\':
				{
					match("\\");
					if (0==inputState.guessing)
					{
						s+=("\\\\");
					}
					break;
				}
				case '\t':  case '\n':  case '\u000b':  case '\u000c':
				case '\r':  case ' ':  case '!':  case '#':
				case '$':  case '%':  case '&':  case '\'':
				case '(':  case ')':  case '*':  case '+':
				case ',':  case '-':  case '.':  case '/':
				case '0':  case '1':  case '2':  case '3':
				case '4':  case '5':  case '6':  case '7':
				case '8':  case '9':  case ':':  case ';':
				case '<':  case '=':  case '>':  case '?':
				case '@':  case 'A':  case 'B':  case 'C':
				case 'D':  case 'E':  case 'F':  case 'G':
				case 'H':  case 'I':  case 'J':  case 'K':
				case 'L':  case 'M':  case 'N':  case 'O':
				case 'P':  case 'Q':  case 'R':  case 'S':
				case 'T':  case 'U':  case 'V':  case 'W':
				case 'X':  case 'Y':  case 'Z':  case '[':
				case ']':  case '^':  case '_':  case 'a':
				case 'b':  case 'c':  case 'd':  case 'e':
				case 'f':  case 'g':  case 'h':  case 'i':
				case 'j':  case 'k':  case 'l':  case 'm':
				case 'n':  case 'o':  case 'p':  case 'q':
				case 'r':  case 's':  case 't':  case 'u':
				case 'v':  case 'w':  case 'x':  case 'y':
				case 'z':  case '{':  case '|':  case '}':
				case '~':  case '\u2028':  case '\u2029':
				{
					{
						ch = LA(1);
						match(tokenSet_17_);
					}
					if (0==inputState.guessing)
					{
						s+=(ch);
					}
					break;
				}
				default:
					if ((LA(1)=='"') && (LA(2)=='"'))
					{
						match("\"\"");
						if (0==inputState.guessing)
						{
							s+=("\"");
						}
					}
				else
				{
					goto _loop733_breakloop;
				}
				break; }
			}
_loop733_breakloop:			;
		}    // ( ... )*
		match("\"");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mLBRACE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = LBRACE;
		
		match('{');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mRBRACE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = RBRACE;
		
		match('}');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mLBRACK(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = LBRACK;
		
		match('[');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mRBRACK(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = RBRACK;
		
		match(']');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mLPAREN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = LPAREN;
		
		match('(');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mRPAREN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = RPAREN;
		
		match(')');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mPLUS_ASN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PLUS_ASN;
		
		match("+=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mMINUS_ASN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = MINUS_ASN;
		
		match("-=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mSTAR(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = STAR;
		
		match('*');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mSTAR_ASN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = STAR_ASN;
		
		match("*=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mDIV(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = DIV;
		
		match('/');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mDIV_ASN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = DIV_ASN;
		
		match("/=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mMOD(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = MOD;
		
		match('%');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mMOD_ASN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = MOD_ASN;
		
		match("%=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mINC(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = INC;
		
		match("++");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mDEC(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = DEC;
		
		match("--");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mSL(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = SL;
		
		match("<<");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mSL_ASN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = SL_ASN;
		
		match("<<=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mSR(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = SR;
		
		match(">>");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mSR_ASN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = SR_ASN;
		
		match(">>=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mBAND(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = BAND;
		
		match('&');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mBAND_ASN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = BAND_ASN;
		
		match("&=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mBOR(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = BOR;
		
		match('|');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mBOR_ASN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = BOR_ASN;
		
		match("|=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mBXOR(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = BXOR;
		
		match('^');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mBXOR_ASN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = BXOR_ASN;
		
		match("^=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mBNOT(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = BNOT;
		
		match('~');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mASSIGN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = ASSIGN;
		
		match('=');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mEQUAL(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = EQUAL;
		
		match("==");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mLTHAN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = LTHAN;
		
		match('<');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mLE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = LE;
		
		match("<=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mGTHAN(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = GTHAN;
		
		match(">");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mGE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = GE;
		
		match(">=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mLNOT(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = LNOT;
		
		match('!');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mNOT_EQUAL(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = NOT_EQUAL;
		
		match("!=");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mLOR(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = LOR;
		
		match("||");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mLAND(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = LAND;
		
		match("&&");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mCOMMA(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = COMMA;
		
		match(',');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mCOLON(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = COLON;
		
		match(':');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mSEMI(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = SEMI;
		
		match(';');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mHASH(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = HASH;
		
		match('#');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mQUOTE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = QUOTE;
		
		match("\"");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mQUESTION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = QUESTION;
		
		match('?');
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	public void mPP_DIRECTIVE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_DIRECTIVE;
		
		mHASH(false);
		{
			switch ( LA(1) )
			{
			case '\t':  case '\u000b':  case '\u000c':  case ' ':
			{
				mPP_WHITESPACE(false);
				break;
			}
			case 'd':  case 'e':  case 'i':  case 'l':
			case 'r':  case 'u':  case 'w':
			{
				break;
			}
			default:
			{
				throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
			}
			 }
		}
		{
			switch ( LA(1) )
			{
			case 'd':  case 'u':
			{
				mPP_DECLARATION(false);
				break;
			}
			case 'l':
			{
				mPP_LINE(false);
				break;
			}
			case 'r':
			{
				mPP_START_REGION(false);
				break;
			}
			default:
				bool synPredMatched783 = false;
				if (((LA(1)=='e') && (LA(2)=='n') && (LA(3)=='d') && (LA(4)=='r')))
				{
					int _m783 = mark();
					synPredMatched783 = true;
					inputState.guessing++;
					try {
						{
							mPP_END_REGION(false);
						}
					}
					catch (RecognitionException)
					{
						synPredMatched783 = false;
					}
					rewind(_m783);
					inputState.guessing--;
				}
				if ( synPredMatched783 )
				{
					mPP_END_REGION(false);
				}
				else if ((LA(1)=='e'||LA(1)=='i') && (LA(2)=='f'||LA(2)=='l'||LA(2)=='n') && (tokenSet_18_.member(LA(3))) && (tokenSet_19_.member(LA(4)))) {
					mPP_CONDITIONAL(false);
				}
				else if ((LA(1)=='e'||LA(1)=='w') && (LA(2)=='a'||LA(2)=='r')) {
					mPP_DIAGNOSTIC(false);
				}
			else
			{
				throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
			}
			break; }
		}
		if (0==inputState.guessing)
		{
			_ttype = Token.SKIP;
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_WHITESPACE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_WHITESPACE;
		
		{ // ( ... )+
		int _cnt786=0;
		for (;;)
		{
			switch ( LA(1) )
			{
			case ' ':
			{
				match(' ');
				break;
			}
			case '\t':
			{
				match('\u0009');
				break;
			}
			case '\u000b':
			{
				match('\u000B');
				break;
			}
			case '\u000c':
			{
				match('\u000C');
				break;
			}
			default:
			{
				if (_cnt786 >= 1) { goto _loop786_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
			}
			break; }
			_cnt786++;
		}
_loop786_breakloop:		;
		}    // ( ... )+
		if (0==inputState.guessing)
		{
			_ttype = Token.SKIP;
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_DECLARATION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_DECLARATION;
		
		switch ( LA(1) )
		{
		case 'd':
		{
			match("define");
			mPP_WHITESPACE(false);
			mCONDITIONAL_SYMBOL(false);
			mPP_NEW_LINE(false);
			break;
		}
		case 'u':
		{
			match("undef");
			mPP_WHITESPACE(false);
			mCONDITIONAL_SYMBOL(false);
			mPP_NEW_LINE(false);
			break;
		}
		default:
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		 }
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_END_REGION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_END_REGION;
		
		match("endregion");
		mPP_MESSAGE(false);
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_CONDITIONAL(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_CONDITIONAL;
		
		if ((LA(1)=='e') && (LA(2)=='l') && (LA(3)=='i'))
		{
			mPP_ELIF_SECTION(false);
		}
		else if ((LA(1)=='e') && (LA(2)=='l') && (LA(3)=='s')) {
			mPP_ELSE_SECTION(false);
		}
		else if ((LA(1)=='e') && (LA(2)=='n')) {
			mPP_ENDIF(false);
		}
		else if ((LA(1)=='i')) {
			mPP_IF_SECTION(false);
		}
		else
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_LINE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_LINE;
		
		match("line");
		mPP_WHITESPACE(false);
		mLINE_INDICATOR(false);
		mPP_NEW_LINE(false);
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_DIAGNOSTIC(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_DIAGNOSTIC;
		
		switch ( LA(1) )
		{
		case 'e':
		{
			match("error");
			mPP_MESSAGE(false);
			break;
		}
		case 'w':
		{
			match("warning");
			mPP_MESSAGE(false);
			break;
		}
		default:
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		 }
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_START_REGION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_START_REGION;
		
		match("region");
		mPP_MESSAGE(false);
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_NEW_LINE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_NEW_LINE;
		
		{
			switch ( LA(1) )
			{
			case '/':
			{
				mSINGLE_LINE_COMMENT(false);
				break;
			}
			case '\n':  case '\r':  case '\u2028':  case '\u2029':
			{
				mNEW_LINE(false);
				break;
			}
			default:
			{
				throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
			}
			 }
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_EXPRESSION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_EXPRESSION;
		
		mPP_OR_EXPRESSION(false);
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_OR_EXPRESSION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_OR_EXPRESSION;
		
		mPP_AND_EXPRESSION(false);
		{    // ( ... )*
			for (;;)
			{
				if ((tokenSet_20_.member(LA(1))) && (tokenSet_20_.member(LA(2))) && (tokenSet_21_.member(LA(3))) && (tokenSet_22_.member(LA(4))))
				{
					{
						switch ( LA(1) )
						{
						case '\t':  case '\u000b':  case '\u000c':  case ' ':
						{
							mPP_WHITESPACE(false);
							break;
						}
						case '|':
						{
							break;
						}
						default:
						{
							throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
						}
						 }
					}
					mLOR(false);
					{
						switch ( LA(1) )
						{
						case '\t':  case '\u000b':  case '\u000c':  case ' ':
						{
							mPP_WHITESPACE(false);
							break;
						}
						case '!':  case '$':  case '(':  case '@':
						case 'A':  case 'B':  case 'C':  case 'D':
						case 'E':  case 'F':  case 'G':  case 'H':
						case 'I':  case 'J':  case 'K':  case 'L':
						case 'M':  case 'N':  case 'O':  case 'P':
						case 'Q':  case 'R':  case 'S':  case 'T':
						case 'U':  case 'V':  case 'W':  case 'X':
						case 'Y':  case 'Z':  case '_':  case 'a':
						case 'b':  case 'c':  case 'd':  case 'e':
						case 'f':  case 'g':  case 'h':  case 'i':
						case 'j':  case 'k':  case 'l':  case 'm':
						case 'n':  case 'o':  case 'p':  case 'q':
						case 'r':  case 's':  case 't':  case 'u':
						case 'v':  case 'w':  case 'x':  case 'y':
						case 'z':
						{
							break;
						}
						default:
						{
							throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
						}
						 }
					}
					mPP_AND_EXPRESSION(false);
				}
				else
				{
					goto _loop794_breakloop;
				}
				
			}
_loop794_breakloop:			;
		}    // ( ... )*
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_AND_EXPRESSION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_AND_EXPRESSION;
		
		mPP_EQUALITY_EXPRESSION(false);
		{    // ( ... )*
			for (;;)
			{
				if ((tokenSet_23_.member(LA(1))) && (tokenSet_23_.member(LA(2))) && (tokenSet_24_.member(LA(3))) && (tokenSet_22_.member(LA(4))))
				{
					{
						switch ( LA(1) )
						{
						case '\t':  case '\u000b':  case '\u000c':  case ' ':
						{
							mPP_WHITESPACE(false);
							break;
						}
						case '&':
						{
							break;
						}
						default:
						{
							throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
						}
						 }
					}
					mLAND(false);
					{
						switch ( LA(1) )
						{
						case '\t':  case '\u000b':  case '\u000c':  case ' ':
						{
							mPP_WHITESPACE(false);
							break;
						}
						case '!':  case '$':  case '(':  case '@':
						case 'A':  case 'B':  case 'C':  case 'D':
						case 'E':  case 'F':  case 'G':  case 'H':
						case 'I':  case 'J':  case 'K':  case 'L':
						case 'M':  case 'N':  case 'O':  case 'P':
						case 'Q':  case 'R':  case 'S':  case 'T':
						case 'U':  case 'V':  case 'W':  case 'X':
						case 'Y':  case 'Z':  case '_':  case 'a':
						case 'b':  case 'c':  case 'd':  case 'e':
						case 'f':  case 'g':  case 'h':  case 'i':
						case 'j':  case 'k':  case 'l':  case 'm':
						case 'n':  case 'o':  case 'p':  case 'q':
						case 'r':  case 's':  case 't':  case 'u':
						case 'v':  case 'w':  case 'x':  case 'y':
						case 'z':
						{
							break;
						}
						default:
						{
							throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
						}
						 }
					}
					mPP_EQUALITY_EXPRESSION(false);
				}
				else
				{
					goto _loop799_breakloop;
				}
				
			}
_loop799_breakloop:			;
		}    // ( ... )*
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_EQUALITY_EXPRESSION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_EQUALITY_EXPRESSION;
		
		mPP_UNARY_EXPRESSION(false);
		{    // ( ... )*
			for (;;)
			{
				if ((tokenSet_25_.member(LA(1))) && (tokenSet_25_.member(LA(2))) && (tokenSet_26_.member(LA(3))) && (tokenSet_22_.member(LA(4))))
				{
					{
						switch ( LA(1) )
						{
						case '\t':  case '\u000b':  case '\u000c':  case ' ':
						{
							mPP_WHITESPACE(false);
							break;
						}
						case '!':  case '=':
						{
							break;
						}
						default:
						{
							throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
						}
						 }
					}
					mEQUALITY_OP(false);
					{
						switch ( LA(1) )
						{
						case '\t':  case '\u000b':  case '\u000c':  case ' ':
						{
							mPP_WHITESPACE(false);
							break;
						}
						case '!':  case '$':  case '(':  case '@':
						case 'A':  case 'B':  case 'C':  case 'D':
						case 'E':  case 'F':  case 'G':  case 'H':
						case 'I':  case 'J':  case 'K':  case 'L':
						case 'M':  case 'N':  case 'O':  case 'P':
						case 'Q':  case 'R':  case 'S':  case 'T':
						case 'U':  case 'V':  case 'W':  case 'X':
						case 'Y':  case 'Z':  case '_':  case 'a':
						case 'b':  case 'c':  case 'd':  case 'e':
						case 'f':  case 'g':  case 'h':  case 'i':
						case 'j':  case 'k':  case 'l':  case 'm':
						case 'n':  case 'o':  case 'p':  case 'q':
						case 'r':  case 's':  case 't':  case 'u':
						case 'v':  case 'w':  case 'x':  case 'y':
						case 'z':
						{
							break;
						}
						default:
						{
							throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
						}
						 }
					}
					mPP_UNARY_EXPRESSION(false);
				}
				else
				{
					goto _loop804_breakloop;
				}
				
			}
_loop804_breakloop:			;
		}    // ( ... )*
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_UNARY_EXPRESSION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_UNARY_EXPRESSION;
		
		switch ( LA(1) )
		{
		case '$':  case '(':  case '@':  case 'A':
		case 'B':  case 'C':  case 'D':  case 'E':
		case 'F':  case 'G':  case 'H':  case 'I':
		case 'J':  case 'K':  case 'L':  case 'M':
		case 'N':  case 'O':  case 'P':  case 'Q':
		case 'R':  case 'S':  case 'T':  case 'U':
		case 'V':  case 'W':  case 'X':  case 'Y':
		case 'Z':  case '_':  case 'a':  case 'b':
		case 'c':  case 'd':  case 'e':  case 'f':
		case 'g':  case 'h':  case 'i':  case 'j':
		case 'k':  case 'l':  case 'm':  case 'n':
		case 'o':  case 'p':  case 'q':  case 'r':
		case 's':  case 't':  case 'u':  case 'v':
		case 'w':  case 'x':  case 'y':  case 'z':
		{
			mPP_PRIMARY_EXPRESSION(false);
			break;
		}
		case '!':
		{
			mLNOT(false);
			{
				switch ( LA(1) )
				{
				case '\t':  case '\u000b':  case '\u000c':  case ' ':
				{
					mPP_WHITESPACE(false);
					break;
				}
				case '!':  case '$':  case '(':  case '@':
				case 'A':  case 'B':  case 'C':  case 'D':
				case 'E':  case 'F':  case 'G':  case 'H':
				case 'I':  case 'J':  case 'K':  case 'L':
				case 'M':  case 'N':  case 'O':  case 'P':
				case 'Q':  case 'R':  case 'S':  case 'T':
				case 'U':  case 'V':  case 'W':  case 'X':
				case 'Y':  case 'Z':  case '_':  case 'a':
				case 'b':  case 'c':  case 'd':  case 'e':
				case 'f':  case 'g':  case 'h':  case 'i':
				case 'j':  case 'k':  case 'l':  case 'm':
				case 'n':  case 'o':  case 'p':  case 'q':
				case 'r':  case 's':  case 't':  case 'u':
				case 'v':  case 'w':  case 'x':  case 'y':
				case 'z':
				{
					break;
				}
				default:
				{
					throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
				}
				 }
			}
			mPP_UNARY_EXPRESSION(false);
			break;
		}
		default:
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		 }
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mEQUALITY_OP(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = EQUALITY_OP;
		
		switch ( LA(1) )
		{
		case '=':
		{
			mEQUAL(false);
			break;
		}
		case '!':
		{
			mNOT_EQUAL(false);
			break;
		}
		default:
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		 }
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_PRIMARY_EXPRESSION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_PRIMARY_EXPRESSION;
		
		bool synPredMatched810 = false;
		if (((LA(1)=='t') && (LA(2)=='r') && (LA(3)=='u') && (LA(4)=='e')))
		{
			int _m810 = mark();
			synPredMatched810 = true;
			inputState.guessing++;
			try {
				{
					match("true");
				}
			}
			catch (RecognitionException)
			{
				synPredMatched810 = false;
			}
			rewind(_m810);
			inputState.guessing--;
		}
		if ( synPredMatched810 )
		{
			match("true");
		}
		else {
			bool synPredMatched812 = false;
			if (((LA(1)=='f') && (LA(2)=='a') && (LA(3)=='l') && (LA(4)=='s')))
			{
				int _m812 = mark();
				synPredMatched812 = true;
				inputState.guessing++;
				try {
					{
						match("false");
					}
				}
				catch (RecognitionException)
				{
					synPredMatched812 = false;
				}
				rewind(_m812);
				inputState.guessing--;
			}
			if ( synPredMatched812 )
			{
				match("false");
			}
			else if ((tokenSet_2_.member(LA(1))) && (tokenSet_27_.member(LA(2))) && (true) && (true)) {
				mCONDITIONAL_SYMBOL(false);
			}
			else if ((LA(1)=='(')) {
				mLPAREN(false);
				{
					switch ( LA(1) )
					{
					case '\t':  case '\u000b':  case '\u000c':  case ' ':
					{
						mPP_WHITESPACE(false);
						break;
					}
					case '!':  case '$':  case '(':  case '@':
					case 'A':  case 'B':  case 'C':  case 'D':
					case 'E':  case 'F':  case 'G':  case 'H':
					case 'I':  case 'J':  case 'K':  case 'L':
					case 'M':  case 'N':  case 'O':  case 'P':
					case 'Q':  case 'R':  case 'S':  case 'T':
					case 'U':  case 'V':  case 'W':  case 'X':
					case 'Y':  case 'Z':  case '_':  case 'a':
					case 'b':  case 'c':  case 'd':  case 'e':
					case 'f':  case 'g':  case 'h':  case 'i':
					case 'j':  case 'k':  case 'l':  case 'm':
					case 'n':  case 'o':  case 'p':  case 'q':
					case 'r':  case 's':  case 't':  case 'u':
					case 'v':  case 'w':  case 'x':  case 'y':
					case 'z':
					{
						break;
					}
					default:
					{
						throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
					}
					 }
				}
				mPP_EXPRESSION(false);
				{
					switch ( LA(1) )
					{
					case '\t':  case '\u000b':  case '\u000c':  case ' ':
					{
						mPP_WHITESPACE(false);
						break;
					}
					case ')':
					{
						break;
					}
					default:
					{
						throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
					}
					 }
				}
				mRPAREN(false);
			}
			else
			{
				throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
			}
			}
			if (_createToken && (null == _token) && (_ttype != Token.SKIP))
			{
				_token = makeToken(_ttype);
				_token.setText(text.ToString(_begin, text.Length-_begin));
			}
			returnToken_ = _token;
		}
		
	protected void mCONDITIONAL_SYMBOL(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = CONDITIONAL_SYMBOL;
		
		mIDENTIFIER(false);
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_IF_SECTION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_IF_SECTION;
		
		match("if");
		mPP_WHITESPACE(false);
		mPP_EXPRESSION(false);
		{
			switch ( LA(1) )
			{
			case '\t':  case '\u000b':  case '\u000c':  case ' ':
			{
				mPP_WHITESPACE(false);
				break;
			}
			case '\n':  case '\r':  case '/':  case '\u2028':
			case '\u2029':
			{
				break;
			}
			default:
			{
				throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
			}
			 }
		}
		mPP_NEW_LINE(false);
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_ELIF_SECTION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_ELIF_SECTION;
		
		match("elif");
		mPP_WHITESPACE(false);
		mPP_EXPRESSION(false);
		{
			switch ( LA(1) )
			{
			case '\t':  case '\u000b':  case '\u000c':  case ' ':
			{
				mPP_WHITESPACE(false);
				break;
			}
			case '\n':  case '\r':  case '/':  case '\u2028':
			case '\u2029':
			{
				break;
			}
			default:
			{
				throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
			}
			 }
		}
		mPP_NEW_LINE(false);
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_ELSE_SECTION(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_ELSE_SECTION;
		
		match("else");
		mPP_NEW_LINE(false);
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_ENDIF(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_ENDIF;
		
		match("endif");
		mPP_NEW_LINE(false);
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mLINE_INDICATOR(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = LINE_INDICATOR;
		
		switch ( LA(1) )
		{
		case '0':  case '1':  case '2':  case '3':
		case '4':  case '5':  case '6':  case '7':
		case '8':  case '9':
		{
			{ // ( ... )+
			int _cnt827=0;
			for (;;)
			{
				if (((LA(1) >= '0' && LA(1) <= '9')))
				{
					mDECIMAL_DIGIT(false);
				}
				else
				{
					if (_cnt827 >= 1) { goto _loop827_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
				}
				
				_cnt827++;
			}
_loop827_breakloop:			;
			}    // ( ... )+
			{
				switch ( LA(1) )
				{
				case '\t':  case '\u000b':  case '\u000c':  case ' ':
				{
					mPP_WHITESPACE(false);
					mFILE_NAME(false);
					break;
				}
				case '\n':  case '\r':  case '/':  case '\u2028':
				case '\u2029':
				{
					break;
				}
				default:
				{
					throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
				}
				 }
			}
			break;
		}
		case 'd':
		{
			match("default");
			break;
		}
		default:
		{
			throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());
		}
		 }
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mFILE_NAME(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = FILE_NAME;
		
		match("\"");
		{ // ( ... )+
		int _cnt831=0;
		for (;;)
		{
			if ((tokenSet_28_.member(LA(1))))
			{
				mFILE_NAME_CHARACTER(false);
			}
			else
			{
				if (_cnt831 >= 1) { goto _loop831_breakloop; } else { throw new NoViableAltForCharException((char)LA(1), getFilename(), getLine(), getColumn());; }
			}
			
			_cnt831++;
		}
_loop831_breakloop:		;
		}    // ( ... )+
		match("\"");
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mFILE_NAME_CHARACTER(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = FILE_NAME_CHARACTER;
		
		{
			match(tokenSet_28_);
		}
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	protected void mPP_MESSAGE(bool _createToken) //throws RecognitionException, CharStreamException, TokenStreamException
{
		int _ttype; Token _token=null; int _begin=text.Length;
		_ttype = PP_MESSAGE;
		
		{    // ( ... )*
			for (;;)
			{
				if ((tokenSet_0_.member(LA(1))))
				{
					mNOT_NEW_LINE(false);
				}
				else
				{
					goto _loop839_breakloop;
				}
				
			}
_loop839_breakloop:			;
		}    // ( ... )*
		mNEW_LINE(false);
		if (_createToken && (null == _token) && (_ttype != Token.SKIP))
		{
			_token = makeToken(_ttype);
			_token.setText(text.ToString(_begin, text.Length-_begin));
		}
		returnToken_ = _token;
	}
	
	
	private static long[] mk_tokenSet_0_()
	{
		long[] data = new long[258];
		data[0]=-4294960640L;
		data[1]=9223372032559808511L;
		for (int i = 2; i<=257; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_0_ = new BitSet(mk_tokenSet_0_());
	private static long[] mk_tokenSet_1_()
	{
		long[] data = new long[130];
		data[0]=4294973952L;
		data[1]=46181893548277760L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_1_ = new BitSet(mk_tokenSet_1_());
	private static long[] mk_tokenSet_2_()
	{
		long[] data = new long[130];
		data[0]=68719476736L;
		data[1]=576460745995190271L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_2_ = new BitSet(mk_tokenSet_2_());
	private static long[] mk_tokenSet_3_()
	{
		long[] data = new long[130];
		data[0]=288019269919178752L;
		for (int i = 1; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_3_ = new BitSet(mk_tokenSet_3_());
	private static long[] mk_tokenSet_4_()
	{
		long[] data = new long[258];
		data[0]=-4402341471744L;
		data[1]=9223372032559808511L;
		for (int i = 2; i<=257; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_4_ = new BitSet(mk_tokenSet_4_());
	private static long[] mk_tokenSet_5_()
	{
		long[] data = new long[258];
		data[0]=-4294951424L;
		data[1]=9223372032559808511L;
		for (int i = 2; i<=127; i++) { data[i]=0L; }
		data[128]=3298534883328L;
		for (int i = 129; i<=257; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_5_ = new BitSet(mk_tokenSet_5_());
	private static long[] mk_tokenSet_6_()
	{
		long[] data = new long[130];
		data[0]=287948969894477824L;
		data[1]=576460745995190270L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_6_ = new BitSet(mk_tokenSet_6_());
	private static long[] mk_tokenSet_7_()
	{
		long[] data = new long[130];
		data[0]=287948901175001088L;
		data[1]=541165879422L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_7_ = new BitSet(mk_tokenSet_7_());
	private static long[] mk_tokenSet_8_()
	{
		long[] data = new long[130];
		data[0]=0L;
		data[1]=9024791442886656L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_8_ = new BitSet(mk_tokenSet_8_());
	private static long[] mk_tokenSet_9_()
	{
		long[] data = new long[130];
		data[0]=0L;
		data[1]=35527969480784L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_9_ = new BitSet(mk_tokenSet_9_());
	private static long[] mk_tokenSet_10_()
	{
		long[] data = new long[130];
		data[0]=287948901175001088L;
		data[1]=137438953504L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_10_ = new BitSet(mk_tokenSet_10_());
	private static long[] mk_tokenSet_11_()
	{
		long[] data = new long[130];
		data[0]=287992881640112128L;
		data[1]=137438953504L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_11_ = new BitSet(mk_tokenSet_11_());
	private static long[] mk_tokenSet_12_()
	{
		long[] data = new long[130];
		data[0]=287948901175001088L;
		data[1]=35527969480784L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_12_ = new BitSet(mk_tokenSet_12_());
	private static long[] mk_tokenSet_13_()
	{
		long[] data = new long[130];
		data[0]=282041912393728L;
		data[1]=23714567704018944L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_13_ = new BitSet(mk_tokenSet_13_());
	private static long[] mk_tokenSet_14_()
	{
		long[] data = new long[258];
		data[0]=-554050774528L;
		data[1]=9223372032291373055L;
		for (int i = 2; i<=257; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_14_ = new BitSet(mk_tokenSet_14_());
	private static long[] mk_tokenSet_15_()
	{
		long[] data = new long[258];
		data[0]=-21474829824L;
		data[1]=9223372032559808511L;
		for (int i = 2; i<=257; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_15_ = new BitSet(mk_tokenSet_15_());
	private static long[] mk_tokenSet_16_()
	{
		long[] data = new long[258];
		data[0]=-21474829824L;
		data[1]=9223372032291373055L;
		for (int i = 2; i<=257; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_16_ = new BitSet(mk_tokenSet_16_());
	private static long[] mk_tokenSet_17_()
	{
		long[] data = new long[258];
		data[0]=-21474820608L;
		data[1]=9223372032291373055L;
		for (int i = 2; i<=127; i++) { data[i]=0L; }
		data[128]=3298534883328L;
		for (int i = 129; i<=257; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_17_ = new BitSet(mk_tokenSet_17_());
	private static long[] mk_tokenSet_18_()
	{
		long[] data = new long[130];
		data[0]=4294973952L;
		data[1]=2254067556417536L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_18_ = new BitSet(mk_tokenSet_18_());
	private static long[] mk_tokenSet_19_()
	{
		long[] data = new long[130];
		data[0]=1181116013056L;
		data[1]=576460745995190271L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_19_ = new BitSet(mk_tokenSet_19_());
	private static long[] mk_tokenSet_20_()
	{
		long[] data = new long[130];
		data[0]=4294973952L;
		data[1]=1152921504606846976L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_20_ = new BitSet(mk_tokenSet_20_());
	private static long[] mk_tokenSet_21_()
	{
		long[] data = new long[130];
		data[0]=1181116013056L;
		data[1]=1729382250602037247L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_21_ = new BitSet(mk_tokenSet_21_());
	private static long[] mk_tokenSet_22_()
	{
		long[] data = new long[258];
		data[0]=2593936302894235136L;
		data[1]=1729382250602037247L;
		for (int i = 2; i<=127; i++) { data[i]=0L; }
		data[128]=3298534883328L;
		for (int i = 129; i<=257; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_22_ = new BitSet(mk_tokenSet_22_());
	private static long[] mk_tokenSet_23_()
	{
		long[] data = new long[130];
		data[0]=279172880896L;
		for (int i = 1; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_23_ = new BitSet(mk_tokenSet_23_());
	private static long[] mk_tokenSet_24_()
	{
		long[] data = new long[130];
		data[0]=1455993920000L;
		data[1]=576460745995190271L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_24_ = new BitSet(mk_tokenSet_24_());
	private static long[] mk_tokenSet_25_()
	{
		long[] data = new long[130];
		data[0]=2305843022098602496L;
		for (int i = 1; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_25_ = new BitSet(mk_tokenSet_25_());
	private static long[] mk_tokenSet_26_()
	{
		long[] data = new long[130];
		data[0]=2305844190329707008L;
		data[1]=576460745995190271L;
		for (int i = 2; i<=129; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_26_ = new BitSet(mk_tokenSet_26_());
	private static long[] mk_tokenSet_27_()
	{
		long[] data = new long[258];
		data[0]=2593935203382607360L;
		data[1]=1729382250602037246L;
		for (int i = 2; i<=127; i++) { data[i]=0L; }
		data[128]=3298534883328L;
		for (int i = 129; i<=257; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_27_ = new BitSet(mk_tokenSet_27_());
	private static long[] mk_tokenSet_28_()
	{
		long[] data = new long[258];
		data[0]=-21474820608L;
		data[1]=9223372032559808511L;
		for (int i = 2; i<=127; i++) { data[i]=0L; }
		data[128]=3298534883328L;
		for (int i = 129; i<=257; i++) { data[i]=0L; }
		return data;
	}
	public static readonly BitSet tokenSet_28_ = new BitSet(mk_tokenSet_28_());
	
}
}
