/*
 * Copyright (c) 2003 The University of Wroclaw.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *    3. The name of the University may not be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 * NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

using System;
using System.IO;
using System.Text;
using System.Collections;

namespace Nemerle.Compiler {

class Lexer : yyParser.yyInput 
{
	FileStream file;
	string file_name;
	Object val;
	int line;
	int col;

	int last_line;
	int last_col;
	
	static Hashtable keywords;
	
	public Lexer(string fn)
	{
		file_name = fn;
		file = new FileStream(fn, FileMode.Open);
		line = 1;
		col = 1;
	}

	int putback = -1;

	int do_read()
	{
		int ch = file.ReadByte();
		if (ch == '\n') {
			line++;
			col = 1;
		} else
			col++;
		return ch;
	}
	
	int read()
	{
		if (putback == -1)
			return do_read();
		else {
			int c = putback;
			putback = -1;
			return c;
		}
	}

	int peek()
	{
		if (putback == -1)
			putback = do_read();
		return putback;
	}

	bool is_id_beg(int ch)
	{
		return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_';
	}

	bool is_digit(int ch)
	{
		return ch >= '0' && ch <= '9';
	}

	bool is_opchar(int ch)
	{
		switch (ch) {
		case '=':
		case '<':
		case '>':
		case '@':
		case '^':
		case '|':
		case '&':
		case '+':
		case '-':
		case '*':
		case '/':
		case '$':
		case '%':
		case '!':
		case '?':
		case '~':
		case '.':
		case ':':
		case '#':
			return true;
		default:
			return false;
		}
	}

	void skip_comment()
	{
		int ch;
		bool seen_star = false;

		for (;;) {
			ch = read();
			if (seen_star && ch == ')')
				break;
			if (ch == '*')
				seen_star = true;
			else
				seen_star = false;
		}
	}

	int get_op(int tok, int first_ch)
	{
		StringBuilder buf = new StringBuilder();
		
		buf.Append((char)first_ch);
		for (;;) {
			if (is_opchar(peek()))
				buf.Append((char)read());
			else
				break;
		}

		string s = buf.ToString();
		
		val = (object)s;

		switch (s) {
		case "*":
		case "=":
		case "?":
		case "|":
			return s[0];

		case "<-":
			return Token.LESS_MINUS;

		case "->":
			return Token.MINUS_MORE;

		case "=>":
			return Token.EQ_MORE;

		default:
			return tok;
		}
	}

	int get_number(int first_ch)
	{
		StringBuilder buf = new StringBuilder();
		
		buf.Append((char)first_ch);
		for (;;) {
			if (is_digit(peek()))
				buf.Append((char)read());
			else
				break;
		}

		val = (object)Int32.Parse(buf.ToString());

		return Token.NUMBER_LITERAL;
	}

	int get_id(int first_ch)
	{
		StringBuilder buf = new StringBuilder();
	
		if (first_ch != '\'')
			buf.Append((char)first_ch);
		for (;;) {
			if (is_id_beg(peek()) || is_digit(peek()) || peek() == '\'')
				buf.Append((char)read());
			else
				break;
		}
		
		string str = buf.ToString();

		val = (object)str;

		if (first_ch == '\'')
			return Token.TYVAR;

		init_keywords();

		if (keywords[str] != null)
			return (int)keywords[str];
		else
			return Token.ID;
	}

	int escape_value(int ch)
	{
		switch (ch) {
		case 'n': return '\n';
		case 't': return '\t';
		case 'r': return '\r';
		case 'b': return '\b';
		case '"': return '"';
		case '\'': return '\'';
		default:
			return -1;
		}
	}

	int error(string msg)
	{
		Message.error(get_current_location(), msg);
		return Token.ERROR;
	}

	int get_string()
	{
		StringBuilder buf = new StringBuilder();
		int ch = -1;
		
		while (ch != '"') {
			ch = read();
			switch (ch) {
			case '"':
				break;

			case '\\':
				ch = read();
				int esc = escape_value(ch);
				if (esc == -1)
					return error("invalid escape in string literal");
				buf.Append((char)esc);
				break;

			case '\n':
				return error("newline in string literal");

			case -1:
				return error("EOF in string literal");

			default:
				buf.Append((char)ch);
				break;
			}
		}

		val = (object)buf.ToString();
		
		return Token.STRING_LITERAL;
	}

	int get_quoted_op()
	{
		get_op(Token.ID, read());
		if (peek() != '`') {
			read();
			return error("invalid character in quoted op");
		}
		read();
		return Token.ID;
	}

	int get_double_quoted_id()
	{
		StringBuilder buf = new StringBuilder();
		int ch = -1;
		
		while (ch != '`') {
			ch = read();
			switch (ch) {
			case '`':
				if (read() != '`')
					return error("invalid character in double quoted id");
				break;

			case '\n':
				return error("newline in double quoted id");

			case -1:
				return error("EOF in double quoted id");
			}
		
			if (ch != '`')
				buf.Append((char)ch);
		}

		val = (object)buf.ToString();
		
		return Token.ID;
	}
	
	int get_quoted_id()
	{
		StringBuilder buf = new StringBuilder();
		int ch = -1;
		
		while (ch != '`') {
			ch = read();
			switch (ch) {
			case '`':
				break;

			case '\n':
				return error("newline in infix id");

			case -1:
				return error("EOF in infix id");
			}
		
			if (ch != '`')
				buf.Append((char)ch);
		}

		val = (object)buf.ToString();
		
		return Token.OP4;
	}

	int get_token()
	{
		last_line = line;
		last_col = col;

		int ch = read();
		
		switch (ch) {
		case -1:
			return -1;
			
		case ' ':
		case '\t':
		case '\r':
		case '\n':
			return get_token();

		case '"':
			return get_string();

		case '`':
			if (peek() == '`') {
				read();
				return get_double_quoted_id();
			} else if (is_opchar(peek())) {
				return get_quoted_op();
			} else
				return get_quoted_id();

		case '\'':
			if (is_id_beg(peek()))
				return get_id('\'');
			else
				return error("invalid character in type variable");

		case '(':
			if (peek() == '*') {
				read();
				skip_comment();
				return get_token();
			} else
				return ch;
			
		case ':':
			if (peek() == '>') {
				read();
				return Token.COLON_MORE;
			} else
				return ch;

		case '{':
		case '}':
		case '[':
		case ']':
		case ',':
		case ';':
		case '.':
		case ')':
		case '#':
		case '\\':
			return ch;

		case '*':
			if (peek() == '*')
				return get_op(Token.OP1, ch);
			else
				return get_op(Token.OP2, ch);

		case '/':
			if (peek() == '/') {
				do {
					ch = read();
				} while (ch != '\n' && ch != -1);
				return get_token();
			} else
				return get_op(Token.OP2, ch);
				
		case '%':
			return get_op(Token.OP2, ch);

		case '+':
		case '-':
			return get_op(Token.OP3, ch);

		case '@':
		case '^':
		case '$':
		case '~':
		case '?':
			return get_op(Token.OP4, ch);

		case '=':
		case '<':
		case '>':
		case '!':
			return get_op(Token.OP5, ch);

		case '&':
			return get_op(Token.OP6, ch);

		case '|':
			return get_op(Token.OP7, ch);

		default:
			if (is_digit(ch))
				return get_number(ch);
				
			if (is_id_beg(ch))
				return get_id(ch);

			return error("invalid character");
		}
	}

	public Location get_location()
	{
		return new Location(file_name, last_line, last_col);
	}

	public Location get_current_location()
	{
		return new Location(file_name, line, col);
	}

	// yyParser.yyInput
	int cur_tok = -1;
	
	public bool advance()
	{
		cur_tok = get_token();
		return cur_tok != -1;
	}

	public int token()
	{
		if (cur_tok == -1)
			throw new ICE("attempt to read beyond end of file");
		return cur_tok;
	}

	public object value()
	{
		return val;
	}
	
	static void init_keywords()
	{
		if (keywords != null)
			return;
			
		keywords = new Hashtable();
		keywords["_"] = (int)'_';
		keywords["abstract"] = Token.KW_ABSTRACT;
		keywords["const"] = Token.KW_CONST;
		keywords["extern"] = Token.KW_EXTERN;
		keywords["internal"] = Token.KW_INTERNAL;
		keywords["new"] = Token.KW_NEW;
		keywords["private"] = Token.KW_PRIVATE;
		keywords["protected"] = Token.KW_PROTECTED;
		keywords["sealed"] = Token.KW_SEALED;
		keywords["volatile"] = Token.KW_VOLATILE;
		keywords["class"] = Token.KW_CLASS;
		keywords["enum"] = Token.KW_ENUM;
		keywords["extends"] = Token.KW_EXTENDS;
		keywords["finally"] = Token.KW_FINALLY;
		keywords["in"] = Token.KW_IN;
		keywords["method"] = Token.KW_METHOD;
		keywords["null"] = Token.KW_NULL;
		keywords["out"] = Token.KW_OUT;
		keywords["public"] = Token.KW_PUBLIC;
		keywords["raise"] = Token.KW_RAISE;
		keywords["ref"] = Token.KW_REF;
		keywords["struct"] = Token.KW_STRUCT;
		keywords["this"] = Token.KW_THIS;
		keywords["variant"] = Token.KW_VARIANT;
		keywords["interface"] = Token.KW_INTERFACE;
		keywords["implements"] = Token.KW_IMPLEMENTS;
		keywords["namespace"] = Token.KW_NAMESPACE;
		keywords["where"] = Token.KW_WHERE;
		keywords["field"] = Token.KW_FIELD;
		keywords["value"] = Token.KW_VALUE;
		keywords["type"] = Token.KW_TYPE;
		keywords["let"] = Token.KW_LET;
		keywords["in"] = Token.KW_IN;
		keywords["fun"] = Token.KW_FUN;
		keywords["and"] = Token.KW_AND;
		keywords["tymatch"] = Token.KW_TYMATCH;
		keywords["with"] = Token.KW_WITH;
		keywords["try"] = Token.KW_TRY;
		keywords["open"] = Token.KW_OPEN;
		keywords["void"] = Token.KW_VOID;
		keywords["base"] = Token.KW_BASE;
		keywords["if"] = Token.KW_IF;
		keywords["then"] = Token.KW_THEN;
		keywords["else"] = Token.KW_ELSE;
		keywords["variant"] = Token.KW_VARIANT;
		keywords["letfun"] = Token.KW_LETFUN;
		keywords["as"] = Token.KW_AS;
		keywords["record"] = Token.KW_RECORD;
		keywords["match"] = Token.KW_MATCH;
	}
}

}
