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
 * NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

namespace Nemerle.Compiler {

class CS_glue {
	public static string strip_last_part(string s)
	{
		int idx = s.LastIndexOf('.', s.Length - 2);
		return s.Substring(0, idx + 1);
	}
	
	public static string get_ns(string s)
	{
		int idx = s.LastIndexOf('.');
		if (idx == -1)
			return "";
		else
			return s.Substring(0, idx);
	}

	public static string strip_ns(string s)
	{
		int idx = s.LastIndexOf('.');
		return s.Substring(idx + 1);
	}
	
	static public System.IO.StreamWriter output_file;

	public static void close_file ()
	{
		if (output_file != null)
		        output_file.Close ();
	}
	
	public static void write_string(string s)
	{
		if (output_file == null)
			output_file = new System.IO.StreamWriter("out.cs");
		output_file.Write(s);
	}
	
	public static string operator_name(string s)
	{
		if (s.StartsWith("%op"))
			return s.Substring(3, s.Length - 3);
		return "";
	}

	public static string mangle(string s)
	{
		char [] p = s.ToCharArray();
		for (int i = 0; i < p.Length; i++)
			if (!((p[i] >= 'a' && p[i] <= 'z') ||
			      (p[i] >= 'A' && p[i] <= 'Z') ||
				  (p[i] >= '0' && p[i] <= '9')))
				p[i] = '_';
		return new string(p);
	}

	public static string quote(string s)
	{
		char [] o = new char [s.Length * 2];
		int i, p = 0;

		for (i = 0; i < s.Length; i++)
			switch (s[i]) {
			case '\n':
				o[p++] = '\\';
				o[p++] = 'n';
				break;

			case '\t':
				o[p++] = '\\';
				o[p++] = 't';
				break;

			case '\"':
			case '\'':
			case '\\':
				o[p++] = '\\';
				o[p++] = s[i];
				break;

			default:
				o[p++] = s[i];
				break;
			}
			
		return new string(o, 0, p);
	}

	public static bool is_capitalized(string s)
	{
		int idx = s.LastIndexOf('.');
		return s[idx + 1] >= 'A' && s[idx + 1] <= 'Z';
	}

	public static string xmlescape(string s)
	{
		System.Text.StringBuilder buf = new System.Text.StringBuilder();

		for (int i = 0; i < s.Length; i++)
			switch (s[i]) {
			case '"':
				buf.Append("&quot;");
				break;
			case '<':
				buf.Append("&lt;");
				break;
			case '&':
				buf.Append("&amp;");
				break;
			default:
				buf.Append(s[i]);
				break;
			}
			
		return buf.ToString();
	}
}

class XParser : Parser {
	override public void yyerror(string message, string[] expected) {
		string s = message;
		if (expected != null && expected.Length > 0) {
			s += ", expecting";
			for (int n = 0; n < expected.Length; ++ n)
				s += " " + expected[n];
		} 
		Message.error(lex.get_location(), s);
	}

	public XParser() {
//		this.debug = new yydebug.yyDebugSimple();
	}
}

class MainClass {
	static void bomb(System.Exception e, string msg)
	{
		    Message.maybe_bailout(true);
			System.Console.WriteLine("internal compiler error: " + msg + "\n" + e.StackTrace);
			System.Environment.Exit(1);
	}

	public static void Main()
	{
		string[] argv = System.Environment.GetCommandLineArgs();
		
		try {
			list ret = new list.Nil();
			bool do_xml = false;
			for (int i = 1; i < argv.Length; i++) {
//				System.Console.WriteLine("processing " + argv[i]);
				Parser p = new XParser();
				if (argv[i] == "-x")
					do_xml = true;
				else
					ret = new list.Cons(p.parse(new Lexer(argv[i])), ret);
			}
			Passes.run(do_xml, ret);
			CS_glue.close_file ();
		} catch (yyParser.yyException e) {
		    Message.maybe_bailout();
			bomb(e, "got parsing exception, but no error seen");
                } catch (System.IO.FileNotFoundException e){
                        Message.error (e.Message);
		} catch (Recovery e) {
			bomb(e, "got Recovery exception");
		} catch (Nemerle.Core.Invalid_argument e) {
			bomb(e, "got Invalid_argument (" + e.msg + ")");
		} catch (Nemerle.Core.Match_failure e) {
			bomb(e, "got Match_failure exception");
		} catch (ICE i) {
			bomb(i, i.msg);
		}

		Message.maybe_bailout();
	}
}

}
