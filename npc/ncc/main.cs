namespace Nemerle.Compiler {

class CS_glue {
	public static string strip_last_part(string s)
	{
		int idx = s.LastIndexOf('.', s.Length - 2);
		return s.Substring(0, idx + 1);
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
	public static void Main()
	{
		string[] argv = System.Environment.GetCommandLineArgs();
		
		try {
			list ret = new list.Nil();
			for (int i = 1; i < argv.Length; i++) {
//				System.Console.WriteLine("processing " + argv[i]);
				Parser p = new XParser();
				ret = new list.Cons(p.parse(new Lexer(argv[i])), ret);
			}
			Passes.run(ret);
		} catch (yyParser.yyException e) {
		    Message.maybe_bailout();
			throw new ICE("got parsing exception, but no error seen");
		} catch (Recovery e) {
		    Message.maybe_bailout(true);
			throw new ICE("got Recovery exception");
		} catch (Nemerle.Core.Match_failure e) {
		    Message.maybe_bailout(true);
			throw new ICE("got Match_failure exception");
		} catch (ICE i) {
		    Message.maybe_bailout(true);
			System.Console.WriteLine("internal compiler error: " + i.msg + "\n" + i.StackTrace);
			System.Environment.Exit(1);
		}

		Message.maybe_bailout();
	}
}

}
