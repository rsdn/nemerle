namespace Nemerle.Compiler {

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
			for (int i = 1; i < argv.Length; i++) {
//				System.Console.WriteLine("processing " + argv[i]);
				Parser p = new XParser();
				p.parse(new Lexer(argv[i]));
			}
		} catch (yyParser.yyException e) {
		} catch (ICE i) {
			System.Console.WriteLine("internal compiler error: " + i.msg);
		}
	}
}

}
