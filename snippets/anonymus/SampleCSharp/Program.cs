using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Nemerle.Extensions;

namespace Sample {

	class Program {
		static void Main(string[] args) {
			// Every Nemerle anonymous class implements IAnonymous interface.
			

			var o = Foo.GetObject("blablabla", 10);

			Console.WriteLine("Object:");
			Console.WriteLine(o);
			Console.WriteLine();


			Console.WriteLine("Type name:");
			Console.WriteLine(o.GetType());
			Console.WriteLine();


			Console.WriteLine("Indexing:");
			foreach (var field in o.Fields) {
				Console.WriteLine("{0} = {1}", field, o[field]);
			}
			Console.WriteLine();


			var test = "X";
			Console.WriteLine("Is there field '{0}'?", test);
			Console.WriteLine(Array.Exists(o.Fields, f => f == test) ? "yes" : "no");
			Console.WriteLine();


			Console.WriteLine("Value of field '{0}':", test);
			Console.WriteLine(o[test] ?? "NULL");
			Console.WriteLine();


			Console.WriteLine("Press any key...");
			Console.ReadKey(true);
		}
	}

}
