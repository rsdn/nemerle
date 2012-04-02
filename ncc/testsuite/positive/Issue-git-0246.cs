enum E
{
	A = 2
}

public class Program
{
	const int X = 42;

  static void Main()
	{
		E x = (E)X;

		switch (x)
		{
			case E.A:
				break;
			case (E)Program.X:
				System.Console.WriteLine("OK");
				break;
			default:
				break;
		}
	}
}

/*
BEGIN-OUTPUT
OK
END-OUTPUT
*/