using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DefaultArgumentsTester;

namespace ConsoleApplication1
{
    class Program
    {
        static void Main(string[] args)
        {
            new Class1().F();
            new Class1().F(1);
            new Class1().F(2, "a");
            new Class1().F(3, "b", 3);

            new Class1().F2('q', 5, "d");
            new Class1().F2('q', 6, "e", 7.5);
        }
    }
}