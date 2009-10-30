using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using System.Xml.Linq;
using WpfHint;

namespace WinFormTestHint
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            var xml = "<test>aaaa\r\n<b>bbb</b>\r\nccc</test>";
            var root = XElement.Parse(xml);
            foreach (var element in root.Elements())
            {
                Console.WriteLine(element.Value);
            }
            foreach (var node in root.Nodes())
            {
                Console.WriteLine(node);
            }
            
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new Form1());
        }
    }
}
