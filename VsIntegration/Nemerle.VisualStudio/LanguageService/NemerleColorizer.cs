using System;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using Nemerle.Completion2;

namespace Nemerle.VisualStudio.LanguageService
{
	class NemerleColorizer : Colorizer, IDisposable
	{
		public NemerleColorizer(NemerleLanguageService ls, IVsTextLines buffer, NemerleScanner scaner) 
			: base(ls, buffer, scaner)
		{
		}

		public bool IsClosed { get; private set; }

		public override void CloseColorizer()
		{
			base.CloseColorizer();
			IsClosed = true;
		}

		void IDisposable.Dispose()
		{
			base.Dispose();
			IsClosed = true;
		}

		public void SetCurrentLine(int line)
		{
			NemerleScanner scanner = (NemerleScanner)Scanner;
			scanner._currentLine = line;
		}

		public override int ColorizeLine(int line, int length, IntPtr ptr, int state, uint[] attrs)
		{
			NemerleScanner scanner = (NemerleScanner)Scanner;
			scanner._currentLine = line;
			ScanLexer lex = scanner.GetLexer();

			if (lex == null)
				return 0;

			lex.SetFileName(scanner._source.GetFilePath());

			int ret;

			try
			{
				ret = base.ColorizeLine(line, length, ptr, state, attrs);

				if (attrs != null && scanner._colorizeEnd)
					attrs[length] = (uint)scanner._lastColor;
			}
			finally
			{
				scanner._currentLine = -1;
			}

			return ret;
		}
	}
}
