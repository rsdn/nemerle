using System;

using EnvDTE;

namespace Nemerle.VisualStudio.FileCodeModel
{
	internal class NullTextPoint : TextPoint
	{
		public DTE DTE { get { return null; } }
		public TextDocument Parent { get { return null; } }
		public int Line { get { return 1; } }
		public int LineCharOffset { get { return 0; } }
		public int AbsoluteCharOffset { get { return 0; } }
		public int DisplayColumn { get { return 0; } }
		public bool AtEndOfDocument { get { return false; } }
		public bool AtStartOfDocument { get { return true; } }
		public bool AtEndOfLine { get { return false; } }
		public bool AtStartOfLine { get { return true; } }
		public int LineLength { get { return 0; } }

		bool TextPoint.EqualTo(TextPoint Point)
		{
			return Equals(Point);
		}

		public bool LessThan(TextPoint Point)
		{
			return true;
		}

		public bool GreaterThan(TextPoint Point)
		{
			return true;
		}

		public bool TryToShow(vsPaneShowHow How, object PointOrCount)
		{
			return false;
		}

		public EditPoint CreateEditPoint()
		{
			return null;
		}

		public CodeElement get_CodeElement(vsCMElement Scope)
		{
			return null;
		}
	}
}