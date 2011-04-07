using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Nemerle.VisualStudio.Project;
using Nemerle.Compiler;
using Nemerle.Compiler.Utils;
using Microsoft.VisualStudio.Project;
using Nemerle.Compiler.Parsetree;
using System.CodeDom;
using System.Drawing;

namespace Nemerle.VisualStudio.Helpers
{
	class NemerleProjectSourcesButchEditHelper : IDisposable
	{
		readonly ProjectInfo _projectInfo;
		readonly Dictionary<int, NemerleSourceButchEditHelper> _fileIndexMap = new Dictionary<int, NemerleSourceButchEditHelper>();
		readonly string _description;
		readonly string _ident = "  ";

		public NemerleProjectSourcesButchEditHelper(ProjectInfo projectInfo, string description)
		{
			_projectInfo = projectInfo;
			_description = description;
		}

		#region IDisposable Members

		public void Dispose()
		{
			foreach (var item in _fileIndexMap.Values)
				item.Dispose();
		}

		#endregion

		NemerleSourceButchEditHelper GetHelper(int fileIndex)
		{
			NemerleSourceButchEditHelper helper;

			if (!_fileIndexMap.TryGetValue(fileIndex, out helper))
			{
				var source = _projectInfo.GetEditableSource(fileIndex, WindowFrameShowAction.DoNotShow);
				helper = new NemerleSourceButchEditHelper(source, null, true, _description);
				_fileIndexMap.Add(fileIndex, helper);
			}

			return helper;
		}

		public void Add(Location loc, string text)
		{
			GetHelper(loc.FileIndex).Add(loc, text);
		}

		public void ApplyEdits()
		{
			foreach (var item in _fileIndexMap.Values)
				item.ApplyEdits();
		}

		public static bool IsAllCharsIsSpaces(string text)
		{
			for (int i = 0; i < text.Length; i++)
			{
				var ch = text[i];

				if (ch != ' ' && ch != '\t')
					return false;
			}

			return true;
		}

		//TODO: Добавить методы обновляющие содержимое тела метода, добавляющие и удаляющие поля
		static string ReplaceNonSpaceWithSpace(string text)
		{
			if (IsAllCharsIsSpaces(text))
				return text;

			var ary = text.ToCharArray();

			for (int i = 0; i < ary.Length; i++)
			{
				var ch = ary[i];
				if (ch != ' ' && ch != '\t')
					ary[i] = ' ';
			}

			return new string(ary);
		}

		string CalcIndent(Location memLoc)
		{
			var helper = GetHelper(memLoc.FileIndex);
			var source = helper.Source;
			var firstLine = source.GetLine(memLoc.Line);
			var prefixSpaces = firstLine.Substring(0, memLoc.Column - 1);
			return ReplaceNonSpaceWithSpace(ReplaceNonSpaceWithSpace(prefixSpaces));
		}

		public void Add(Location parent, Location inner, string text)
		{
			var prefixSpaces = CalcIndent(parent);
			var bodyPrefix = prefixSpaces + _ident;
			const StringComparison op = StringComparison.Ordinal;
			var nl = Environment.NewLine;
			var text2 = text.StartsWith("\r", op) || text.StartsWith("\n", op)
									? text : nl + text;
			var text3 = text2.TrimEnd('\r', '\n', ' ', '\t');
			var text4 = text3.Replace("\n", "\n" + bodyPrefix);
			var text5 = text4 + nl + prefixSpaces;

			Add(inner, text5);
		}

		public void ReplaseMethodBody(ClassMember.Function function, string text)
		{
			var h = GetHelper(function.BodyCloseTokenLocation.FileIndex);
			var xx = h.Source.GetText();
			var closeBrecket = h.Source.GetText(function.BodyCloseTokenLocation.ToTextSpan());
			if (closeBrecket != "}")
			{
				System.Windows.Forms.MessageBox.Show("Internal Nemerle WinForms Designer Error!");
				throw new ArgumentException("closeBrecket != \"}\"");
			}
			Add(function.Location, function.BodyInnerLocation, text);
		}

		internal void AddField(TopDeclaration topDeclaration, CodeMemberField codeMemberField)
		{
			string text = FormCodeDomGenerator.ToString(codeMemberField);
			Add(topDeclaration.Location, topDeclaration.BodyCloseTokenLocation.FromStart(), text);
		}

		internal void AddMethod(
			TopDeclaration topDeclaration,
			CodeMemberMethod codeMemberMethod,
			CodeTypeDeclaration declaration)
		{
			string text = FormCodeDomGenerator.ToString(codeMemberMethod, declaration);
			var loc = topDeclaration.BodyCloseTokenLocation.FromStart();
			codeMemberMethod.UserData[typeof(Point)] = new Point(loc.Column + 3, loc.Line + _ident.Length);
			Add(topDeclaration.Location, loc, text);
		}

		internal void RemoveField(ClassMember.Field field)
		{
			var memLoc = field.Location;
			var helper = GetHelper(memLoc.FileIndex);
			var source = helper.Source;
			var firstLineText = source.GetLine(memLoc.Line);
			var prefix = firstLineText.Substring(0, memLoc.Column - 1);
			int endLine = memLoc.EndLine;
			int endCol = memLoc.EndColumn;
			var startCol = IsAllCharsIsSpaces(prefix) ? 1 : memLoc.Column;
			var endLineText = source.GetLine(memLoc.EndLine);
			var x = endCol - 1;
			var sufix = endLineText.Substring(x, endLineText.Length - x);
			var lineCount = source.LineCount;
			var startLine = memLoc.Line;

			// Коамилятор не всегда учитывает точку с запятой в местоположении выражения.
			// Компенсируем эту проблему удаляя точку с запятой из начала строки.
			if (sufix.StartsWith(";"))
			{
				sufix = sufix.Substring(1);
				endCol++;
			}

			// Если в конце строки нет других выражений, то продлеваем локешон до начала 
			// следующей строки и удаляем все пустые строки идущие за данной строкой.
			while (endLine + 1 < lineCount && IsAllCharsIsSpaces(sufix))
			{
				endLine++;
				endCol = 1;
				sufix = source.GetLine(endLine);
			}

			var delLoc = new Location(memLoc.FileIndex, startLine, startCol, endLine, endCol);

			helper.Add(delLoc, "");
		}
	}
}
