using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Nemerle.Completion2;
using Nemerle.VisualStudio.LanguageService;
using Microsoft.VisualStudio.Shell.Interop;
using System.IO;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// Это одна строка в окошке Find All References. От LibraryNode
	/// отличается тем что содержит ссылку на GotoInfo, форматирует своё описание
	/// в стиле соответствующем стилю C#, и поддерживает переход на Location найденной ссылки
	/// </summary>
	/// <remarks>
	/// Пока тип найденной записи не анализируется, из-за этого нет нормальной картинки
	/// </remarks>
	class GotoInfoLibraryNode : LibraryNode
	{
		private readonly NemerleLanguageService _langSvc;
		private readonly GotoInfo _gotoInfo;
		private readonly string _caption;
		private readonly string Text;

		public GotoInfoLibraryNode(NemerleLanguageService langSvc, GotoInfo gotoInfo, string caption)
			: base(gotoInfo.UsageTypeToString())
		{
			_langSvc = langSvc;
			_gotoInfo = gotoInfo;
			_caption = caption;
			CanGoToSource = true;
			
			var project = ProjectInfo.FindProject(gotoInfo.FilePath);
			
			if(project != null)
				Text = _gotoInfo.GetLineOfCode(project.Engine).Trim().Replace("\t", "  ");
			else//я не представляю как такое может случиться, но если случится
				Text = "<not loaded>";//то нужно скоприровать функционал ручной загрузки файла и поиска строки из конструктора GotoUsageForm
		}

		protected override void GotoSource(Microsoft.VisualStudio.Shell.Interop.VSOBJGOTOSRCTYPE gotoType)
		{
			_langSvc.GotoLocation(_gotoInfo.Location, _caption, _caption != null);
		}

		protected override uint CategoryField(LIB_CATEGORY category)
		{
			return (uint)LibraryNodeType.None;
		}

		protected override string GetTextWithOwnership(VSTREETEXTOPTIONS tto)
		{
			if(tto == VSTREETEXTOPTIONS.TTO_DEFAULT)
				return string.Format("{0} {1} - ({2}, {3}) : {4}", Name, _gotoInfo.FilePath, _gotoInfo.Line, _gotoInfo.Column, Text);

			return null;
		}
	}
}
