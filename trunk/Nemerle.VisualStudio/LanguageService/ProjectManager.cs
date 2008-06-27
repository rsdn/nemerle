using System;

using Nemerle.Completion2;

using Nemerle.VisualStudio.Project;

namespace Nemerle.VisualStudio.LanguageService
{
	class ProjectManager : Nemerle.Completion2.ProjectManager
	{
		public ProjectManager(NemerleLanguageService languageService)
		{
			ErrorHelper.ThrowIsNull(languageService, "languageService");
			_languageService = languageService;
		}

		NemerleLanguageService _languageService;

		/// Get manager of code file.
		public override ISource GetSource(string filePath)
		{
			ProjectInfo info = ProjectInfo.FindProject(filePath);

			if (info == null)
				throw new ArgumentException("File '" + filePath + "' not in the project.",
					filePath);
			else
			{
				NemerleSource source = info.GetSource(filePath);

				if (source == null)
					return base.GetSource(filePath);
				else
					return new SourceTextManager(source);
			}
		}
	}
}
