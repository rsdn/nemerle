using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;

using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Shell.Interop;

using VSConstants = Microsoft.VisualStudio.VSConstants;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;

namespace Nemerle.VisualStudio.LanguageService
{
	/// <summary>
	/// Factory class to create an instance of the NemerleContainedLanguage class.
	/// </summary>
	public class NemerleContainedLanguageFactory : IVsContainedLanguageFactory
	{
		private Dictionary<ModuleId, NemerleContainedLanguage> languages;
		private NemerleIntellisenseProvider intellisenseProject;
		internal NemerleContainedLanguageFactory(NemerleIntellisenseProvider intellisenseProject)
		{
			languages = new Dictionary<ModuleId, NemerleContainedLanguage>();
			this.intellisenseProject = intellisenseProject;
		}

		public int GetLanguage(IVsHierarchy pHierarchy, uint itemid, IVsTextBufferCoordinator pBufferCoordinator, out IVsContainedLanguage ppLanguage)
		{
			ModuleId id = new ModuleId(pHierarchy, itemid);
			NemerleContainedLanguage lang;
			if (!languages.TryGetValue(id, out lang))
			{
				lang = new NemerleContainedLanguage(pBufferCoordinator, intellisenseProject, itemid, pHierarchy);
				languages.Add(id, lang);
			}
			ppLanguage = lang;
			return VSConstants.S_OK;
		}
	}
}
