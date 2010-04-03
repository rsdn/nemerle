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
using System.ComponentModel.Design;
using Microsoft.VisualStudio.Shell.Design;

namespace Nemerle.VisualStudio.LanguageService
{
	/// <summary>
	/// The contained language implementation.
	/// This object is the one responsible to provide the colorizer for a specific file and the
	/// command filter for a specific text view.
	/// It also implements IVsContainedCode so that the buffer coordinator can map text spans
	/// between the primary and secondary buffer.
	/// </summary>
	public partial class NemerleContainedLanguage : IVsContainedLanguage, IVsContainedCode
	{
		private EnvDTE.ProjectItem _projectItem = null;
		private string _filePath = null;
		private IVsHierarchy _hierarchy = null;
		private IVsTextBufferCoordinator bufferCoordinator;
		[SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
		private NemerleIntellisenseProvider intellisenseProject;
		private IVsContainedLanguageHost languageHost;
		private NemerleLanguageService language;
		private uint itemId;
		private ITypeResolutionService _typeResolutionService;

		public NemerleContainedLanguage(IVsTextBufferCoordinator bufferCoordinator, NemerleIntellisenseProvider intellisenseProject, uint itemId, IVsHierarchy pHierarchy)
		{
			if (null == bufferCoordinator)
			{
				throw new ArgumentNullException("bufferCoordinator");
			}
			if (null == intellisenseProject)
			{
				throw new ArgumentNullException("intellisenseProject");
			}
			_hierarchy = pHierarchy;

			object projectItem = null;
			pHierarchy.GetProperty(itemId, (int)__VSHPROPID.VSHPROPID_ExtObject, out projectItem);

			_projectItem = projectItem as EnvDTE.ProjectItem;

			EnvDTE.Property prop = _projectItem.Properties.Item("FullPath");
			if (prop != null)
				_filePath = prop.Value as string;

			_typeResolutionService = null;
			DynamicTypeService typeService = LanguageService.GetService(typeof(DynamicTypeService)) as DynamicTypeService;
			if (typeService != null)
				_typeResolutionService = typeService.GetTypeResolutionService(this._hierarchy);
	
			this.bufferCoordinator = bufferCoordinator;
			this.intellisenseProject = intellisenseProject;
			this.itemId = itemId;
			// Make sure that the secondary buffer uses the IronPython language service.
			IVsTextLines buffer;
			ErrorHandler.ThrowOnFailure(bufferCoordinator.GetSecondaryBuffer(out buffer));
			Guid languageGuid;
			this.GetLanguageServiceID(out languageGuid);
			ErrorHandler.ThrowOnFailure(buffer.SetLanguageServiceID(ref languageGuid));
		}

		public int GetColorizer(out IVsColorizer ppColorizer)
		{
			ppColorizer = null;
			if (null == LanguageService)
			{
				// We should always be able to get the language service.
				return VSConstants.E_UNEXPECTED;
			}
			IVsTextLines buffer;
			ErrorHandler.ThrowOnFailure(bufferCoordinator.GetSecondaryBuffer(out buffer));
			return LanguageService.GetColorizer(buffer, out ppColorizer);
		}

		public int GetLanguageServiceID(out Guid pguidLangService)
		{
			pguidLangService = new Guid(NemerleConstants.LanguageServiceGuidString);
			return VSConstants.S_OK;
		}

		public int GetTextViewFilter(IVsIntellisenseHost pISenseHost, IOleCommandTarget pNextCmdTarget, out IVsTextViewFilter pTextViewFilter)
		{
			pTextViewFilter = null;
			return VSConstants.E_NOTIMPL;

			//IVsTextLines buffer;
			//ErrorHandler.ThrowOnFailure(bufferCoordinator.GetSecondaryBuffer(out buffer));

			//bool doOutlining = LanguageService.Preferences.AutoOutlining;
			//LanguageService.Preferences.AutoOutlining = false;

			//NemerleSource source = LanguageService.CreateSource(buffer) as NemerleSource;
			//LanguageService.Preferences.AutoOutlining = doOutlining;

			//CodeWindowManager windowMgr = LanguageService.CreateCodeWindowManager(null, source);

			//language.AddCodeWindowManager(windowMgr);

			//TextViewWrapper view = new TextViewWrapper(languageHost, pISenseHost, bufferCoordinator, pNextCmdTarget);
			//windowMgr.OnNewView(view);

			//// language.AddSpecialSource(source, view); // from python

			//pTextViewFilter = view.InstalledFilter;
			//NemerleViewFilter nemerleFilter = pTextViewFilter as NemerleViewFilter;
			//if (null != nemerleFilter)
			//    nemerleFilter.BufferCoordinator = this.bufferCoordinator;

			//return VSConstants.S_OK;
		}

		public int Refresh(uint dwRefreshMode)
		{
			return VSConstants.S_OK;
		}

		public int SetBufferCoordinator(IVsTextBufferCoordinator pBC)
		{
			bufferCoordinator = pBC;
			return VSConstants.S_OK;
		}

		public int SetHost(IVsContainedLanguageHost pHost)
		{
			languageHost = pHost;
			return VSConstants.S_OK;
		}

		public int WaitForReadyState()
		{
			// Do Nothing
			return VSConstants.S_OK;
		}

		public int EnumOriginalCodeBlocks(out IVsEnumCodeBlocks ppEnum)
		{
			IVsTextLines buffer;
			ErrorHandler.ThrowOnFailure(bufferCoordinator.GetSecondaryBuffer(out buffer));
			ppEnum = null; //new CodeBlocksEnumerator(buffer); 
			return VSConstants.S_OK;
		}

		public int HostSpansUpdated()
		{
			return VSConstants.S_OK;
		}


		private NemerleLanguageService LanguageService
		{
			get
			{
				if (null == language)
				{
					// Try to get the language service using the global service provider.
					language = NemerlePackage.GetGlobalService(typeof(NemerleLanguageService)) as NemerleLanguageService;
				}
				return language;
			}
		}
	}
}

