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

using Nemerle.Compiler;
using Nemerle.Completion2;
using Nemerle.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using EnvDTE;

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
		private ProjectItem _projectItem = null;
		private string _filePath = null;
		private ProjectInfo _projectInfo = null;
		private IVsHierarchy _hierarchy = null;
		private IVsTextBufferCoordinator bufferCoordinator;
		[SuppressMessage("Microsoft.Performance", "CA1823:AvoidUnusedPrivateFields")]
		private NemerleIntellisenseProvider intellisenseProject;
		private IVsContainedLanguageHost languageHost;
		private NemerleLanguageService language;
		private uint itemId;
		private ITypeResolutionService _typeResolutionService;
		private DocumentEvents _documentEvents;
		private _dispDocumentEvents_DocumentClosingEventHandler _documentClosingEventHandler;
		private CodeWindowManager _windowManager = null;

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

			var project = _projectItem.ContainingProject as NemerleOAProject;
			if(project != null)
			{
				_projectInfo = ((NemerleProjectNode)project.Project).ProjectInfo;
			}
	
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

			_documentClosingEventHandler = new _dispDocumentEvents_DocumentClosingEventHandler(OnDocumentClosing);
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

			IVsTextLines buffer;
			ErrorHandler.ThrowOnFailure(bufferCoordinator.GetSecondaryBuffer(out buffer));

			var secondaryFilePath = FilePathUtilities.GetFilePath(buffer);

			if (secondaryFilePath == null)
				secondaryFilePath = NemerleSource.GetStubFileForSecondaryBuffer(buffer);

			var secondaryFileIndex = Location.GetFileIndex(secondaryFilePath);
			var primaryFileindex = Location.GetFileIndex(_filePath);

			bool doOutlining = LanguageService.Preferences.AutoOutlining;
			LanguageService.Preferences.AutoOutlining = false;

			if (_projectInfo != null && LanguageService.GetSource(buffer) == null)
			{
				// создаем и регистрируем в проекте временный source, чтобы не сломалась логика  
				// конструктора NemerleSource (см вызов LanguageService.AddEditableSource) 
				//_projectInfo.ReplaseOrAddSource(new FileNemerleSource(secondaryFileIndex));
				_projectInfo.AddEditableSource((NemerleSource)LanguageService.CreateSource(buffer));
			}
			
			NemerleSource source = LanguageService.GetOrCreateSource(buffer) as NemerleSource;
			source.SetBufferCoordinator(bufferCoordinator);

			if (_projectInfo != null)
			{
				_projectInfo.Engine.RequestOnBuildTypesTree();
			}
	
			LanguageService.Preferences.AutoOutlining = doOutlining;

			_windowManager = LanguageService.CreateCodeWindowManager(null, source);

			language.AddCodeWindowManager(_windowManager);

			// увеличиваем внутренний счетчик openCount, для того чтобы впоследствии корректно отработала логика закрытия соурса
			source.Open();

			TextViewWrapper view = new TextViewWrapper(languageHost, pISenseHost, bufferCoordinator, pNextCmdTarget, source);
			_windowManager.OnNewView(view);

			pTextViewFilter = view.InstalledFilter;
			NemerleViewFilter nemerleFilter = pTextViewFilter as NemerleViewFilter;
			if (null != nemerleFilter)
				nemerleFilter.BufferCoordinator = this.bufferCoordinator;

			// сохраним значение DocumentEvents в переменной класса, чтобы исключить преждевременное уничтожение 
			// объекта и автоматическоей отписывание от событий. 
			// 
			// Источник решения:
			// PRB: Visual Studio .NET events being disconnected from add-in (http://www.mztools.com/articles/2005/mz2005012.aspx) 
			_documentEvents = _projectItem.DTE.Events.get_DocumentEvents(_projectItem.Document);

			_documentEvents.DocumentClosing += _documentClosingEventHandler;

			return VSConstants.S_OK;
		}

		// После закрытия aspx файла исключим автосгенерированный исходник из списка компиляции проекта
		void OnDocumentClosing(EnvDTE.Document document)
		{
			if (Location.GetFileIndex(document.FullName) == Location.GetFileIndex(_filePath))
			{
				try
				{
					if (_windowManager != null)
						// RemoveAdornments вызывает уничтожение (close и dispose) view filter и source, связанных с _windowManager
						_windowManager.RemoveAdornments();
				}
				finally
				{
					_windowManager = null;
					_documentEvents.DocumentClosing -= _documentClosingEventHandler;
				}
			}
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
			ppEnum = new CodeBlocksEnumerator(buffer); 
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

