using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;

using EnvDTE;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Web.Application;
using Microsoft.VisualStudio.Shell.Design;
using System.ComponentModel.Design;
using System.Reflection;
using System.Globalization;
using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.LanguageService
{
	[Guid(NemerleConstants.NemerleWACodeBehindEventBindingGuidString)]
	// Код получен модификацией дизасемблированного кода Microsoft.VisualStudio.Web.Application.CodeBehindEventBinding
	public class NemerleWACodeBehindEventBinding : IVsCodeBehindEventBinding, IDisposable
	{
		#region IVsCodeBehindEventBinding Members

		private ServiceProvider _serviceProvider;
		private IVsHierarchy _hierarchy;
		protected bool _executing;

		int IVsCodeBehindEventBinding.Initialize(Microsoft.VisualStudio.OLE.Interop.IServiceProvider oleServiceProvider, IVsHierarchy hierarchy)
		{
			_serviceProvider = new ServiceProvider(oleServiceProvider);
			_hierarchy = hierarchy;
			_executing = false;
			return 0;
		}

		int IVsCodeBehindEventBinding.Close()
		{
			Dispose();
			return NativeMethods.S_OK;
		}

		int IVsCodeBehindEventBinding.CreateEventHandler(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, string eventHandlerName)
		{
			return WrapAndExecuteInternal(() => CreateEventHandler(document, codeBehind, codeBehindFile, className, objectTypeName, eventName, eventHandlerName));
		}

		int IVsCodeBehindEventBinding.CreateUniqueEventHandlerName(string document, string codeBehind, string codeBehindFile, string className, string objectName, string eventName, out string eventHandlerName)
		{
			string name = null;
			var result = WrapAndExecuteInternal(() => CreateUniqueEventHandlerName(document, codeBehind, codeBehindFile, className, objectName, eventName, out name));

			eventHandlerName = name;
			return result;
		}

		int IVsCodeBehindEventBinding.GetCompatibleEventHandlers(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, out string[] eventHandlerNames)
		{
			string[] names = null;
			var result = WrapAndExecuteInternal(() => GetCompatibleEventHandlers(document, codeBehind, codeBehindFile, className, objectTypeName, eventName, out names));

			eventHandlerNames = names;
			return result;
		}

		int IVsCodeBehindEventBinding.IsExistingEventHandler(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, string eventHandlerName, out bool isExistingEventHandler)
		{
			bool isExisting = false;
			var result = WrapAndExecuteInternal(() => IsExistingEventHandler(document, codeBehind, codeBehindFile, className, objectTypeName, eventName, eventHandlerName, out isExisting));

			isExistingEventHandler = isExisting;
			return result;
		}

		int IVsCodeBehindEventBinding.ShowEventHandler(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, string eventHandlerName)
		{
			return WrapAndExecuteInternal(() => ShowEventHandler(document, codeBehind, codeBehindFile, className, objectTypeName, eventName, eventHandlerName));
		}

		#region Static event binding (not supported)

		int IVsCodeBehindEventBinding.IsStaticEventBindingSupported(string document, string codeBehind, string codeBehindFile, string className, out bool isStaticEventBindingSupported)
		{
			isStaticEventBindingSupported = false;
			return NativeMethods.S_OK;
		}

		int IVsCodeBehindEventBinding.RemoveStaticEventBinding(string document, string codeBehind, string codeBehindFile, string className, string objectName, string eventName, string eventHandlerName)
		{
			return NativeMethods.E_NOTIMPL;
		}

		int IVsCodeBehindEventBinding.AddStaticEventBinding(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string objectName, string eventName, string eventHandlerName)
		{
			return NativeMethods.E_NOTIMPL;
		}

		int IVsCodeBehindEventBinding.GetStaticEventBinding(string document, string codeBehind, string codeBehindFile, string className, string objectName, string eventName, out string eventHandlerName)
		{
			eventHandlerName = null;
			return NativeMethods.E_NOTIMPL;
		}

		#endregion

		#endregion

		#region Implementation

		private delegate int ExecuteInternalDelegate();
		private int WrapAndExecuteInternal(ExecuteInternalDelegate execute)
		{
			if (this._executing)
				return NativeMethods.E_FAIL;

			int result = 0;
			try
			{
				this._executing = true;
				result = execute();
			}
			catch
			{
				result = NativeMethods.E_FAIL;
			}
			finally
			{
				this._executing = false;
			}

			return result;
		}

		protected int CreateEventHandler(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, string eventHandlerName)
		{
			var binder = GetBinder(document, codeBehind, codeBehindFile);
			if (binder == null)
				return NativeMethods.E_FAIL;

			CodeFunction codeFunction = null;
			UndoContext undoContext = binder.FileCodeModel.DTE.UndoContext;
			if (undoContext != null)
			{
				if (!undoContext.IsOpen)
				{
					undoContext.Open(eventHandlerName, false);
				}
				else
				{
					undoContext = null;
				}
			}
			try
			{
				codeFunction = binder.CreateEventHandler(codeBehindFile, className, objectTypeName, eventName, eventHandlerName);

				EditPoint point = codeFunction.StartPoint.CreateEditPoint();
				if (point != null)
				{
					point.SmartFormat(codeFunction.EndPoint);
				}

				return NativeMethods.S_OK;
			}
			finally
			{
				if (undoContext != null)
				{
					if (codeFunction != null)
					{
						undoContext.Close();

						var nemerleFileCodeModel = binder.FileCodeModel as Nemerle.VisualStudio.FileCodeModel.NemerleFileCodeModel;
						if (nemerleFileCodeModel != null)
						{
							nemerleFileCodeModel.FlushChanges();
						}
					}
					else
					{
						undoContext.SetAborted();
					}
				}
			}
		}

		protected int CreateUniqueEventHandlerName(string document, string codeBehind, string codeBehindFile, string className, string objectName, string eventName, out string eventHandlerName)
		{
			eventHandlerName = null;

			var binder = GetBinder(document, codeBehind, codeBehindFile);
			if (binder == null)
				return NativeMethods.E_FAIL;

			eventHandlerName = binder.CreateUniqueEventHandlerName(className, objectName, eventName);
			return NativeMethods.S_OK;
		}

		protected int GetCompatibleEventHandlers(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, out string[] eventHandlerNames)
		{
			eventHandlerNames = null;

			var binder = GetBinder(document, codeBehind, codeBehindFile);
			if (binder == null)
				return NativeMethods.E_FAIL;

			eventHandlerNames = binder.GetCompatibleEventHandlers(className, objectTypeName, eventName);

			return NativeMethods.S_OK;
		}

		protected int IsExistingEventHandler(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, string eventHandlerName, out bool isExistingEventHandler)
		{
			isExistingEventHandler = false;

			var binder = GetBinder(document, codeBehind, codeBehindFile);
			if (binder == null)
				return NativeMethods.E_FAIL;

			var function = binder.FindEventHandler(className, objectTypeName, eventName, eventHandlerName);
			isExistingEventHandler = function != null;

			return NativeMethods.S_OK;
		}

		protected int ShowEventHandler(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, string eventHandlerName)
		{
			var projectItem = GetProjectItem(document, codeBehind, codeBehindFile);

			var binder = GetBinder(projectItem);
			if (binder == null)
				return NativeMethods.E_FAIL;

			projectItem.Open(EnvDTE.Constants.vsViewKindCode);

			var function = binder.FindEventHandler(className, objectTypeName, eventName, eventHandlerName);

			if (function != null)
			{
				bool prevLineIsEmpty = true;
				EditPoint point = function.EndPoint.CreateEditPoint();
				point.LineUp(1);
				string lines = point.GetLines(point.Line, (int)(point.Line + 1));
				for (int i = 0; i < lines.Length; i++)
				{
					if (!char.IsWhiteSpace(lines[i]))
					{
						prevLineIsEmpty = false;
						break;
					}
				}

				Document document2 = projectItem.Document;
				if (document2 != null)
				{
					Window activeWindow = document2.ActiveWindow;
					if (activeWindow != null)
					{
						TextSelection selection = activeWindow.Selection as TextSelection;
						if (selection != null)
						{
							selection.MoveToPoint(function.EndPoint, false);
							if (prevLineIsEmpty)
							{
								selection.StartOfLine(vsStartOfLineOptions.vsStartOfLineOptionsFirstText, false);
								int virtualCharOffset = selection.AnchorPoint.VirtualCharOffset;
								selection.LineUp(false, 1);
								if (selection.AnchorPoint.VirtualCharOffset <= virtualCharOffset)
								{
									int indentSize = 4;
									TextDocument document3 = document2 as TextDocument;
									if (document3 != null)
									{
										indentSize = document3.IndentSize;
									}
									selection.MoveToLineAndOffset(selection.AnchorPoint.Line, (int)(virtualCharOffset + indentSize), false);
								}
							}
							else
							{
								selection.LineUp(false, 1);
								//selection.StartOfLine(vsStartOfLineOptions.vsStartOfLineOptionsFirstColumn, false);
								selection.EndOfLine(false);
							}
						}
					}
				}
			}

			return NativeMethods.S_OK;
		}

		#endregion

		#region Helper methods

		private ProjectItem GetProjectItem(string document, string codeBehind, string codeBehindFile)
		{
			// grabbed from Microsoft.VisualStudio.Web.Application.VsHierarchyItem.ctor(IVsHierarchy hier)
			IVsProject project = _hierarchy as IVsProject;
			if (project != null)
			{
				int pfFound = 0;
				uint vsitemid = uint.MaxValue;
				VSDOCUMENTPRIORITY[] pdwPriority = new VSDOCUMENTPRIORITY[1];

				if (project.IsDocumentInProject(codeBehindFile, out pfFound, pdwPriority, out vsitemid) == NativeMethods.S_OK &&
					(pfFound != 0) &&
					(vsitemid != uint.MaxValue))
				{
					var propid = __VSHPROPID.VSHPROPID_ExtObject;
					object pvar = null;

					_hierarchy.GetProperty(vsitemid, (int)propid, out pvar);

					return pvar as ProjectItem;
				}
			}

			return null;
		}

		private NemerleCodeBehindEventBinder GetBinder(string document, string codeBehind, string codeBehindFile)
		{
			var projectItem = GetProjectItem(document, codeBehind, codeBehindFile);

			return GetBinder(projectItem);
		}

		private NemerleCodeBehindEventBinder GetBinder(EnvDTE.ProjectItem projectItem)
		{
			if (projectItem == null)
				return null;

			if (projectItem.FileCodeModel == null)
				return null;

			ITypeResolutionService typeResolutionService = null;
			if (this._serviceProvider != null)
			{
				DynamicTypeService typeService = this._serviceProvider.GetService(typeof(DynamicTypeService)) as DynamicTypeService;
				if (typeService != null)
					typeResolutionService = typeService.GetTypeResolutionService(this._hierarchy);
			}

			return new NemerleCodeBehindEventBinder(projectItem.FileCodeModel, typeResolutionService);
		}

		#endregion

		#region IDisposable implementation

		~NemerleWACodeBehindEventBinding()
		{
			this.Dispose();
		}

		public void Dispose()
		{
			if (this._serviceProvider != null)
			{
				this._serviceProvider.Dispose();
				this._serviceProvider = null;
			}
			this._hierarchy = null;
			this._executing = false;
			GC.SuppressFinalize(this);
		}

		#endregion
	}
}
