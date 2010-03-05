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

		protected virtual ProjectItem GetProjectItem(string document, string codeBehind, string codeBehindFile)
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

		protected virtual int ShowEventHandler(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, string eventHandlerName)
		{
			ProjectItem projectItem = GetProjectItem(document, codeBehind, codeBehindFile);
			if (projectItem == null)
				return NativeMethods.E_FAIL;

			projectItem.Open(EnvDTE.Constants.vsViewKindCode);

			EnvDTE.FileCodeModel fileCodeModel = projectItem.FileCodeModel;
			if (fileCodeModel != null)
			{
				CodeClass codeClass = FindClass(fileCodeModel.CodeElements, className);
				if (codeClass != null)
				{
					CodeFunction function = FindEventHandler(codeClass, objectTypeName, eventName, eventHandlerName);
					if (function != null)
					{
						bool flag = true;
						EditPoint point = function.EndPoint.CreateEditPoint();
						point.LineUp(1);
						string lines = point.GetLines(point.Line, (int)(point.Line + 1));
						for (int i = 0; i < lines.Length; i++)
						{
							if (!char.IsWhiteSpace(lines[i]))
							{
								flag = false;
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
									if (flag)
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
				}
			}

			return NativeMethods.S_OK;
		}

		protected virtual int IsExistingEventHandler(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, string eventHandlerName, out bool isExistingEventHandler)
		{
			isExistingEventHandler = false;

			ProjectItem projectItem = GetProjectItem(document, codeBehind, codeBehindFile);
			if (projectItem != null)
			{
				EnvDTE.FileCodeModel fileCodeModel = projectItem.FileCodeModel;
				if (fileCodeModel != null)
				{
					CodeClass class2 = this.FindClass(fileCodeModel.CodeElements, className);
					if (class2 != null)
					{
						if (this.FindEventHandler(class2, objectTypeName, eventName, eventHandlerName) != null)
							isExistingEventHandler = true;
					}
				}
			}

			return NativeMethods.S_OK;
		}

		protected virtual CodeFunction FindEventHandler(CodeClass codeClass, string objectTypeName, string eventName, string eventHandlerName)
		{
			if (codeClass != null)
			{
				string eventHandlerSignature = GetEventHandlerSignature(objectTypeName, eventName);

				foreach (CodeElement element in codeClass.Members)
				{
					CodeFunction codeFunction = element as CodeFunction;
					if (codeFunction != null)
					{
						string name = codeFunction.Name;
						if (!string.IsNullOrEmpty(name) && string.Compare(name, eventHandlerName, StringComparison.InvariantCulture) == 0)
						{
							string functionSignature = GetFunctionSignature(codeFunction);

							if (string.Compare(eventHandlerSignature, functionSignature, StringComparison.InvariantCulture) == 0)
								return codeFunction;
						}
					}
				}
			}

			return null;
		}

		protected virtual string GetFunctionSignature(CodeFunction codeFunction)
		{
			string signature = null;

			if (codeFunction != null)
			{
				signature = codeFunction.Type.AsFullName;
				if (string.IsNullOrEmpty(signature))
				{
					signature = "System.Void";
				}
	
				foreach (CodeElement element in codeFunction.Parameters)
				{
					CodeParameter parameter = element as CodeParameter;
					if (parameter != null)
					{
						signature = signature + "," + parameter.Type.AsFullName;
					}
				}
			}

			return signature;
		}

		protected virtual string GetEventHandlerSignature(string objectTypeName, string eventName)
		{
			string returnTypeName = null;
			string[] paramTypeNames = null;
			string[] paramNames = null;
			string signature = null;

			this.GetEventHandlerSignature(objectTypeName, eventName, out returnTypeName, out paramTypeNames, out paramNames);

			signature = returnTypeName;

			if (paramTypeNames != null)
			{
				for (int i = 0; i < paramTypeNames.Length; i++)
				{
					signature = signature + "," + paramTypeNames[i];
				}
			}

			return signature;
		}

		protected virtual void GetEventHandlerSignature(string objectTypeName, string eventName, out string returnTypeName, out string[] paramTypeNames, out string[] paramNames)
		{
			returnTypeName = null;
			paramTypeNames = null;
			paramNames = null;

			Type type = this.TypeResolutionService.GetType(objectTypeName);
			if (type != null)
			{
				EventInfo info = type.GetEvent(eventName);
				if (info != null)
				{
					Type eventHandlerType = info.EventHandlerType;
					if (eventHandlerType != null)
					{
						MethodInfo method = eventHandlerType.GetMethod("Invoke");
						if (method != null)
						{
							var list = new List<string>();
							var list2 = new List<string>();
							
							foreach (ParameterInfo info3 in method.GetParameters())
							{
								list.Add(this.VsFormatType(info3.ParameterType.FullName));
								list2.Add(info3.Name);
							}
							
							paramTypeNames = list.ToArray();
							paramNames = list2.ToArray();
							
							returnTypeName = this.VsFormatType(method.ReturnType.FullName);
							
							if (string.IsNullOrEmpty(returnTypeName))
							{
								returnTypeName = "System.Void";
							}
						}
					}
				}
			}
		}

		protected virtual int GetCompatibleEventHandlers(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, out string[] eventHandlerNames)
		{
			eventHandlerNames = null;

			ProjectItem projectItem = GetProjectItem(document, codeBehind, codeBehindFile);
			if (projectItem != null)
			{
				EnvDTE.FileCodeModel fileCodeModel = projectItem.FileCodeModel;
				if (fileCodeModel != null)
				{
					CodeClass class2 = this.FindClass(fileCodeModel.CodeElements, className);
					if (class2 != null)
					{
						string eventHandlerSignature = GetEventHandlerSignature(objectTypeName, eventName);
						var list = new List<string>();

						foreach (CodeElement element in class2.Members)
						{
							CodeFunction codeFunction = element as CodeFunction;
							if (codeFunction != null)
							{
								string name = codeFunction.Name;
								if (!string.IsNullOrEmpty(name))
								{
									string str3 = GetFunctionSignature(codeFunction);
									if (string.Compare(eventHandlerSignature, str3, StringComparison.Ordinal) == 0)
									{
										list.Add(name);
									}
								}
							}
						}
						if (list.Count > 0)
						{
							eventHandlerNames = list.ToArray();
						}
					}
				}
			}

			return NativeMethods.S_OK;
		}
		
		protected virtual int CreateUniqueEventHandlerName(string document, string codeBehind, string codeBehindFile, string className, string objectName, string eventName, out string eventHandlerName)
		{
			eventHandlerName = null;

			ProjectItem projectItem = GetProjectItem(document, codeBehind, codeBehindFile);
			if (projectItem != null)
			{
				EnvDTE.FileCodeModel fileCodeModel = projectItem.FileCodeModel;
				if (fileCodeModel != null)
				{
					CodeClass class2 = this.FindClass(fileCodeModel.CodeElements, className);
					if (class2 != null)
					{
						System.Collections.Specialized.StringDictionary dictionary = new System.Collections.Specialized.StringDictionary();
						foreach (CodeElement element in class2.Members)
						{
							string name = element.Name;
							if (!string.IsNullOrEmpty(name))
							{
								dictionary[name] = name;
							}
						}
						string str2 = objectName + "_" + eventName;
						int num = 0;
						eventHandlerName = str2;
						while (dictionary[eventHandlerName] != null)
						{
							num = (int)(num + 1);
							eventHandlerName = str2 + ((int)num).ToString(CultureInfo.InvariantCulture);
						}
					}
				}
			}
			return 0;
		}

		protected virtual int CreateEventHandler(string document, string codeBehind, string codeBehindFile, string className, string objectTypeName, string eventName, string eventHandlerName)
		{
			int hr = NativeMethods.E_FAIL;

			ProjectItem projectItem = GetProjectItem(document, codeBehind, codeBehindFile);
			if (projectItem != null)
			{
				EnvDTE.FileCodeModel fileCodeModel = projectItem.FileCodeModel;
				if (fileCodeModel != null)
				{
					CodeClass class2 = this.FindClass(fileCodeModel.CodeElements, className);

					if ((class2 != null) && !string.IsNullOrEmpty(eventHandlerName))
					{
						string str;
						string[] strArray;
						string[] strArray2;
						this.GetEventHandlerSignature(objectTypeName, eventName, out str, out strArray2, out strArray);
						UndoContext undoContext = class2.DTE.UndoContext;
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
							if (string.IsNullOrEmpty(str))
							{
								str = "System.Void";
							}
							vsCMFunction vsCMFunctionFunction = vsCMFunction.vsCMFunctionFunction;
							if (str == "System.Void")
							{
								vsCMFunctionFunction = vsCMFunction.vsCMFunctionSub;
							}
							CodeFunction codeFunction = class2.AddFunction(eventHandlerName, vsCMFunctionFunction, str, -1, vsCMAccess.vsCMAccessProtected, codeBehindFile);
							if ((codeFunction != null) && (strArray2 != null))
							{
								for (int i = 0; i < strArray2.Length; i++)
								{
									codeFunction.AddParameter("_" + strArray[i], strArray2[i], -1);
								}
							}
							EditPoint point = codeFunction.StartPoint.CreateEditPoint();
							if (point != null)
							{
								point.SmartFormat(codeFunction.EndPoint);
							}
							hr = 0;
						}
						finally
						{
							if (undoContext != null)
							{
								if (hr >= 0)
								{
									undoContext.Close();

									var nemerleFileCodeModel = fileCodeModel as Nemerle.VisualStudio.FileCodeModel.NemerleFileCodeModel;
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
				}
			}

			return hr;
		}

		protected virtual CodeClass FindClass(CodeElements codeElements, string className)
		{
			if ((codeElements != null) && !string.IsNullOrEmpty(className))
			{
				foreach (CodeElement element in codeElements)
				{
					switch (element.Kind)
					{
						case vsCMElement.vsCMElementClass:
							{
								CodeClass class2 = element as CodeClass;
								if ((class2 == null) || string.Compare(class2.FullName, className, StringComparison.Ordinal) != 0)
								{
									continue;
								}
								return class2;
							}
						case vsCMElement.vsCMElementNamespace:
							{
								CodeNamespace namespace2 = element as CodeNamespace;
								if (namespace2 == null)
								{
									continue;
								}
								CodeClass class3 = this.FindClass(namespace2.Children, className);
								if (class3 == null)
								{
									continue;
								}
								return class3;
							}
					}
				}
			}
			return null;
		}

		protected ITypeResolutionService TypeResolutionService
		{
			get
			{
				ITypeResolutionService typeResolutionService = null;
				if (this._serviceProvider != null)
				{
					DynamicTypeService service2 = this._serviceProvider.GetService(typeof(DynamicTypeService)) as DynamicTypeService;
					if (service2 != null)
					{
						typeResolutionService = service2.GetTypeResolutionService(this._hierarchy);
					}
				}
				return typeResolutionService;
			}
		}

		protected virtual string VsFormatType(string fullTypeName)
		{
			if (fullTypeName != null)
			{
				return fullTypeName.Replace('+', '.');
			}
			return null;
		}

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
