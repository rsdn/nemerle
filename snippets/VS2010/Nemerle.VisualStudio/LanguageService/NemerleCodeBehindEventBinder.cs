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
	// Provide base functionality for event binding 
	internal class NemerleCodeBehindEventBinder
	{
		private EnvDTE.FileCodeModel _fileCodeModel;
		private ITypeResolutionService _typeResolutionService;

		public NemerleCodeBehindEventBinder(EnvDTE.FileCodeModel fileCodeModel, ITypeResolutionService typeResolutionService)
		{
			_fileCodeModel = fileCodeModel;
			_typeResolutionService = typeResolutionService;
		}

		public EnvDTE.FileCodeModel FileCodeModel
		{
			get { return _fileCodeModel; }
		}

		public CodeClass FindClass(string className)
		{
			return FindClass(_fileCodeModel.CodeElements, className);
		}

		public CodeElement FindClassMember(string className, string member)
		{
			var codeClass = FindClass(className);
			if (codeClass == null)
				return null;

			foreach (CodeElement element in codeClass.Members)
			{
				if (element.Name == member)
					return element;
			}

			return null;
		}
		
		public CodeFunction FindEventHandler(string className, string objectTypeName, string eventName, string eventHandlerName)
		{
			var codeClass = FindClass(className);
			if (codeClass == null)
				return null;

			Type objectType = ResolveType(objectTypeName);

			string eventHandlerSignature = GetEventHandlerSignature(objectType, eventName);

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

			return null;
		}

		public string[] GetCompatibleEventHandlers(string className, string objectTypeName, string eventName)
		{
			Type objectType = ResolveType(objectTypeName); 
			string eventHandlerSignature = GetEventHandlerSignature(objectType, eventName);

			var list = new List<string>();

			var codeClass = FindClass(className);

			if (codeClass != null)
			{
				foreach (CodeElement element in codeClass.Members)
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
			}

			return list.ToArray();
		}

		public string CreateUniqueEventHandlerName(string className, string objectName, string eventName)
		{
			string eventHandlerName = null;

			var codeClass = FindClass(className);
			if (codeClass != null)
			{
				System.Collections.Specialized.StringDictionary dictionary = new System.Collections.Specialized.StringDictionary();
				foreach (CodeElement element in codeClass.Members)
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

			return eventHandlerName;
		}

		public CodeFunction CreateEventHandler(string codeBehindFile, string className, string objectTypeName, string eventName, string eventHandlerName)
		{
			var codeClass = FindClass(className);

			if (codeClass != null)
			{
				if (!string.IsNullOrEmpty(eventHandlerName))
				{
					string str;
					string[] strArray;
					string[] strArray2;

					Type objectType = ResolveType(objectTypeName); 
					GetEventHandlerSignature(objectType, eventName, out str, out strArray2, out strArray);

					if (string.IsNullOrEmpty(str))
					{
						str = "System.Void";
					}

					vsCMFunction vsCMFunctionFunction = vsCMFunction.vsCMFunctionFunction;
					if (str == "System.Void")
					{
						vsCMFunctionFunction = vsCMFunction.vsCMFunctionSub;
					}

					CodeFunction codeFunction = codeClass.AddFunction(eventHandlerName, vsCMFunctionFunction, str, -1, vsCMAccess.vsCMAccessProtected, codeBehindFile);
					if ((codeFunction != null) && (strArray2 != null))
					{
						for (int i = 0; i < strArray2.Length; i++)
						{
							codeFunction.AddParameter("_" + strArray[i], strArray2[i], -1);
						}
					}

					return codeFunction;
				}
			}

			return null;
		}

		private CodeClass FindClass(CodeElements codeElements, string className)
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

		private Dictionary<string, Type> _resolvedTypes = new Dictionary<string, Type>();
		private Type ResolveType(string typeName)
		{
			if (string.IsNullOrEmpty(typeName))
				return null;

			typeName = typeName.Trim();

			var type = _typeResolutionService.GetType(typeName);
			if (type == null)
			{
				type = CsTypeToClrType(typeName);
				if (type == null)
					type = _typeResolutionService.GetType("System." + typeName);
			}

			return type;
		}

		private Type CsTypeToClrType(string typeName)
		{
			switch (typeName)
			{
				case "void":
					return typeof(void);

				case "object":
					return typeof(object);

				case "bool":
					return typeof(bool);

				case "byte":
					return typeof(byte);

				case "sbyte":
					return typeof(sbyte);

				case "short":
					return typeof(short);

				case "ushort":
					return typeof(ushort);

				case "int":
					return typeof(int);

				case "uint":
					return typeof(uint);

				case "long":
					return typeof(long);

				case "ulong":
					return typeof(ulong);

				case "decimal":
					return typeof(decimal);

				case "double":
					return typeof(double);

				case "float":
					return typeof(float);

				case "char":
					return typeof(char);

				case "string":
					return typeof(string);

				default:
					return null;
			}
		}

		private string GetFunctionSignature(CodeFunction codeFunction)
		{
			string signature = null;

			if (codeFunction != null)
			{
				if (string.IsNullOrEmpty(codeFunction.Type.AsFullName))
				{
					signature = "System.Void";
				}
				else
				{
					var type = ResolveType(codeFunction.Type.AsFullName);
					signature = (type == null ? codeFunction.Type.AsFullName : type.FullName);
				}

				foreach (CodeElement element in codeFunction.Parameters)
				{
					CodeParameter parameter = element as CodeParameter;
					if (parameter != null)
					{
						var type = ResolveType(parameter.Type.AsFullName);
						signature = signature + "," + (type == null ? parameter.Type.AsFullName : type.FullName);
					}
				}
			}

			return signature;
		}

		private string GetEventHandlerSignature(Type objectType, string eventName)
		{
			string returnTypeName = null;
			string[] paramTypeNames = null;
			string[] paramNames = null;
			string signature = null;

			this.GetEventHandlerSignature(objectType, eventName, out returnTypeName, out paramTypeNames, out paramNames);

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

		private void GetEventHandlerSignature(Type objectType, string eventName, out string returnTypeName, out string[] paramTypeNames, out string[] paramNames)
		{
			returnTypeName = null;
			paramTypeNames = null;
			paramNames = null;

			if (objectType != null)
			{
				EventInfo info = objectType.GetEvent(eventName);
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

		private string VsFormatType(string fullTypeName)
		{
			if (fullTypeName != null)
			{
				return fullTypeName.Replace('+', '.');
			}
			return null;
		}
	}
}
