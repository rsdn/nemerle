/***************************************************************************

A derivative work based on the SourceOutliner Power Toy sample.

Copyright (c) 2006 Microsoft Corporation. All rights reserved.

***************************************************************************/

using EnvDTE;
using EnvDTE80;
using System;
using System.Diagnostics;
using Nemerle.VisualStudio.FileCodeModel;

namespace Nemerle.VisualStudio.GUI.SourceOutliner
{
	/// <summary>
	/// Class containing helper methods for working with code elements.
	/// </summary>
	[CLSCompliant(false)]
	public static class CodeModelHelpers
	{
		/// <summary>
		/// Returns a CodeElement's access type.
		/// </summary>
		/// <param name="element">The CodeElement to examine.</param>
		/// <returns>The element's access type from the CodeAccessType enumeration.</returns>
		public static CodeAccessType ConvertCMAccessTypeToCodeAccessType(CodeElement element)
		{
			// Determine the access by first determining the kind of code element
			// and then casting it to the appropriate strongly-typed object.

			vsCMAccess rawAccess;

			if (element != null)
			{

				switch (element.Kind)
				{
					case vsCMElement.vsCMElementClass:
						rawAccess = ((CodeClass)element).Access;
						break;
					case vsCMElement.vsCMElementEnum:
						rawAccess = ((CodeEnum)element).Access;
						break;
					case vsCMElement.vsCMElementFunction:
						rawAccess = ((CodeFunction)element).Access;
						break;
					case vsCMElement.vsCMElementInterface:
						rawAccess = ((CodeInterface)element).Access;
						break;
					case vsCMElement.vsCMElementStruct:
						rawAccess = ((CodeStruct)element).Access;
						break;
					case vsCMElement.vsCMElementVariable:
						rawAccess = ((CodeVariable)element).Access;
						break;
					case vsCMElement.vsCMElementDelegate:
						rawAccess = ((CodeDelegate)element).Access;
						break;
					case vsCMElement.vsCMElementProperty:
						rawAccess = ((CodeProperty)element).Access;
						break;
					case vsCMElement.vsCMElementEvent:
						rawAccess = ((CodeEvent)element).Access;
						break;
					case vsCMElement.vsCMElementUnion://union is used to represent Nemerle variant
						rawAccess = ((CodeClass)element).Access;
						break;
					default:
						rawAccess = vsCMAccess.vsCMAccessDefault;
						break;
				}

				// Convert the raw access type to the CodeAccessType enumeration.
				switch (rawAccess)
				{
					case vsCMAccess.vsCMAccessPrivate:
						return CodeAccessType.Private;
					case vsCMAccess.vsCMAccessProtected:
						return CodeAccessType.Protected;
					case vsCMAccess.vsCMAccessPublic:
						return CodeAccessType.Public;
					case vsCMAccess.vsCMAccessAssemblyOrFamily:
						return CodeAccessType.Friend;
					default:
						return CodeAccessType.Public;
				}
			}
			else
				return CodeAccessType.Public;
		}

		/// <summary>
		/// Returns a CodeElement's type.
		/// </summary>
		/// <param name="element">The CodeElement to examine.</param>
		/// <returns>The element's type from the CodeElementType enumeration.</returns>
		public static CodeElementType ConvertCMElementTypeToCodeElementType(CodeElement element)
		{
			if (element == null)
			{
				throw new ArgumentNullException("element");
			}

			switch (element.Kind)
			{
				case vsCMElement.vsCMElementClass:
					return CodeElementType.Class;
				case vsCMElement.vsCMElementDelegate:
					return CodeElementType.Delegate;
				case vsCMElement.vsCMElementEnum:
					return CodeElementType.Enumeration;
				case vsCMElement.vsCMElementEvent:
					return CodeElementType.Event;
				case vsCMElement.vsCMElementFunction:
					return CodeElementType.Method;
				case vsCMElement.vsCMElementInterface:
					return CodeElementType.Interface;
				case vsCMElement.vsCMElementModule:
					return CodeElementType.Module;
				case vsCMElement.vsCMElementNamespace:
					return CodeElementType.Namespace;
				case vsCMElement.vsCMElementProperty:
					return CodeElementType.Property;
				case vsCMElement.vsCMElementStruct:
					return CodeElementType.Structure;
				case vsCMElement.vsCMElementVariable:
					return CodeElementType.Variable;
				case vsCMElement.vsCMElementUnion://Nemerle variant type
					return CodeElementType.Union;
				default:
					return CodeElementType.Method;
			}
		}

		/// <summary>
		/// Returns the short name of a CodeElement from the fully-qualified name. 
		/// </summary>
		/// <param name="element">The CodeElement to extract a name from.</param>
		/// <returns>The generic element name string.</returns>
		private static string ExtractGenericNameFromFullName(CodeElement element)
		{
			int index = element.FullName.LastIndexOf('.');
			string temp = null;

			try
			{
				temp = element.FullName.Substring(index + 1, element.FullName.Length - index - 1);
			}
			catch (ArgumentNullException)
			{
				temp = null;
			}

			return temp;
		}

		/// <summary>
		/// Returns the parameters list from a function.
		/// </summary>
		/// <param name="element">A CodeFunction2 object.</param>
		/// <returns>A string with function parameters.</returns>
		private static string ExtractMethodParameters(CodeFunction2 element)
		{
			// Get the string holding the stub definition of this function. 
			string temp = element.get_Prototype((int)vsCMPrototype.vsCMPrototypeParamTypes);
			int len = temp.Length;
			int index = temp.LastIndexOf('(');
			string str = null;
			try
			{
				str = temp.Substring(index, len - index);
			}
			catch (ArgumentNullException)
			{
				str = null;
			}

			return str;
		}

		/// <summary>
		/// Returns the parent of a CodeElement.
		/// </summary>
		/// <param name="element">The CodeElement whose parent is needed.</param>
		/// <returns>The parent CodeElement object.</returns>
		public static object GetCodeElementParent(CodeElement element)
		{
			object objectParent = null;

			if (element == null)
			{
				throw new ArgumentNullException("element");
			}

			switch (element.Kind)
			{
				case EnvDTE.vsCMElement.vsCMElementClass:
					objectParent = ((CodeClass)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementUnion://nemerle variant type and variant options
					objectParent = ((CodeClass)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementDelegate:
					objectParent = ((CodeDelegate)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementEnum:
					objectParent = ((CodeEnum)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementEvent:
					objectParent = ((CodeEvent)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementFunction:
					objectParent = ((CodeFunction)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementInterface:
					objectParent = ((CodeInterface)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementModule:
					// VS has no CodeModule class, so a CodeClass is used instead
					// to provide the list of children.
					objectParent = ((CodeClass)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementNamespace:
					objectParent = ((CodeNamespace)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementProperty:
					objectParent = ((CodeProperty)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementStruct:
					objectParent = ((CodeStruct)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementVariable:
					objectParent = ((CodeVariable)element).Parent;
					break;
				case EnvDTE.vsCMElement.vsCMElementParameter:
					objectParent = ((CodeParameter)element).Parent;
					break;
				default:
					break;
			}

			return objectParent;
		}

		/// <summary>
		/// Returns a CodeElement's display name for use in TreeViews.
		/// </summary>
		/// <param name="element">The CodeElement whose display name is needed.</param>
		/// <returns>The element's display string.</returns>
		/// <remarks>
		/// For methods, this is the function prototype;
		/// for other elements, it is simply the Name.
		/// </remarks>
		public static string GetDisplayNameFromCMElement(CodeElement element)
		{
			string strName = null;

			if (element == null)
			{
				throw new ArgumentNullException("element");
			}

			try
			{
				if (CodeModelHelpers.IsGeneric(element, out strName))
				{
					Debug.Assert(strName != null);
				}
				else
				{
					switch (element.Kind)
					{
						case vsCMElement.vsCMElementFunction:
							CodeFunction codeFunction = ((CodeFunction)element);
							if (element is CodeDomCodeFunction)
								strName = codeFunction.get_Prototype((int)vsCMPrototype.vsCMPrototypeParamTypes | (int)vsCMPrototype.vsCMPrototypeType);
							else
								strName = codeFunction.get_Prototype((int)vsCMPrototype.vsCMPrototypeParamTypes);
							break;

						case vsCMElement.vsCMElementVariable:
							CodeDomCodeVariable codeTypeVariable = element as CodeDomCodeVariable;
							if (codeTypeVariable != null)
								strName = codeTypeVariable.get_Prototype((int)(int)vsCMPrototype.vsCMPrototypeType);
							else
								strName = element.Name;
							break;

						case vsCMElement.vsCMElementProperty:
							CodeDomCodeProperty codeTypeProperty = element as CodeDomCodeProperty;
							if (codeTypeProperty != null)
								strName = codeTypeProperty.get_Prototype((int)(int)vsCMPrototype.vsCMPrototypeType);
							else
								strName = element.Name;
							break;

						default:
							strName = element.Name;
							break;
					}
				}
			}
			catch
			{
				strName = element.Name;
			}

			return strName;
		}

		/// <summary>
		/// Returns the children of a CodeElement.
		/// </summary>
		/// <param name="element">The CodeElement parent to enumerate.</param>
		/// <returns>A CodeElements object containing the child elements.</returns>
		public static CodeElements GetMembersOf(CodeElement element)
		{
			CodeElements members = null;

			if (element == null)
			{
				throw new ArgumentNullException("element");
			}

			if (CodeModelHelpers.HasChildrenKind(element.Kind))
			{
				try
				{
					switch (element.Kind)
					{
						case vsCMElement.vsCMElementNamespace:
							members = ((CodeNamespace)element).Members;
							break;
						case vsCMElement.vsCMElementClass:
							members = ((CodeClass)element).Members;
							break;
						case vsCMElement.vsCMElementUnion://nemerle variant type and variant options
							members = ((CodeClass)element).Members;
							break;
						case vsCMElement.vsCMElementEnum:
							members = ((CodeEnum)element).Members;
							break;
						case vsCMElement.vsCMElementInterface:
							members = ((CodeInterface)element).Members;
							break;
						case vsCMElement.vsCMElementStruct:
							members = ((CodeStruct)element).Members;
							break;
						case vsCMElement.vsCMElementDelegate:
							members = ((CodeDelegate)element).Members;
							break;
						case vsCMElement.vsCMElementModule:
							// VS has no CodeModule class, so a CodeClass is used instead
							// to provide the list of children.
							members = ((CodeClass)element).Members;
							break;
					}
				}
				catch (Exception)
				{
					throw;
				}
			}

			return members;
		}

		/// <summary>
		/// Returns a CodeElement's documentation comment if it has 
		/// one, otherwise returns relevant information from the prototype.
		/// </summary>
		/// <param name="element">A CodeElement object.</param>
		/// <returns>A string representing the element's definition.</returns>
		public static string GetPrototypeFromCMElement(CodeElement element)
		{
			System.Xml.XmlDocument docComment = new System.Xml.XmlDocument();

			try
			{
				if (element == null)
				{
					throw new ArgumentNullException("element");
				}

				switch (element.Kind)
				{
					case vsCMElement.vsCMElementFunction:
						CodeFunction codeFunction = ((CodeFunction)element);
						if (!string.IsNullOrEmpty(codeFunction.DocComment))
						{
							docComment.LoadXml(String.Format("<docComment>{0}</docComment>", codeFunction.DocComment));
							System.Xml.XmlNode node = docComment.SelectSingleNode("/docComment/summary");
							if (node != null)
							{
								return String.Format("{0}\n{1}", node.InnerText.Trim(),
										codeFunction.get_Prototype(
										(int)vsCMPrototype.vsCMPrototypeType | (int)vsCMPrototype.vsCMPrototypeParamNames | (int)vsCMPrototype.vsCMPrototypeParamTypes));
							}
							else
							{
								return string.Empty;
							}
						}
						else
						{
							return codeFunction.get_Prototype((int)vsCMPrototype.vsCMPrototypeType |
																								(int)vsCMPrototype.vsCMPrototypeParamNames |
																								(int)vsCMPrototype.vsCMPrototypeParamTypes);
						}

					case vsCMElement.vsCMElementProperty:
						CodeProperty codeProperty = ((CodeProperty)element);
						if (!string.IsNullOrEmpty(codeProperty.DocComment))
						{
							docComment.LoadXml(String.Format("<docComment>{0}</docComment>", codeProperty.DocComment));
							System.Xml.XmlNode node = docComment.SelectSingleNode("/docComment/summary");
							if (node != null)
							{
								return String.Format("{0}\n{1}", node.InnerText.Trim(),
										codeProperty.get_Prototype((int)vsCMPrototype.vsCMPrototypeType |
																							 (int)vsCMPrototype.vsCMPrototypeParamNames |
																							 (int)vsCMPrototype.vsCMPrototypeParamTypes));
							}
							else
							{
								return string.Empty;
							}
						}
						else
						{

							return codeProperty.get_Prototype((int)vsCMPrototype.vsCMPrototypeType |
																								(int)vsCMPrototype.vsCMPrototypeParamNames |
																								(int)vsCMPrototype.vsCMPrototypeParamTypes);
						}

					case vsCMElement.vsCMElementVariable:
						CodeVariable codeVariable = ((CodeVariable)element);
						return codeVariable.get_Prototype((int)vsCMPrototype.vsCMPrototypeType);

					case vsCMElement.vsCMElementEvent:
						CodeEvent codeEvent = ((CodeEvent)element);
						if (!string.IsNullOrEmpty(codeEvent.DocComment))
						{
							docComment.LoadXml(String.Format("<docComment>{0}</docComment>", codeEvent.DocComment));
							System.Xml.XmlNode node = docComment.SelectSingleNode("/docComment/summary");
							if (node != null)
							{
								return String.Format("{0}\n{1}", node.InnerText.Trim(),
										codeEvent.get_Prototype((int)vsCMPrototype.vsCMPrototypeType |
																						(int)vsCMPrototype.vsCMPrototypeParamNames |
																						(int)vsCMPrototype.vsCMPrototypeParamTypes));
							}
							else
							{
								return string.Empty;
							}
						}
						else
						{

							return codeEvent.get_Prototype((int)vsCMPrototype.vsCMPrototypeType |
																						 (int)vsCMPrototype.vsCMPrototypeParamNames |
																						 (int)vsCMPrototype.vsCMPrototypeParamTypes);
						}

					case vsCMElement.vsCMElementDelegate:
						CodeDelegate codeDelegate = ((CodeDelegate)element);
						if (!string.IsNullOrEmpty(codeDelegate.DocComment))
						{
							docComment.LoadXml(String.Format("<docComment>{0}</docComment>", codeDelegate.DocComment));
							System.Xml.XmlNode node = docComment.SelectSingleNode("/docComment/summary");
							if (node != null)
							{
								return String.Format("{0}\n{1}", node.InnerText.Trim(),
										codeDelegate.get_Prototype((int)vsCMPrototype.vsCMPrototypeType |
																							 (int)vsCMPrototype.vsCMPrototypeParamNames |
																							 (int)vsCMPrototype.vsCMPrototypeParamTypes));
							}
							else
							{
								return string.Empty;
							}
						}
						else
						{

							return codeDelegate.get_Prototype((int)vsCMPrototype.vsCMPrototypeType |
																								(int)vsCMPrototype.vsCMPrototypeParamNames |
																								(int)vsCMPrototype.vsCMPrototypeParamTypes);
						}

					case vsCMElement.vsCMElementClass:
						CodeClass codeClass = ((CodeClass)element);
						if (!string.IsNullOrEmpty(codeClass.DocComment))
						{
							docComment.LoadXml(String.Format("<docComment>{0}</docComment>", codeClass.DocComment));
							System.Xml.XmlNode node = docComment.SelectSingleNode("/docComment/summary");
							if (node != null)
							{
								return node.InnerText.Trim();
							}
							else
							{
								return string.Empty;
							}
						}
						else
						{
							return string.Empty;
						}

					//nemerle variant type and variant options
					case vsCMElement.vsCMElementUnion:
						CodeClass codeVariant = ((CodeClass)element);
						if (!string.IsNullOrEmpty(codeVariant.DocComment))
						{
							docComment.LoadXml(String.Format("<docComment>{0}</docComment>", codeVariant.DocComment));
							System.Xml.XmlNode node = docComment.SelectSingleNode("/docComment/summary");
							if (node != null)
							{
								return node.InnerText.Trim();
							}
							else
							{
								return string.Empty;
							}
						}
						else
						{
							return string.Empty;
						}


					case vsCMElement.vsCMElementStruct:
						CodeStruct codeStruct = ((CodeStruct)element);
						if (!string.IsNullOrEmpty(codeStruct.DocComment))
						{
							docComment.LoadXml(String.Format("<docComment>{0}</docComment>", codeStruct.DocComment));
							System.Xml.XmlNode node = docComment.SelectSingleNode("/docComment/summary");
							if (node != null)
							{
								return node.InnerText.Trim();
							}
							else
							{
								return string.Empty;
							}
						}
						else
						{
							return string.Empty;
						}

					case vsCMElement.vsCMElementInterface:
						CodeInterface codeInterface = ((CodeInterface)element);
						if (!string.IsNullOrEmpty(codeInterface.DocComment))
						{
							docComment.LoadXml(String.Format("<docComment>{0}</docComment>", codeInterface.DocComment));
							System.Xml.XmlNode node = docComment.SelectSingleNode("/docComment/summary");
							if (node != null)
							{
								return node.InnerText.Trim();
							}
							else
							{
								return string.Empty;
							}
						}
						else
						{
							return string.Empty;
						}

					default:
						return string.Empty;
				}
			}
			catch
			{
				return string.Empty;
			}
		}

		/// <summary>
		/// Returns an identifier to use for a unique CodeElement.
		/// </summary>
		/// <param name="element">The CodeElement to ID.</param>
		/// <returns>A string that identifies the element.</returns>
		/// <remarks>
		/// Each language has limitations when creating a 'unique' identifier.
		/// If the language is VB, a method identifier may not actually 
		/// remain unique, because VB creates the identifier based on the original
		/// method signature and never updates it if the signature changes.
		/// For example, if a method is copied and pasted to create a similar method, the new 
		/// method initially has the same signature as the old one and thus VB's ElementID
		/// will return the same identifier for the clone as it returned for the copied method.
		/// If the language is C#, the code element throws and exception when accessing the 
		/// ElementID, so the function prototype is used here to generate an ID instead.
		/// </remarks>
		public static string GetUniqueElementId(CodeElement element)
		{
			string strElementID = null;

			if (element == null)
			{
				throw new ArgumentNullException("element");
			}

			try
			{
				// VB will return an ElementID for the element.
				if (element.Language == CodeModelLanguageConstants.vsCMLanguageVB)
				{
					CodeElement2 codeElement2 = element as CodeElement2;
					if (codeElement2 != null)
					{
						strElementID = codeElement2.ElementID;
					}
				}

				// For non-VB languages, construct an ID from the best available signature.
				if (strElementID == null)
				{
					switch (element.Kind)
					{
						case vsCMElement.vsCMElementFunction:
							CodeFunction codeFunction = ((CodeFunction)element);
							strElementID = codeFunction.get_Prototype((int)vsCMPrototype.vsCMPrototypeParamTypes);
							break;
						default:
							strElementID = element.FullName;
							break;
					}
				}
			}
			catch (Exception)
			{
				strElementID = element.Name;
			}

			return strElementID;
		}

		/// <summary>
		/// Determines whether a particular CodeElement type is one that can have child elements.
		/// </summary>
		/// <param name="kind">An object type identifier from the CodeElement.Kind enumeration.</param>
		/// <returns>true if the CodeElement type can have children, otherwise false.</returns>
		public static bool HasChildrenKind(EnvDTE.vsCMElement kind)
		{
			switch (kind)
			{
				case EnvDTE.vsCMElement.vsCMElementClass:
				case EnvDTE.vsCMElement.vsCMElementDelegate:
				case EnvDTE.vsCMElement.vsCMElementEnum:
				case EnvDTE.vsCMElement.vsCMElementInterface:
				case EnvDTE.vsCMElement.vsCMElementModule:
				case EnvDTE.vsCMElement.vsCMElementNamespace:
				case EnvDTE.vsCMElement.vsCMElementStruct:
				case EnvDTE.vsCMElement.vsCMElementUnion://nemerle variant type and variant options
					return true;

				default:
					return false;
			}
		}

		/// <summary>
		/// Returns a flag indicating whether a CodeElement is a generic, 
		/// and also returns the element's generic name.
		/// </summary>
		/// <param name="element">The CodeElement to test.</param>
		/// <param name="name">The returned generic name.</param>
		/// <returns>true if the delegate is a generic, otherwise false.</returns>
		public static bool IsGeneric(CodeElement element, out string name)
		{
			bool isGen = false;
			string postfix = null;

			if (element == null)
			{
				throw new ArgumentNullException("element");
			}

			switch (element.Kind)
			{
				case vsCMElement.vsCMElementClass:
					CodeClass2 codeClass = element as CodeClass2;

					if ((codeClass != null) && codeClass.IsGeneric)
					{
						isGen = true;
					}
					break;

				//union declaration is used to represent variant types
				case vsCMElement.vsCMElementUnion:
					CodeClass2 codeVariant = element as CodeClass2;

					if ((codeVariant != null) && codeVariant.IsGeneric)
					{
						isGen = true;
					}
					break;

				case vsCMElement.vsCMElementInterface:
					CodeInterface2 codeInterface = element as CodeInterface2;

					if ((codeInterface != null) && codeInterface.IsGeneric)
					{
						isGen = true;
					}
					break;


				case vsCMElement.vsCMElementFunction:
					CodeFunction2 codeFunction = element as CodeFunction2;

					if ((codeFunction != null) && codeFunction.IsGeneric)
					{
						// Get information about the parameters, which is appended to the function name later.
						postfix = ExtractMethodParameters(codeFunction);
						isGen = true;
					}
					break;

				case vsCMElement.vsCMElementProperty:
					CodeProperty2 codeProperty = element as CodeProperty2;

					if ((codeProperty != null) && codeProperty.IsGeneric)
					{
						isGen = true;
					}
					break;

				case vsCMElement.vsCMElementVariable:
					CodeVariable2 codeVariable = element as CodeVariable2;

					if ((codeVariable != null) && codeVariable.IsGeneric)
					{
						isGen = true;
					}
					break;

				case vsCMElement.vsCMElementDelegate:
					CodeDelegate2 codeDelegate = element as CodeDelegate2;

					if ((codeDelegate != null) && codeDelegate.IsGeneric)
					{
						isGen = true;
					}
					break;
			}

			if (isGen)
			{
				// postfix is not null if the CodeElement is a generic function.
				name = ExtractGenericNameFromFullName(element) + postfix;
			}
			else
			{
				name = null;
			}

			return isGen;
		}

		/// <summary>
		/// Determines whether a particular CodeElement type is one that will be diagrammed.
		/// </summary>
		/// <param name="kind">An object type identifier from the CodeElement.Kind enumeration.</param>
		/// <returns>true if Source Outliner is interested in the type, otherwise false.</returns>
		public static bool IsInterestingKind(EnvDTE.vsCMElement kind)
		{
			switch (kind)
			{
				case EnvDTE.vsCMElement.vsCMElementClass:
				case EnvDTE.vsCMElement.vsCMElementDelegate:
				case EnvDTE.vsCMElement.vsCMElementEnum:
				case EnvDTE.vsCMElement.vsCMElementEvent:
				case EnvDTE.vsCMElement.vsCMElementFunction:
				case EnvDTE.vsCMElement.vsCMElementInterface:
				case EnvDTE.vsCMElement.vsCMElementModule:
				case EnvDTE.vsCMElement.vsCMElementNamespace:
				case EnvDTE.vsCMElement.vsCMElementProperty:
				case EnvDTE.vsCMElement.vsCMElementStruct:
				case EnvDTE.vsCMElement.vsCMElementVariable:
				case EnvDTE.vsCMElement.vsCMElementParameter:
				case EnvDTE.vsCMElement.vsCMElementUnion:
					return true;

				default:
					return false;
			}
		}
	}
}