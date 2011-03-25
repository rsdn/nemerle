using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Diagnostics.CodeAnalysis;
using System.Text;

using Microsoft.VisualStudio.TextManager.Interop;

using VSConstants = Microsoft.VisualStudio.VSConstants;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using Nemerle.VisualStudio.FileCodeModel;
using System.IO;
using System.Collections.Generic;
using System.Collections;
using System.Runtime.InteropServices;

namespace Nemerle.VisualStudio.LanguageService
{
	public partial class NemerleContainedLanguage : IVsContainedLanguageCodeSupport
	{
		private string _codeBehindFile = null;

		// TODO: заменить на получение имени файла из арибута aspx страницы
		private string CodeBehindFile
		{
			get
			{
				if (_codeBehindFile == null)
				{
					if (_filePath.EndsWith(".aspx", StringComparison.OrdinalIgnoreCase))
						_codeBehindFile = _filePath + NemerleConstants.FileExtension;
					else
						_codeBehindFile = _filePath;
				}

				return _codeBehindFile;
			}
		}

		private NemerleCodeBehindEventBinder GetBinder()
		{
			var projectItem = intellisenseProject.GetProjectItem(CodeBehindFile);

			return new NemerleCodeBehindEventBinder(projectItem.FileCodeModel, _typeResolutionService);
		}

		public int CreateUniqueEventName(string pszClassName, string pszObjectName, string pszNameOfEvent, out string pbstrEventHandlerName)
		{
			pbstrEventHandlerName = GetBinder().CreateUniqueEventHandlerName(pszClassName, pszObjectName, pszNameOfEvent);
			return VSConstants.S_OK;
		}

		[SuppressMessage("Microsoft.Globalization", "CA1303:DoNotPassLiteralsAsLocalizedParameters", MessageId = "System.ArgumentException.#ctor(System.String)")]
		[SuppressMessage("Microsoft.Usage", "CA2204:LiteralsShouldBeSpelledCorrectly", MessageId = "psz")]
		[SuppressMessage("Microsoft.Usage", "CA2208:InstantiateArgumentExceptionsCorrectly")]
		public int EnsureEventHandler(string pszClassName, string pszObjectTypeName, string pszNameOfEvent, string pszEventHandlerName, uint itemidInsertionPoint, out string pbstrUniqueMemberID, out string pbstrEventBody, TextSpan[] pSpanInsertionPoint)
		{
			if (string.IsNullOrEmpty(pszClassName))
			{
				throw new System.ArgumentNullException("pszClassName");
			}
			if ((null == pSpanInsertionPoint) || (pSpanInsertionPoint.Length == 0))
			{
				throw new System.ArgumentNullException("pSpanInsertionPoint");
			}
			if (string.IsNullOrEmpty(pszEventHandlerName))
			{
				throw new System.ArgumentNullException("pszEventHandlerName");
			}

			// Initialize the out parameters.
			pbstrUniqueMemberID = null;
			pbstrEventBody = null;

			var binder = GetBinder();

			var codeClass = binder.FindClass(pszClassName);
			if (codeClass == null)
				return VSConstants.E_FAIL;

			var handler = binder.FindEventHandler(pszClassName, pszObjectTypeName, pszNameOfEvent, pszEventHandlerName);

			if (handler != null)
			{
				pbstrUniqueMemberID = ClassMemberUniqueId(codeClass, pszEventHandlerName);
				pSpanInsertionPoint[0] = CodeElementSpan(handler);

				return VSConstants.S_OK;
			}
			else
			{
				handler = binder.CreateEventHandler(CodeBehindFile, pszClassName, pszObjectTypeName, pszNameOfEvent, pszEventHandlerName);
				if (handler == null)
					return VSConstants.S_OK;

				// Make sure that the right document is visible.
				if (null != _projectItem)
				{
					_projectItem.Open(System.Guid.Empty.ToString("B"));
					if (null != _projectItem.Document)
					{
						_projectItem.Document.Activate();
					}
				}

				var textSpan = new TextSpan();
				textSpan.iStartLine = textSpan.iEndLine = codeClass.EndPoint.Line;
				textSpan.iStartIndex = 0;
				textSpan.iEndIndex = codeClass.EndPoint.LineCharOffset;

				pSpanInsertionPoint[0] = textSpan;
				pbstrUniqueMemberID = ClassMemberUniqueId(codeClass, pszEventHandlerName);

				var preferences = LanguageService.GetLanguagePreferences();

				var function = Environment.NewLine + CodeFunctionToString(handler, codeClass).Trim('\n', '\r');
				function = function.Replace("\n", "\n" + (preferences.InsertTabs ? "\t" : new string(' ', preferences.TabSize)));

				pbstrEventBody = function + Environment.NewLine;

				return VSConstants.S_OK;
			}
		}

		private string CodeFunctionToString(EnvDTE.CodeFunction function, EnvDTE.CodeClass parent)
		{
			var domFunction = function as CodeDomCodeElement<CodeMemberMethod>;
			var domClass = parent as CodeDomCodeElement<CodeTypeDeclaration>;

			if (domFunction == null || domClass == null)
				throw new ArgumentException();

			return Nemerle.Compiler.Utils.FormCodeDomGenerator.ToString(domFunction.CodeObject, domClass.CodeObject);
		}

		public int GetBaseClassName(string pszClassName, out string pbstrBaseClassName)
		{
			throw new System.NotImplementedException();
		}

		public int GetCompatibleEventHandlers(string pszClassName, string pszObjectTypeName, string pszNameOfEvent, out int pcMembers,
			out IntPtr ppbstrEventHandlerNames,
			out IntPtr ppbstrMemberIDs)
		{
			pcMembers = 0;
			ppbstrEventHandlerNames = IntPtr.Zero;
			ppbstrMemberIDs = IntPtr.Zero;

			var binder = GetBinder();

			var codeClass = binder.FindClass(pszClassName);
			if (codeClass == null)
				return VSConstants.E_FAIL;

			var handlers = binder.GetCompatibleEventHandlers(pszClassName, pszObjectTypeName, pszNameOfEvent);
			if (handlers == null)
				return VSConstants.E_FAIL;

			pcMembers = handlers.Length;
			ppbstrEventHandlerNames = StringArrayToIntPtr(handlers);
			ppbstrMemberIDs = StringArrayToIntPtr(Array.ConvertAll(handlers, name => ClassMemberUniqueId(codeClass, name)));

			return VSConstants.S_OK;
		}

		public static IntPtr StringArrayToIntPtr(string[] array)
		{
			IntPtr[] pointers = new IntPtr[array.Length];

			for (int i = 0; i < array.Length; i++)
			{
				pointers[i] = Marshal.StringToBSTR(array[i]);
			}

			IntPtr rRoot = Marshal.AllocCoTaskMem(IntPtr.Size * array.Length);
			Marshal.Copy(pointers, 0, rRoot, array.Length);
			return rRoot;
		}

		public int GetEventHandlerMemberID(string pszClassName, string pszObjectTypeName, string pszNameOfEvent, string pszEventHandlerName, out string pbstrUniqueMemberID)
		{
			// This function must return an unique string for the event handler.
			// Note that if this function returns a not empty string, then it means that the event handler
			// is implemented inside the secondary buffer.
			pbstrUniqueMemberID = null;

			var binder = GetBinder();

			var codeClass = binder.FindClass(pszClassName);
			if (codeClass == null)
				return VSConstants.S_OK;

			var handler = binder.FindEventHandler(pszClassName, pszObjectTypeName, pszNameOfEvent, pszEventHandlerName);

			if (handler == null)
				return VSConstants.S_OK;

			pbstrUniqueMemberID = ClassMemberUniqueId(codeClass, pszEventHandlerName);

			return VSConstants.S_OK;
		}

		public int GetMemberNavigationPoint(string pszClassName, string pszUniqueMemberID, TextSpan[] pSpanNavPoint, out uint pItemID)
		{
			// Validate the parameters
			if (string.IsNullOrEmpty(pszClassName))
			{
				throw new System.ArgumentNullException("pszClassName");
			}

			if (string.IsNullOrEmpty(pszUniqueMemberID))
			{
				throw new System.ArgumentNullException("pszUniqueMemberID");
			}

			if ((null == pSpanNavPoint) || (0 == pSpanNavPoint.Length))
			{
				throw new ArgumentNullException("pSpanNavPoint");
			}

			var binder = GetBinder();

			// Verify that the unique id is valid.
			string memberName = MemberNameFromUniqueId(pszUniqueMemberID);
			if (string.IsNullOrEmpty(memberName))
			{
				throw new System.ArgumentException();
			}
			// Remove the '_' added by the file code model
			if (memberName[0] == '_')
			{
				memberName = memberName.Substring(1);
			}

			// Get the code element for the member.
			var member = binder.FindClassMember(pszClassName, memberName);
			if (member == null)
			{
				throw new System.ArgumentException();
			}

			// Set the output variables.
			var navPoint = member.GetEndPoint(EnvDTE.vsCMPart.vsCMPartNavigate);
			pSpanNavPoint[0] = new TextSpan
			{
				iStartIndex = navPoint.LineCharOffset,
				iStartLine = navPoint.Line,
				iEndIndex = navPoint.LineCharOffset,
				iEndLine = navPoint.Line
			};

			pItemID = intellisenseProject.GetProjectItemId(CodeBehindFile);

			return VSConstants.S_OK;
		}

		public int GetMembers(string pszClassName, uint dwFlags, out int pcMembers, out IntPtr ppbstrDisplayNames, out IntPtr ppbstrMemberIDs)
		{
			throw new System.NotImplementedException();
		}

		public int IsValidID(string bstrID, out bool pfIsValidID)
		{
			// This function should return the same value as EnvDTE.CodeModel.IsValidID, but at the moment
			// there is no IronPython support for CodeModel (only FileCodeModel), so we assume that any
			// identifier is a valid ID.
			pfIsValidID = true;
			return VSConstants.S_OK;
		}

		public int OnRenamed(ContainedLanguageRenameType clrt, string bstrOldID, string bstrNewID)
		{
			// This method is called when a control is renamed; we don't want to do anything in this case.
			return VSConstants.S_OK;
		}

		private static string uniqueIdSeparator = "@";
		private static string ClassMemberUniqueId(EnvDTE.CodeClass codeClass, string memberName)
		{
			return string.Format(System.Globalization.CultureInfo.InvariantCulture, "{0}{1}{2}", memberName, uniqueIdSeparator, codeClass.FullName);
		}

		private static string MemberNameFromUniqueId(string uniqueId)
		{
			if (string.IsNullOrEmpty(uniqueId))
			{
				return null;
			}
			int separatorPos = uniqueId.IndexOf(uniqueIdSeparator);
			if (separatorPos <= 0)
			{
				return null;
			}
			return uniqueId.Substring(0, separatorPos);
		}

		private static TextSpan CodeElementSpan(EnvDTE.CodeFunction element)
		{
			try
			{
				return SpanFromTextPoints(element.StartPoint, element.EndPoint);
			}
			catch
			{
				return new TextSpan();
			}
		}

		private static TextSpan CodeElementSpan(EnvDTE.CodeElement element)
		{
			try
			{
				return SpanFromTextPoints(element.StartPoint, element.EndPoint);
			}
			catch
			{
				return new TextSpan();
			}
		}

		private static TextSpan SpanFromTextPoints(EnvDTE.TextPoint start, EnvDTE.TextPoint end)
		{
			TextSpan span = new TextSpan();

			span.iStartLine = start.Line;
			span.iStartIndex = start.DisplayColumn;

			EnvDTE.TextPoint endPoint = end;
			if (null == endPoint)
			{
				endPoint = start;
			}
			span.iEndLine = endPoint.Line;
			span.iEndIndex = endPoint.DisplayColumn;

			if (span.iStartIndex > 0)
				span.iStartIndex--;

			if (span.iEndIndex > 0)
				span.iEndIndex--;

			return span;
		}
	}
}
