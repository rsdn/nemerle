using System;
using System.IO;
using System.Collections.Generic;

using Microsoft.VisualStudio.Project;
using Nemerle.Compiler;
using Nemerle.VisualStudio.LanguageService;
using Msbuild = Microsoft.Build.BuildEngine;
using Microsoft.VisualStudio.TextManager.Interop;
using Nemerle.Compiler.Parsetree;
using Nemerle.Completion2;
using System.Text;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Win32;
using System.Diagnostics;

namespace Nemerle.VisualStudio
{
	static class Utils
	{
		public static string GetTypeGuidAsString(HierarchyNode node)
		{
			Guid guid;
			var res = node.GetGuidProperty((int)__VSHPROPID.VSHPROPID_TypeGuid, out guid);
			if (res >= 0)
				return guid.ToString("B");
			return "";
		}

		public static string ConvertToText(List<string> lines)
		{
			string[] ary = new string[lines.Count + 1];
			lines.CopyTo(ary);
			ary[ary.Length - 1] = string.Empty;
			return string.Join(System.Environment.NewLine, ary);
		}

		public static Location LocationFromSpan(int fileIndex, TextSpan span)
		{
			return new Location(fileIndex, span.iStartLine + 1, span.iStartIndex + 1, span.iEndLine + 1, span.iEndIndex + 1);
		}

		public static TextSpan SpanFromLocation(Location location)
		{
			TextSpan span = new TextSpan();
			CopyLocationToSpan(location, ref span);
			return span;
		}

		public static void CopyLocationToSpan(Location location, ref TextSpan span)
		{
			span.iStartLine = location.Line - 1;
			span.iEndLine = location.EndLine - 1;
			span.iStartIndex = location.Column - 1;
			span.iEndIndex = location.EndColumn - 1;
		}

		public static bool IsAllWhiteSpace(string str)
		{
			foreach (char ch in str)
				if (!char.IsWhiteSpace(ch))
					return false;

			return true;
		}

		public static NemerleSource GetFileSource(IServiceProvider site, string filePath)
		{
			NemerleLanguageService lang = (NemerleLanguageService)site.GetService(
				typeof(NemerleLanguageService));
			return (NemerleSource)lang.GetSource(filePath);
		}

		public static string GetFileLine(IServiceProvider site, string filePath, int line)
		{
			return GetFileSource(site, filePath).GetLine(line);
		}

		public static string GetFileCode(IServiceProvider site, string filePath)
		{
			return GetFileSource(site, filePath).GetText();
		}

		public static bool IsTrue(Msbuild.Project project, string valueName)
		{
			return Eq(project.GetEvaluatedProperty(valueName), "true");
		}

		public static bool Eq(string str1, string str2)
		{
			return string.Compare(str1, str2, StringComparison.InvariantCultureIgnoreCase) == 0;
		}

		public static string GetModuleName(Type type)
		{
			// Replace is a temporary solution util we get PLK for Nemerle.VisualStudio.dll.
			//
			return Path.GetFileNameWithoutExtension(type.Module.Name);
		}

		public static T GetService<T>(IServiceProvider provider)
			where T : class
		{
			return provider.GetService(typeof(T)) as T;
		}

		public static bool IsNemerleFileExtension(string path)
		{
			ErrorHelper.ThrowIsNullOrEmpty(path, "path");

			return String.Compare(
				Path.GetExtension(path),
				NemerleConstants.FileExtension,
				StringComparison.OrdinalIgnoreCase) == 0;
		}

		public static string NemerleBinariesPath
		{
			get
			{
				string path = new Uri(typeof(GlobalEnv).Assembly.CodeBase).LocalPath;
				return Path.GetDirectoryName(path);
			}
		}

		public static T GetRequiredService<T>(IServiceProvider provider)
			where T : class
		{
			T service = GetService<T>(provider);

			ErrorHelper.ThrowIsNull(service, "service", typeof(T).FullName);

			return service;
		}

		public static bool IsSeparator(char ch)
		{
			switch (ch)
			{
				case ' ': case '\t': case '\v':
				case '{': case '}':
				case '(': case ')':
				case '[': case ']':
				case ';':
					return true;

				default:
					return false;
			}
		}

		public static TextSpan ToTextSpan(this Location location)
		{
			TextSpan span = new TextSpan();

			span.iStartLine  = location.Line	  - 1;
			span.iStartIndex = location.Column	- 1;
			span.iEndLine	= location.EndLine   - 1;
			span.iEndIndex   = location.EndColumn - 1;

			return span;
		}
		
		public static int GetGlyph(TopDeclaration decl)
		{
			if (decl == null)
				return -1;

			NemerleAttributes attrs = decl.Attributes;

			GlyphType kind =
				decl is TopDeclaration.Delegate ? GlyphType.Delegate :
				decl is TopDeclaration.Enum ? GlyphType.Enum :
				decl is TopDeclaration.Interface ? GlyphType.Interface :
				decl is TopDeclaration.Macro ? GlyphType.Macro :
				decl is TopDeclaration.Variant ? GlyphType.Variant :
				decl is TopDeclaration.VariantOption ? GlyphType.VariantOption :
				Has(attrs, NemerleAttributes.Struct) ? GlyphType.Struct :
																							GlyphType.Class;

			GlyphSubtype modifier =
				Has(attrs, NemerleAttributes.Internal | NemerleAttributes.Protected) ?
														 GlyphSubtype.ProtectedInternal :
				Has(attrs, NemerleAttributes.Internal) ? GlyphSubtype.Internal :
				Has(attrs, NemerleAttributes.Protected) ? GlyphSubtype.Protected :
				Has(attrs, NemerleAttributes.Public) ? GlyphSubtype.Public :
				Has(attrs, NemerleAttributes.Private) ? GlyphSubtype.Private :
				decl is TopDeclaration.VariantOption ? GlyphSubtype.Public :
																								 GlyphSubtype.Private;

			return (int)kind + (int)modifier;
		}

		public static int GetGlyph(ClassMember member)
		{
			if (member == null)
				return -1;

			int kind = 0;
			int modifier = 0;

			if	  (member is ClassMember.Field)	kind = 7;
			else if (member is ClassMember.Property) kind = 17;
			else if (member is ClassMember.Function) kind = 12;
			else if (member is ClassMember.Event)	kind = 5;

			NemerleAttributes attrs = member.Attributes;

			modifier =
				Has(attrs, NemerleAttributes.Internal)		   ? 1 :
				Has(attrs, NemerleAttributes.Protected)		  ? 3 :
				Has(attrs, NemerleAttributes.Public)			 ? 0 :
				member.DefinedIn is TopDeclaration.VariantOption ? 0 :
																											 4;
			return kind * 6 + modifier;
		}
		
		private static bool Has(NemerleAttributes attrs, NemerleAttributes value)
		{
			return (attrs & value) == value;
		}

		public static string Join<T>(this IEnumerable<T> values, string separator)
		{
			var result = new StringBuilder();

			foreach (var v in values)
			{
				result.Append(v.ToString());
				result.Append(separator);
			}

			if (result.Length > 0)
				result.Length -= separator.Length;

			return result.ToString();
		}

		public static bool IsNullOrEmpty(this string str)
		{
			return string.IsNullOrEmpty(str);
		}

		public static int ToVsLineCoord(this int coord)
		{
			return coord - 1;
		}

		public static int ToNccLineCoord(this int coord)
		{
			return coord + 1;
		}

		public static int ToVsColCoord(this int coord)
		{
			return coord - 1;
		}

		public static int ToNccColCoord(this int coord)
		{
			return coord + 1;
		}

		public static ClassMember[] GetMembers(this TopDeclaration decl)
		{
			var members = Nemerle.Compiler.Utils.AstUtils.GetMembers(decl).ToArray();
			// Sort by name (like in C#).
			Array.Sort(members, (m1, m2) => string.Compare(m1.Name, m2.Name));
			return members;
		}

		public static string GetLabel(this TopDeclaration decl)
		{
			if (decl.DefinedIn != null)
			{
				var name = GetLabel(decl.DefinedIn) + "." + decl.Name;
				return name;
			}
			else
			{
				var ns = decl.name.GetName().context.CurrentNamespace.FullName.Join(".");
				return ns.IsNullOrEmpty() ? decl.Name : ns + "." + decl.Name;
			}
		}

		public static string GetLabel(this ClassMember member)
		{
			return Nemerle.Compiler.Utils.AstUtils.GetMemberLabel(member);
		}
/*
		public static RegistryKey VSRegistry_RegistryRoot
		{
			get
			{
				Microsoft.VisualStudio.Shell.Interop.ILocalRegistry3 ILocalRegistry3 =
					Microsoft.VisualStudio.Shell.Package.GetGlobalService(typeof(Microsoft.VisualStudio.Shell.Interop.SLocalRegistry))
					as Microsoft.VisualStudio.Shell.Interop.ILocalRegistry3;
				string root = null;
				ILocalRegistry3.GetLocalRegistryRoot(out root);

				if(root == null)
					return null;

				return Registry.LocalMachine.OpenSubKey(root);
			}
		}
*/
		/// <summary>
		/// Displays the specified message string.
		/// </summary>
		/// <param name="prefix">An optional message prefix, such as Status: or Error:.</param>
		/// <param name="message">The message to write.</param>
		public static void DisplayMessage(string prefix, string message)
		{
			// Messages are current shown to the trace, not the user.
			// Change this to a MessageBox to display in the UI.
			string output = message.Trim();
			if(prefix.Length > 0)
			{
				output = prefix.Trim() + " " + output + Environment.NewLine;
			}
			Trace.WriteLine(output);
		}
	}
}
