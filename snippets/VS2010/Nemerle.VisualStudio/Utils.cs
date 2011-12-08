using System;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Nemerle.Compiler;
using Nemerle.VisualStudio.LanguageService;
using Msbuild = Microsoft.Build.Evaluation;
using Microsoft.VisualStudio.TextManager.Interop;
using Nemerle.Compiler.Parsetree;
using Nemerle.Completion2;
using System.Text;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Win32;
using System.Diagnostics;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Language.Intellisense;

namespace Nemerle.VisualStudio
{
	static class Utils
	{
		public static void RunSyncInUIThread(Action action)
		{
			if (UIThread.Instance.IsUIThread)
				return;

			UIThread.Instance.RunSync(action);
		}

		public static T CalcSyncInUIThread<T>(Func<T> func)
		{
			if (UIThread.Instance.IsUIThread)
				return func();

			T value = default(T);
			UIThread.Instance.RunSync(() => value = func());
			return value;
		}

		public static string HtmlMangling(string str)
		{
			return str.Replace("&", "&amp;").Replace(">", "&gt;").Replace("<", "&lt;");
		}

		public static ISmartTagBroker GetSmartTagBroker(this ITextView textView)
		{
			ISmartTagBroker tag = null;
			// The ISmartTagBroker property added in NemerleImplementsSmartTaggerProvider
			textView.Properties.TryGetProperty<ISmartTagBroker>(typeof(ISmartTagBroker), out tag);
			return tag;
		}

		public static IVsTextBuffer ToIVsTextBuffer(this ITextBuffer textBuffer)
		{
			IVsTextBuffer buffer;

			if (!textBuffer.Properties.TryGetProperty<IVsTextBuffer>(typeof(IVsTextBuffer), out buffer))
				return null;

			return buffer;
		}

		public static ITextBuffer ToITextBuffer(this IVsTextBuffer vsTextBuffer)
		{
			object obj2;
			IVsUserData data = vsTextBuffer as IVsUserData;
			if (data == null)
			{
				throw new InvalidOperationException("The shims should allow us to cast to IVsUserData");
			}
			Guid guidIVxTextBuffer = DefGuidList.guidIVxTextBuffer;
			ErrorHelper.ThrowOnFailure(data.GetData(ref guidIVxTextBuffer, out obj2));
			ITextBuffer buffer = obj2 as ITextBuffer;
			if (buffer == null)
			{
				throw new InvalidOperationException("user data doesnt implement the interface");
			}
			return buffer;
		}

		public static ITextView ToITextView(this IVsTextView vsTextView)
		{
			object obj2;
			IVsUserData data = vsTextView as IVsUserData;
			if (data == null)
			{
				throw new InvalidOperationException("The IVsTextView shims should allow us to cast to IVsUserData");
			}
			Guid guidIWpfTextViewHost = DefGuidList.guidIWpfTextViewHost;
			ErrorHelper.ThrowOnFailure(data.GetData(ref guidIWpfTextViewHost, out obj2));
			IWpfTextViewHost host = obj2 as IWpfTextViewHost;
			return host.TextView;
		}

		public static IVsTextView ToVsTextView(this ITextView view)
		{
			return view.Properties.GetProperty<IVsTextView>(typeof(IVsTextView));
		}

		public static string GetFilePath(this IVsTextView textViewAdapter)
		{
			return GetFilePath(GetBuffer(textViewAdapter));
		}

		public static IVsTextLines GetBuffer(this IVsTextView textViewAdapter)
		{
			IVsTextLines vsTextLines;
			ErrorHelper.ThrowOnFailure(textViewAdapter.GetBuffer(out vsTextLines));
			return vsTextLines;
		}

		public static string GetFilePath(this ITextBuffer textBuffer)
		{
			return GetFilePath((IPersistFileFormat)textBuffer.ToIVsTextBuffer());
		}

		private static string GetFilePath(IPersistFileFormat persistFileFormat)
		{
			string filePath;
			uint formatIndex;
			ErrorHelper.ThrowOnFailure(persistFileFormat.GetCurFile(out filePath, out formatIndex));
			return filePath;
		}

		public static string GetFilePath(this IVsTextLines vsTextLines)
		{
			return GetFilePath((IPersistFileFormat)vsTextLines);
		}

		public static string GetLiadingSpaces(this string text)
		{
			return text.Substring(0, text.Length - text.TrimStart(' ', '\t').Length);
		}

		public static string MakeIndentString(int indentCount, bool insertTabs, int indentSize, int tabSize)
		{
			string indent = null;

			if (insertTabs)
			{
				var allIndentSize = indentCount   * indentSize;
				var spacesNeeded  = allIndentSize % tabSize;
				var tabsNeeded    = allIndentSize / tabSize;

				if (tabsNeeded > 0 && spacesNeeded > 0)
				{
					var sb = new StringBuilder(tabsNeeded + spacesNeeded);
					sb.Append('\t', tabsNeeded);
					sb.Append(' ', spacesNeeded);
					indent = sb.ToString();
				}
				else if (tabsNeeded > 0)
					indent = new string('\t', tabsNeeded);
				else if (spacesNeeded > 0)
					indent = new string(' ', spacesNeeded);
			}
			else
				indent = new string(' ', indentSize * indentCount);

			return indent;
		}

		public static string MakeIndentString(this LanguagePreferences pref)
		{
			string indent = null;

			if (pref.InsertTabs)
			{
				var spacesNeeded = pref.IndentSize % pref.TabSize;
				var tabsNeeded = pref.IndentSize / pref.TabSize;

				if (tabsNeeded > 0 && spacesNeeded > 0)
				{
					var sb = new StringBuilder(tabsNeeded + spacesNeeded);
					sb.Append('\t', tabsNeeded);
					sb.Append(' ', spacesNeeded);
					indent = sb.ToString();
				}
				else if (tabsNeeded > 0)
					indent = new string('\t', tabsNeeded);
				else if (spacesNeeded > 0)
					indent = new string(' ', spacesNeeded);
			}
			else
				indent = new string(' ', pref.IndentSize);

			return indent;
		}

		public static TextPoint GetCaretTextPoint(this IVsTextView textView)
		{
			int line, idx;
			ErrorHandler.ThrowOnFailure(textView.GetCaretPos(out line, out idx));
			return new TextPoint(line + 1, idx + 1);
		}

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

		public static bool IsSpanEmpty(this TextSpan span)
		{
			return span.iStartLine == span.iEndLine && span.iStartIndex == span.iEndIndex;
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
			//TODO: проверить является ли GetPropertyValue аналогом GetEvaluatedProperty?
			return Eq(project.GetPropertyValue(valueName), "true");
		}

		public static bool Eq(string str1, string str2)
		{
			return string.Equals(str1, str2, StringComparison.InvariantCultureIgnoreCase);
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
				case ' ':
				case '\t':
				case '\v':
				case '{':
				case '}':
				case '(':
				case ')':
				case '[':
				case ']':
				case ';':
					return true;

				default:
					return false;
			}
		}

		public static TextSpan ToTextSpan(this Location location)
		{
			TextSpan span = new TextSpan();

			span.iStartLine = location.Line - 1;
			span.iStartIndex = location.Column - 1;
			span.iEndLine = location.EndLine - 1;
			span.iEndIndex = location.EndColumn - 1;

			return span;
		}

		public static TextSpan ToTextSpan(this TextPoint point)
		{
			TextSpan span = new TextSpan();

			span.iStartLine = point.Line - 1;
			span.iStartIndex = point.Column - 1;
			span.iEndLine = point.Line - 1;
			span.iEndIndex = point.Column - 1;

			return span;
		}

		public static TextSpan ToTextSpan(TextPoint startPoint, TextPoint endPoint)
		{
			TextSpan span = new TextSpan();

			span.iStartLine = startPoint.Line - 1;
			span.iStartIndex = startPoint.Column - 1;
			span.iEndLine = endPoint.Line - 1;
			span.iEndIndex = endPoint.Column - 1;

			return span;
		}

		public static int GetGlyph(TopDeclaration decl)
		{
			if (decl == null)
				return -1;

			NemerleModifiers attrs = decl.Attributes;

			GlyphType kind =
				decl is TopDeclaration.Delegate ? GlyphType.Delegate :
				decl is TopDeclaration.Enum ? GlyphType.Enum :
				decl is TopDeclaration.Interface ? GlyphType.Interface :
				decl is TopDeclaration.Macro ? GlyphType.Macro :
				decl is TopDeclaration.Variant ? GlyphType.Variant :
				decl is TopDeclaration.VariantOption ? GlyphType.VariantOption :
				Has(attrs, NemerleModifiers.Struct) ? GlyphType.Struct :
																							GlyphType.Class;

			GlyphSubtype modifier =
				Has(attrs, NemerleModifiers.Internal | NemerleModifiers.Protected) ?
														 GlyphSubtype.ProtectedInternal :
				Has(attrs, NemerleModifiers.Internal) ? GlyphSubtype.Internal :
				Has(attrs, NemerleModifiers.Protected) ? GlyphSubtype.Protected :
				Has(attrs, NemerleModifiers.Public) ? GlyphSubtype.Public :
				Has(attrs, NemerleModifiers.Private) ? GlyphSubtype.Private :
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

			if (member is ClassMember.Field) kind = 7;
			else if (member is ClassMember.Property) kind = 17;
			else if (member is ClassMember.Function) kind = 12;
			else if (member is ClassMember.Event) kind = 5;

			NemerleModifiers attrs = member.Attributes;

			modifier =
				Has(attrs, NemerleModifiers.Internal) ? 1 :
				Has(attrs, NemerleModifiers.Protected) ? 3 :
				Has(attrs, NemerleModifiers.Public) ? 0 :
				member.DefinedIn is TopDeclaration.VariantOption ? 0 :
																											 4;
			return kind * 6 + modifier;
		}

		private static bool Has(NemerleModifiers attrs, NemerleModifiers value)
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
			return Nemerle.Compiler.Utils.AstUtils.GetMemberLabel(member).Replace("\r\n", " ").Replace('\r', ' ').Replace('\n', ' ');
		}

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
			if (prefix.Length > 0)
			{
				output = prefix.Trim() + " " + output + Environment.NewLine;
			}
			Trace.WriteLine(output);
		}

		public static string GetRelativePath(string basePath, string subPath)
		{
			ErrorHelper.ThrowIsNullOrEmpty(basePath, "basePath");
			ErrorHelper.ThrowIsNullOrEmpty(subPath, "subPath");

			if (!Path.IsPathRooted(basePath))
				throw new ArgumentException("The 'basePath' is not rooted.");

			if (!Path.IsPathRooted(subPath))
				return subPath;

			if (!StringComparer.OrdinalIgnoreCase.Equals(Path.GetPathRoot(basePath), Path.GetPathRoot(subPath)))
				return subPath;

			if (!basePath.EndsWith(Path.DirectorySeparatorChar.ToString()))
				basePath += Path.DirectorySeparatorChar;

			Url url = new Url(basePath);
			return url.MakeRelative(new Url(subPath));
		}

		public static int TimeSince(int start)
		{
			int ticks = Environment.TickCount;
			long t = (long)ticks;
			long s = (long)start;
			// ticks wraps around every 29 days from int.MaxValue to int.MinValue, so we have to watch out 
			// for wrap around!
			if (ticks < start)
			{
				s = s - (long)int.MaxValue;
				t = t - (long)int.MinValue;
			}
			return (int)Math.Min((long)int.MaxValue, t - s);
		}

		public static string ToVsOutputStringFormat(this Location loc)
		{
			var res = loc.File + "(" + loc.Line + "," + loc.Column;

			if (loc.EndLine > loc.Line || loc.EndLine == loc.Line && loc.EndColumn > loc.Column)
				res += "," + loc.EndLine + "," + loc.EndColumn;

			return res + "): ";
		}

		public static AttributeTargets ToValidOn(object validOn)
		{
			var str = validOn.ToString();
			switch (str)
			{
				case "Type": return AttributeTargets.Class;
				default: return (AttributeTargets)Enum.Parse(typeof(AttributeTargets), str);
			}
		}

		public static string ValidOnToString(AttributeTargets attributeTargets)
		{
			var str = attributeTargets.ToString();
			switch (str)
			{
				case "Class": return "Type";
				default: return str;
			}
		}
	}
}
