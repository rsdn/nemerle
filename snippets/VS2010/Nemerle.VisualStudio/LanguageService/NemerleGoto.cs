using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.SymbolStore;
using System.Linq;
using System.Reflection;
using System.Text;
using System.IO;
using Nemerle.Completion2;
using Microsoft.VisualStudio;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.TextManager.Interop;
using System.Runtime.CompilerServices;
using Location = Nemerle.Compiler.Location;
using Nemerle.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.LanguageService
{
	static class NemerleGoto
	{
		#region Constants

		private static /*readonly*/ Guid IID_MetaDataImport = new Guid("7DAC8207-D3AE-4c75-9B67-92801A497D44");
		private const BindingFlags DeclaredMembers = BindingFlags.DeclaredOnly
										| BindingFlags.Instance | BindingFlags.Static
										| BindingFlags.Public | BindingFlags.NonPublic;

		#endregion

		#region Debug

		private static TraceSwitch _ts;
		private static TraceSwitch TS
		{
			get
			{
				return _ts ?? (_ts = new TraceSwitch("NemerleAuthoringScope",
					"Enables Nemerle.VisualStudio.LanguageService.NemerleAuthoringScope trace"));
			}
		}

		#endregion

		internal static GotoInfo[] GenerateSource(GotoInfo[] infos, IIdeEngine engine, out string captiopn)
		{
			Debug.Assert(infos != null && infos.Length == 1, "GenerateSource lacks required parameter");
			GotoInfo info = infos[0];
			Debug.Assert(info != null && info.MemberInfo != null, "GenerateSource lacks required parameter");

			MemberInfo mi = info.MemberInfo;
			Type type = (mi.MemberType == MemberTypes.TypeInfo || mi.MemberType == MemberTypes.NestedType)
				? (Type)mi : mi.DeclaringType;

			captiopn = type.Name + "[from metadata]";

			Debug.WriteLineIf(TS.TraceVerbose, string.Format("Generating source for ({0})", type.FullName), TS.DisplayName);

			string tempFileName = string.Format("{0}{1}${2}v{3}${4}.n", Path.GetTempPath(),
				type.FullName, Path.GetFileName(info.FilePath),
				type.Assembly.GetName().Version, Process.GetCurrentProcess().Id);

			string text = null;

			if (File.Exists(tempFileName))
				text = File.ReadAllText(tempFileName, Encoding.UTF8);

			GotoInfo[] resulr = new GotoInfo[0];
			var fileIndex = Location.GetFileIndex(tempFileName);

			using (TextWriter w = new StringWriter())
			{
				resulr = new GotoInfo[] { engine.GenerateCode(info.Member, fileIndex, w) };
				var newText = w.ToString();
				var needSave = text == null || !string.Equals(newText, text, StringComparison.Ordinal);
				if (needSave)
				{
					if (text != null)
						File.SetAttributes(tempFileName, FileAttributes.Normal);
					File.WriteAllText(tempFileName, newText, Encoding.UTF8);
					File.SetAttributes(tempFileName, FileAttributes.ReadOnly | FileAttributes.Temporary);
				}
			}

			return resulr;
		}

		internal static GotoInfo[] LookupLocationsFromPdb(GotoInfo info, IVsSmartOpenScope vsSmartOpenScope)
		{
			Debug.Assert(info != null && info.MemberInfo != null, "LookupLocationsFromPdb lacks required parameter");
			var sources = new Dictionary<string, Location>();

			if (string.IsNullOrEmpty(info.FilePath) || !File.Exists(info.FilePath))
				return new GotoInfo[0];

			object unkMetaDataImport;
			IntPtr ptrMetaDataImport = IntPtr.Zero;
			ISymbolBinder1 binder;
			ISymbolReader reader;
			ISymbolDocument[] documents;
			int[] lines;
			int[] columns;
			int[] endLines;
			int[] endColumns;
			int[] offsets;
			var methods = new List<MethodBase>();
			Type ty = null;

			try
			{
				int hr = vsSmartOpenScope.OpenScope(info.FilePath, 0, ref IID_MetaDataImport, out unkMetaDataImport);

				if (hr == VSConstants.S_OK)
				{
					ptrMetaDataImport = Marshal.GetIUnknownForObject(unkMetaDataImport);
					binder = new SymBinder();
					reader = binder.GetReader(ptrMetaDataImport, info.FilePath, null);
					documents = new ISymbolDocument[1];
					lines = new int[1];
					columns = new int[1];
					endLines = new int[1];
					endColumns = new int[1];
					offsets = new int[1];
				}
				else
				{
					Debug.WriteLineIf(TS.TraceWarning,
						string.Format("Failed to obtain MetaDataImport from VS, hr 0x{0:X8}", hr), TS.DisplayName);

					return new GotoInfo[0];
				}

				switch (info.MemberInfo.MemberType)
				{
					case MemberTypes.Constructor:
					case MemberTypes.Method:
						MethodBase mb = (MethodBase)info.MemberInfo;

						// Abstract methods does not contain any code.
						//
						if (mb.IsAbstract)
							methods.AddRange(mb.DeclaringType.GetMethods(DeclaredMembers));
						else
							methods.Add(mb);
						break;

					case MemberTypes.Property:
						PropertyInfo pi = (PropertyInfo)info.MemberInfo;
						methods.AddRange(pi.GetAccessors(true));
						break;

					case MemberTypes.Field:
						methods.AddRange(info.MemberInfo.DeclaringType.GetMethods(DeclaredMembers));
						break;

					case MemberTypes.Event:
						EventInfo ei = (EventInfo)info.MemberInfo;
						methods.Add(ei.GetAddMethod(true));
						methods.Add(ei.GetRemoveMethod(true));
						methods.Add(ei.GetRaiseMethod(true));
						methods.AddRange(ei.GetOtherMethods(true));
						break;

					case MemberTypes.TypeInfo:
					case MemberTypes.NestedType:
						ty = (Type)info.MemberInfo;
						methods.AddRange(ty.GetMethods(DeclaredMembers));
						methods.AddRange(ty.GetConstructors(DeclaredMembers));
						break;
					default:
						Trace.Fail("Unexpected MemberType " + info.MemberInfo.MemberType);
						break;
				}


				foreach (MethodBase mb in methods)
				{
					if (mb == null || Attribute.GetCustomAttribute(mb, typeof(CompilerGeneratedAttribute)) != null)
						continue;

					try
					{
						SymbolToken token = new SymbolToken(mb.MetadataToken);
						ISymbolMethod method = reader.GetMethod(token);

						if (method.SequencePointCount > 0)
						{
							method.GetSequencePoints(offsets, documents, lines, columns, endLines, endColumns);

							var path = documents[0].URL;
							// We are interested in unique files only.
							if (File.Exists(path) && (ty == null || mb.DeclaringType.Equals(ty)))
							{
								Location value;
								if (sources.TryGetValue(path, out value))
								{
									if ((value.Column == 0 || value.Line == 0) && lines[0] != 0 && columns[0] != 0)
										sources[path] = new Location(path, lines[0], columns[0], endLines[0], endColumns[0]);
								}
								else
									sources.Add(path, new Location(path, lines[0], columns[0], endLines[0], endColumns[0]));
							}
						}
					}
					catch (COMException ex)
					{
						// Abstract method or not a method at all.
						// Sequence points are available only for methods.
						//
						Trace.WriteLineIf(TS.TraceError,
							string.Format("({0}) {1}, code 0x{2:X8}", mb.Name, ex.Message, ex.ErrorCode), TS.DisplayName);
					}
				}
			}
			catch (COMException ex)
			{
				// The file was not found or source locations were stripped from the pdb.
				//
				Trace.WriteLineIf(TS.TraceError,
					string.Format("{0}, code 0x{1:8X}", ex.Message, ex.ErrorCode), TS.DisplayName);
			}
			finally
			{
				if (ptrMetaDataImport != IntPtr.Zero)
					Marshal.Release(ptrMetaDataImport);
			}

			return sources.Select(x => new GotoInfo(x.Key, x.Value)).ToArray();
		}
	}
}
