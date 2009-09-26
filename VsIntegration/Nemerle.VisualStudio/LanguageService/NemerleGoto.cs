using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.SymbolStore;
using System.Linq;
using System.Reflection;
using System.IO;
using Nemerle.Completion2;
using Microsoft.VisualStudio;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.TextManager.Interop;
using System.Runtime.CompilerServices;
using Location = Nemerle.Compiler.Location;
using Nemerle.VisualStudio.Project;

namespace Nemerle.VisualStudio.LanguageService
{
	static class NemerleGoto
	{
		#region Constants

		private static /*readonly*/ Guid IID_MetaDataImport = new Guid("7DAC8207-D3AE-4c75-9B67-92801A497D44");
		private const BindingFlags DeclaredMembers = BindingFlags.DeclaredOnly
										| BindingFlags.Instance | BindingFlags.Static
										| BindingFlags.Public   | BindingFlags.NonPublic;

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

		// Configurable option:
		//
		// When true, all files recovered from the .pdb will be recompiled to find real location.
		// When false, the relevant method locations will be returned.
		//
		private static readonly bool _findExactLocation = true;

    internal static GotoInfo[] GenerateSource(GotoInfo[] infos, Engine engine)
		{
      Debug.Assert(infos != null && infos.Length == 1, "GenerateSource lacks required parameter");
      GotoInfo info = infos[0];
      Debug.Assert(info != null && info.MemberInfo != null, "GenerateSource lacks required parameter");

      MemberInfo mi = info.MemberInfo;
			Type type = (mi.MemberType == MemberTypes.TypeInfo || mi.MemberType == MemberTypes.NestedType) 
        ? (Type)mi : mi.DeclaringType;

			Debug.WriteLineIf(TS.TraceVerbose, string.Format("Generating source for ({0})", type.FullName), TS.DisplayName);

			string tempFileName = string.Format("{0}{1}${2}v{3}${4}.n", Path.GetTempPath(),
				type.FullName, Path.GetFileName(info.FilePath),
				type.Assembly.GetName().Version, Process.GetCurrentProcess().Id);

      if (File.Exists(tempFileName))
        File.Delete(tempFileName);

			// Delete previously generated file, if any.
			//
			////if (_temporaryFileRef != null)
			////	_temporaryFileRef.Close();
      GotoInfo[] resulr = new GotoInfo[0];

			using (TextWriter w = new StreamWriter(tempFileName))
        resulr = new GotoInfo[] { engine.GenerateCode(type, info.MemberInfo, w) };

			// Made this file 'DeleteOnClose'.
			//
			////_temporaryFileRef = new FileStream(tempFileName, FileMode.Open,
			////	FileAccess.Read, FileShare.ReadWrite, 4096, FileOptions.DeleteOnClose);

			// Mark this file is not editable.
			//
			File.SetAttributes(tempFileName, FileAttributes.ReadOnly | FileAttributes.Temporary);

			// Now Visual studio can open this file.
			//
      return resulr;
		}

    internal static GotoInfo[] LookupLocationsFromPdb(GotoInfo info, ProjectInfo projectInfo)
		{
      Debug.Assert(info != null && info.MemberInfo != null, "LookupLocationsFromPdb lacks required parameter");

			if (string.IsNullOrEmpty(info.FilePath) || !File.Exists(info.FilePath))
				return null;

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
			var infos = new List<GotoInfo>();
			var methods = new List<MethodBase>();

			try
			{
        int hr = projectInfo.SmartOpenScope.OpenScope(info.FilePath, 0, ref IID_MetaDataImport, out unkMetaDataImport);

				if (hr == VSConstants.S_OK)
				{
					ptrMetaDataImport = Marshal.GetIUnknownForObject(unkMetaDataImport);
					binder            = new SymBinder();
					reader            = binder.GetReader(ptrMetaDataImport, info.FilePath, null);
					documents         = new ISymbolDocument[1];
					lines             = new int[1];
					columns           = new int[1];
					endLines          = new int[1];
					endColumns        = new int[1];
					offsets           = new int[1];
				}
				else
				{
					Debug.WriteLineIf(TS.TraceWarning,
						string.Format("Failed to obtain MetaDataImport from VS, hr 0x{0:X8}", hr), TS.DisplayName);
					return null;
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
            Type t = (Type)info.MemberInfo;
						methods.AddRange(t.GetMethods(DeclaredMembers));
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
              if (File.Exists(path) && infos.Find(item => item.FilePath == path) == null)
							{
                infos.Add(new GotoInfo(path, new Nemerle.Compiler.Location(path, lines[0], columns[0], endLines[0], endColumns[0])));
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

			// No sources were found
			//
			if (infos.Count == 0)
				return null;

			// In case of a Method we already succeeded.
			//
      if (info.MemberInfo is MethodBase)
			{
				Debug.Assert(infos.Count < 2, "Partial method is not expected.");
				return infos.ToArray();
			}

			// TODO: implement other languages than Nemerle
			//
			if (_findExactLocation && infos[0].FilePath.EndsWith(NemerleConstants.FileExtension, StringComparison.OrdinalIgnoreCase))
			{
				try
				{
					// Quickly compile and find the _exact_ location(s)
					//

					Completion2.Engine e = NemerleLanguageService.DefaultEngine;

					Trace.Assert("не реализовано" == null);

					//TODO: VladD2: нужно отпарсить файл и найти там искомый элемент

					//foreach (GotoInfo gi in infos)
					//	e.Sources.AddOrUpdate(gi.FilePath, File.ReadAllText(gi.FilePath));

          GotoInfo[] exactInfo = e.Project.GetGotoInfo(info.MemberInfo);

					if (null != exactInfo && exactInfo.Length > 0)
						return exactInfo;
				}
				catch (Exception ex)
				{
					// This is definitelly unreachable, but since we have a backup route, it make sence.
					//
					Trace.WriteLineIf(TS.TraceError,
						string.Format("({0}) {1}", info.Member.Name, ex.Message), TS.DisplayName);
				}
				finally
				{
					// Nemerle.Compiler.ManagerClass.Instance is a singletone. Actually, it's a bug.
					// Quick'n'durty solution is 'save and revert when done'
					//
				}

				// Fall through to backup route
			}

			return infos.ToArray();
		}
	}
}
