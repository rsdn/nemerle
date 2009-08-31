using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Diagnostics.SymbolStore;
using System.Windows.Forms;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Completion2;
using Nemerle.VisualStudio.Project;
using Nemerle.VisualStudio.GUI;

namespace Nemerle.VisualStudio.LanguageService
{
	/// <summary>
	/// Encapsulates information about the source as obtained from a parsing operation.
	/// </summary>
	class NemerleAuthoringScope : AuthoringScope
	{
		#region ctors

		public NemerleAuthoringScope(
			ProjectInfo      project,
			AuthoringSink    sink,
			CompletionElem[] overloadPossibility)
			: this(project, sink)
		{
			_overloadPossibility = overloadPossibility;
		}

		public NemerleAuthoringScope(ProjectInfo project, AuthoringSink sink, string filePath, ISource source)
			: this(project, sink)
		{
			_filePath   = filePath;
			_sourceText = source;
		}

		public NemerleAuthoringScope(ProjectInfo project, AuthoringSink sink, Methods methods)
			: this(project, sink)
		{
			_methods = methods;
		}

		public NemerleAuthoringScope(ProjectInfo project, AuthoringSink sink)
		{
			_project = project;
			_sink    = sink;
		}
 
		#endregion

		#region Fields
		
		private readonly AuthoringSink    _sink;
		private readonly CompletionElem[] _overloadPossibility;
		private readonly Methods          _methods;
		private readonly ProjectInfo      _project;
		private readonly string           _filePath;
		private readonly ISource          _sourceText;

		private static   Stream           _temporaryFileRef;

		// Configurable option:
		//
		// When true, all files recovered from the .pdb will be recompiled to find real location.
		// When false, the relevant method locations will be returned.
		//
		private readonly bool             _findExactLocation = true;

		// Configurable option:
		//
		// When true, a fake source with definitions will be generated
		//
		private readonly bool             _generateFakeSource = true;

		#endregion

		#region Constants

		private /*readonly*/ Guid   IID_MetaDataImport = new Guid("7DAC8207-D3AE-4c75-9B67-92801A497D44");
		private const BindingFlags  DeclaredMembers	= BindingFlags.DeclaredOnly
										| BindingFlags.Instance | BindingFlags.Static
										| BindingFlags.Public | BindingFlags.NonPublic;

		#endregion

		#region Overrides

		public override string GetDataTipText(int line, int col, out TextSpan span)
		{
      throw new NotImplementedException("this method should not be called!");
		}

		public override string Goto(
			VSConstants.VSStd97CmdID cmd,
			IVsTextView			  textView,
			int					  line,
			int					  col,
			out TextSpan			 span)
		{
			/*
			IVsUIShell shell = _project.ProjectNode.Package.GetService<IVsUIShell, SVsUIShell>();

			Guid		   guid = new Guid(ToolWindowGuids.ObjectSearchResultsWindow);
			IVsWindowFrame frame;

			shell.FindToolWindow(
				(uint)__VSFINDTOOLWIN.FTW_fForceCreate,
				ref guid,
				out frame);

			if (frame != null)
			{
				object obj;
				frame.GetProperty((int)__VSFPROPID.VSFPROPID_ExtWindowObject, out obj);

				obj.ToString();

				EnvDTE.Window window = (EnvDTE.Window)obj;

				guid = typeof(IVsObjectListOwner).GUID;
				IntPtr ptr;
				frame.QueryViewInterface(ref guid, out ptr);

				IVsObjectListOwner lst = Marshal.GetObjectForIUnknown(ptr) as IVsObjectListOwner;

				int isv = lst.IsVisible();

				lst.ClearCachedData((int)_VSOBJLISTOWNERCACHEDDATAKINDS.LOCDK_SELECTEDNAVINFO);

				guid = typeof(IVsObjectSearchPane).GUID;
				frame.QueryViewInterface(ref guid, out ptr);

				IVsObjectSearchPane pane = Marshal.GetObjectForIUnknown(ptr) as IVsObjectSearchPane;

				frame.Show();
			}
			*/


			span = new TextSpan();

			GotoInfo[] infos;

			switch (cmd)
			{
				case VSConstants.VSStd97CmdID.GotoDecl:
				case VSConstants.VSStd97CmdID.GotoDefn:
					infos = _project.GetGoto(_filePath, line, col, _sourceText);
					break;

				case VSConstants.VSStd97CmdID.GotoRef:
					infos = _project.GetUsages(_filePath, line, col, _sourceText);
					break;

				default:
					Trace.Fail(string.Format("{0}: Unknown cmd ({1})", TS.DisplayName, cmd));
					infos = null;
					break;
			}

			if (infos == null || infos.Length == 0)
			{
				// Not implemented nor expected
				//
				return null;
			}

			if (!infos[0].HasLocation && infos[0].Member != null)
			{
				Debug.Assert(infos.Length == 1, "Multiple unknown locations are unexpected");
				GotoInfo[] infoFromPdb = LookupLocationsFromPdb(infos[0]);

				if (infoFromPdb == null)
					return GenerateSource(ref span, infos[0]);

				infos = infoFromPdb;
			}

			if (infos.Length == 1)
				return SetTextSpan(ref span, infos[0]);
			else if (infos.Length > 0)
			{
				NativeWindow textEditorWnd = 
					NativeWindow.FromHandle(textView.GetWindowHandle());

				using (GotoUsageForm popup = new GotoUsageForm(infos))
					if (popup.ShowDialog(textEditorWnd) == DialogResult.OK)
						return SetTextSpan(ref span, popup.Result);
			}

			return null;
		}

		private string GenerateSource(ref TextSpan span, GotoInfo info)
		{
			if (_generateFakeSource == false)
				return null;

			Debug.Assert(info != null && info.Member != null, "GenerateSource lacks required parameter");

			MemberInfo mi = info.Member;
			Type type	 = (mi.MemberType == MemberTypes.TypeInfo || mi.MemberType == MemberTypes.NestedType)?
				(Type)mi: mi.DeclaringType;

			Debug.WriteLineIf(TS.TraceVerbose, string.Format("Generating source for ({0})", type.FullName), TS.DisplayName);

			string tempFileName = string.Format("{0}{1}${2}v{3}${4}.n", Path.GetTempPath(),
				type.FullName,Path.GetFileName(info.FilePath),
				type.Assembly.GetName().Version, Process.GetCurrentProcess().Id);

			// Delete previously generated file, if any.
			//
			if (_temporaryFileRef != null)
				_temporaryFileRef.Close();

			using (TextWriter w = new StreamWriter(tempFileName))
			{
				SetTextSpan(ref span, _project.Project.GenerateCode(type, info.Member, w));
			}

			// Made this file 'DeleteOnClose'.
			//
			_temporaryFileRef = new FileStream(tempFileName, FileMode.Open,
				FileAccess.Read, FileShare.ReadWrite, 4096, FileOptions.DeleteOnClose);

			// Mark this file is not editable.
			//
			File.SetAttributes(tempFileName, FileAttributes.ReadOnly | FileAttributes.Temporary);

			// Now Visual studio can open this file.
			//
			return tempFileName;
		}

		public override Declarations GetDeclarations(
			IVsTextView view, int line, int col, TokenInfo info, ParseReason reason)
		{
			return new NemerleDeclarations(_overloadPossibility ?? new CompletionElem[0]);
		}

		public override Methods GetMethods(int line, int col, string name)
		{
			return _methods;
		}

		#endregion

		#region Implementation

		private GotoInfo[] LookupLocationsFromPdb(GotoInfo info)
		{
			Debug.Assert(info != null && info.Member != null, "LookupLocationsFromPdb lacks required parameter");

			if (string.IsNullOrEmpty(info.FilePath) || !File.Exists(info.FilePath))
				return null;

			object            unkMetaDataImport;
			IntPtr            ptrMetaDataImport = IntPtr.Zero;
			ISymbolBinder1    binder;
			ISymbolReader     reader;
			ISymbolDocument[] documents;
			int[]             lines;
			int[]             columns;
			int[]             endLines;
			int[]             endColumns;
			int[]             offsets;
			List<GotoInfo>    infos   = new List<GotoInfo>();
			List<MethodBase>  methods = new List<MethodBase>();

			try
			{
				int hr = _project.SmartOpenScope.OpenScope(info.FilePath, 0, ref IID_MetaDataImport, out unkMetaDataImport);

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

				switch (info.Member.MemberType)
				{
					case MemberTypes.Constructor:
					case MemberTypes.Method:
						MethodBase mb = (MethodBase) info.Member;

						// Abstract methods does not contain any code.
						//
						if (mb.IsAbstract)
							methods.AddRange(mb.DeclaringType.GetMethods(DeclaredMembers));
						else
							methods.Add(mb);
						break;

					case MemberTypes.Property:
						PropertyInfo pi = (PropertyInfo) info.Member;
						methods.AddRange(pi.GetAccessors(true));
						break;

					case MemberTypes.Field:
						methods.AddRange(info.Member.DeclaringType.GetMethods(DeclaredMembers));
						break;

					case MemberTypes.Event:
						EventInfo ei = (EventInfo) info.Member;
						methods.Add(ei.GetAddMethod   (true));
						methods.Add(ei.GetRemoveMethod(true));
						methods.Add(ei.GetRaiseMethod (true));
						methods.AddRange(ei.GetOtherMethods(true));
						break;

					case MemberTypes.TypeInfo:
					case MemberTypes.NestedType:
						Type t = (Type)info.Member;
						methods.AddRange(t.GetMethods(DeclaredMembers));
						break;
					default:
						Trace.Fail("Unexpected MemberType " + info.Member.MemberType);
						break;
				}

				foreach (MethodBase mb in methods)
				{
					if (mb == null || Attribute.GetCustomAttribute(mb, typeof(CompilerGeneratedAttribute)) != null)
						continue;

					try
					{
						SymbolToken   token  = new SymbolToken(mb.MetadataToken);
						ISymbolMethod method = reader.GetMethod(token);

						if (method.SequencePointCount > 0)
						{
							method.GetSequencePoints(offsets, documents, lines, columns, endLines, endColumns);

							// We are interested in unique files only.
							//
							if (File.Exists(documents[0].URL) &&
								infos.Find(item => item.FilePath == documents[0].URL) == null)
							{
								infos.Add(new GotoInfo(documents[0].URL, new Nemerle.Compiler.Location(0, lines[0], columns[0], endLines[0], endColumns[0])));
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
			if (info.Member is MethodBase)
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
					
					Completion2.Engine          e = NemerleLanguageService.DefaultEngine;

          Trace.Assert("не реализовано" == null);

          //TODO: VladD2: 

					//foreach (GotoInfo gi in infos)
					//	e.Sources.AddOrUpdate(gi.FilePath, File.ReadAllText(gi.FilePath));

					GotoInfo[] exactInfo = e.Project.GetGotoInfo(info.Member);

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
					_project.Engine.RestoreManagerClassInstance();
				}

				// Fall through to backup route
			}

			return infos.ToArray();
		}

		private static string SetTextSpan(ref TextSpan span, GotoInfo result)
		{
			Utils.CopyLocationToSpan(result.Location, ref span);
			return result.FilePath;
		}

		#endregion

		#region Debug

		private static TraceSwitch _ts;
		private static TraceSwitch  TS
		{
			get
			{
				return _ts ?? (_ts = new TraceSwitch("NemerleAuthoringScope",
					"Enables Nemerle.VisualStudio.LanguageService.NemerleAuthoringScope trace"));
			}
		}

		#endregion
	}
}
