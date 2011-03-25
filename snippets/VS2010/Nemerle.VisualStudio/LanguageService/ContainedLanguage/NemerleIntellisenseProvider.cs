using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using VSConstants = Microsoft.VisualStudio.VSConstants;

using Nemerle.VisualStudio.FileCodeModel;
using Nemerle.VisualStudio.Project;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.LanguageService
{
	/// <summary>
	/// This class implements an intellisense provider for Web projects.
	/// An intellisense provider is a specialization of a language service where it is able to
	/// run as a contained language. The main language service and main editor is usually the
	/// HTML editor / language service, but a specific contained language is hosted for fragments
	/// like the "script" tags.
	/// This object must be COM visible because it will be Co-Created by the host language.
	/// </summary>
	[ComVisible(true)]
	[Guid(NemerleConstants.IntellisenseProviderGuidString)]
	public sealed class NemerleIntellisenseProvider : IVsIntellisenseProject, IDisposable
	{
		private IVsIntellisenseProjectHost hostProject;
		private NemerleContainedLanguageFactory languageFactory;
		private List<string> references;
		private Dictionary<uint, NemerleSourceFileInfo> files;
		private CodeDomProvider nemerleProvider;

		public NemerleIntellisenseProvider()
		{
			System.Diagnostics.Debug.WriteLine("\n\tNemerleIntellisenseProvider created\n");
			references = new List<string>();
			files = new Dictionary<uint, NemerleSourceFileInfo>();
		}

		public int AddAssemblyReference(string bstrAbsPath)
		{
			string path = bstrAbsPath.ToUpper(CultureInfo.CurrentCulture);
			if (!references.Contains(path))
			{
				references.Add(path);

				//if (null != pythonProvider)
				//{
				//    pythonProvider.AddReference(path);
				//}
			}
			return VSConstants.S_OK;
		}

		public void Dispose()
		{
			Dispose(true);
		}

		private void Dispose(bool disposing)
		{
			if (disposing)
			{
				Close();
			}
			GC.SuppressFinalize(this);
		}

		public int AddFile(string bstrAbsPath, uint itemid)
		{
			if (VSConstants.S_OK != IsCompilableFile(bstrAbsPath))
			{
				return VSConstants.S_OK;
			}
			if (!files.ContainsKey(itemid))
			{
				files.Add(itemid, new NemerleSourceFileInfo(bstrAbsPath, itemid));
			}
			return VSConstants.S_OK;
		}

		public int AddP2PReference(object pUnk)
		{
			// Not supported.
			return VSConstants.E_NOTIMPL;
		}

		public int Close()
		{
			this.hostProject = null;
			if (null != nemerleProvider)
			{
				nemerleProvider.Dispose();
				nemerleProvider = null;
			}
			return VSConstants.S_OK;
		}

		public int GetCodeDomProviderName(out string pbstrProvider)
		{
			pbstrProvider = NemerleConstants.LanguageName;
			return VSConstants.S_OK;
		}

		public int GetCompilerReference(out object ppCompilerReference)
		{
			ppCompilerReference = null;
			return VSConstants.E_NOTIMPL;
		}

		public int GetContainedLanguageFactory(out IVsContainedLanguageFactory ppContainedLanguageFactory)
		{
			if (null == languageFactory)
			{
				languageFactory = new NemerleContainedLanguageFactory(this);
			}
			ppContainedLanguageFactory = languageFactory;
			return VSConstants.S_OK;
		}

		public int GetExternalErrorReporter(out IVsReportExternalErrors ppErrorReporter)
		{
			// TODO: Handle the error reporter
			ppErrorReporter = null;
			return VSConstants.E_NOTIMPL;
		}

		public uint GetProjectItemId(string filePath)
		{
			foreach (NemerleSourceFileInfo sourceInfo in files.Values)
			{
				if (0 == string.Compare(sourceInfo.Name, filePath, StringComparison.OrdinalIgnoreCase))
				{
					return sourceInfo.ItemId;
				}
			}

			return 0;
		}

		public EnvDTE.ProjectItem GetProjectItem(string filePath)
		{
			NemerleSourceFileInfo fileInfo = null;
			foreach (NemerleSourceFileInfo sourceInfo in files.Values)
			{
				if (0 == string.Compare(sourceInfo.Name, filePath, StringComparison.OrdinalIgnoreCase))
				{
					fileInfo = sourceInfo;
					break;
				}
			}

			if (null == fileInfo)
			{
				throw new System.InvalidOperationException();
			}

			fileInfo.HostProject = hostProject;
			fileInfo.CodeProvider = CodeDomProvider;
			return fileInfo.ProjectItem;
		}

		public int GetFileCodeModel(object pProj, object pProjectItem, out object ppCodeModel)
		{
			ppCodeModel = null;

			EnvDTE.ProjectItem projectItem = pProjectItem as EnvDTE.ProjectItem;
			if (null == projectItem)
			{
				throw new System.ArgumentException();
			}

			EnvDTE.Property prop = projectItem.Properties.Item("FullPath");
			if (null == prop)
			{
				throw new System.InvalidOperationException();
			}

			string itemPath = prop.Value as string;
			if (string.IsNullOrEmpty(itemPath))
			{
				throw new System.InvalidOperationException();
			}

			foreach (NemerleSourceFileInfo sourceInfo in files.Values)
			{
				if (0 == string.Compare(sourceInfo.Name, itemPath, StringComparison.OrdinalIgnoreCase))
				{
					ppCodeModel = (object)sourceInfo.FileCodeModel;
					break;
				}
			}

			if (ppCodeModel == null)
			{
				throw new System.InvalidOperationException();
			}

			return VSConstants.S_OK;
		}

		public int GetProjectCodeModel(object pProj, out object ppCodeModel)
		{
			ppCodeModel = null;
			return VSConstants.E_NOTIMPL;
		}

		public int Init(IVsIntellisenseProjectHost pHost)
		{
			this.hostProject = pHost;
			return VSConstants.S_OK;
		}

		public int IsCompilableFile(string bstrFileName)
		{
			string ext = System.IO.Path.GetExtension(bstrFileName);
			if (0 == string.Compare(ext, NemerleConstants.FileExtension, StringComparison.OrdinalIgnoreCase))
			{
				return VSConstants.S_OK;
			}
			return VSConstants.S_FALSE;
		}

		public int IsSupportedP2PReference(object pUnk)
		{
			// P2P references are not supported.
			return VSConstants.S_FALSE;
		}

		public int IsWebFileRequiredByProject(out int pbReq)
		{
			pbReq = 0;
			return VSConstants.S_OK;
		}

		public int RefreshCompilerOptions()
		{
			return VSConstants.S_OK;
		}

		public int RemoveAssemblyReference(string bstrAbsPath)
		{
			string path = bstrAbsPath.ToUpper(CultureInfo.CurrentCulture);
			if (references.Contains(path))
			{
				references.Remove(path);
				nemerleProvider = null;
			}
			return VSConstants.S_OK;
		}

		public int RemoveFile(string bstrAbsPath, uint itemid)
		{
			if (files.ContainsKey(itemid))
			{
				files.Remove(itemid);
			}
			return VSConstants.S_OK;
		}

		public int RemoveP2PReference(object pUnk)
		{
			return VSConstants.S_OK;
		}

		public int RenameFile(string bstrAbsPath, string bstrNewAbsPath, uint itemid)
		{
			return VSConstants.S_OK;
		}

		public int ResumePostedNotifications()
		{
			// No-Op
			return VSConstants.S_OK;
		}

		public int StartIntellisenseEngine()
		{
			// No-Op
			return VSConstants.S_OK;
		}

		public int StopIntellisenseEngine()
		{
			// No-Op
			return VSConstants.S_OK;
		}

		public int SuspendPostedNotifications()
		{
			// No-Op
			return VSConstants.S_OK;
		}

		public int WaitForIntellisenseReady()
		{
			// No-Op
			return VSConstants.S_OK;
		}

		internal CodeDomProvider CodeDomProvider
		{
			get
			{
				if (null == nemerleProvider)
				{
					nemerleProvider = new Nemerle.Compiler.Utils.NemerleCodeDomProvider();

					//foreach (string assembly in references)
					//{
					//    pythonProvider.AddReference(assembly);
					//}
				}
				return nemerleProvider;
			}
		}
	}
}
