using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.IO;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

using EnvDTE;
using Microsoft.VisualStudio.TextManager.Interop;

using CDCodeNamespace = System.CodeDom.CodeNamespace;
using VSCodeNamespace = EnvDTE.CodeNamespace;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using Nemerle.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Designer.Interfaces;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Shell.Interop;
using System.Runtime.InteropServices;
using System.Text;
using Nemerle.VisualStudio.LanguageService;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.FileCodeModel
{
	// Provides a FileCodeModel for out of project files.
	internal class NemerleSingleFileCodeModel : FileCodeModelBase
	{
		private CodeDomProvider _codeDomProvider;
		private DateTime _fileTime = DateTime.MinValue;
		private string _filePath;
		private IVsHierarchy _hierarchy;
		private uint _id;

		public NemerleSingleFileCodeModel(IVsHierarchy hierarchy, uint id, ProjectItem projectItem, CodeDomProvider codeDomProvider)
			: base(projectItem)
		{
			_codeDomProvider = codeDomProvider;
			_hierarchy = hierarchy;
			_id = id;

			EnvDTE.Property prop = ProjectItem.Properties.Item("FullPath");
			if (prop != null)
				_filePath = prop.Value as string;
		}

		protected override void Initialize()
		{
			var fileLastWriteTime = File.GetLastWriteTime(_filePath);

			// нет необходимости парсить файл повторно если документ не отмечен как измененный и дата его сохранения не изменилась
			if (fileLastWriteTime > _fileTime || ProjectItem.IsDirty || CompileUnit == null)
			{
				var content = ReadSourceFromBuffer();

				TextReader reader = null;

				try
				{
					reader = (content == null)
						? (TextReader)new StreamReader(_filePath, true)
						: (TextReader)new StringReader(content);

					CompileUnit = _codeDomProvider.Parse(reader);
					_fileTime = fileLastWriteTime;

					reader.Close();
				}
				finally
				{
					if (reader != null)
						reader.Dispose();
				}
			}
		}

		internal override void FlushChanges()
		{
			// NemerleSingleFileCodeModel don't support two-way code model
			throw new NotImplementedException();
		}

		// Считывает содержимое файла из буфера, связанного с документом. 
		// Такой способ позволяет получить еще не сохраненный на диск контент.
		private string ReadSourceFromBuffer()
		{
			IVsRunningDocumentTable rdt = NemerlePackage.GetGlobalService(typeof(IVsRunningDocumentTable)) as IVsRunningDocumentTable;

			if (rdt != null)
			{
				IEnumRunningDocuments documents;

				rdt.GetRunningDocumentsEnum(out documents);

				IntPtr documentData = IntPtr.Zero;
				uint[] docCookie = new uint[1];
				uint fetched;

				while ((Microsoft.VisualStudio.VSConstants.S_OK == documents.Next(1, docCookie, out fetched)) && (1 == fetched))
				{
					uint flags;
					uint editLocks;
					uint readLocks;
					string moniker;
					IVsHierarchy docHierarchy;
					uint docId;
					IntPtr docData = IntPtr.Zero;

					try
					{
						ErrorHandler.ThrowOnFailure(rdt.GetDocumentInfo(docCookie[0], out flags, out readLocks, out editLocks, out moniker, out docHierarchy, out docId, out docData));

						// Check if this document is the one we are looking for.
						if (docId == _id && _hierarchy.Equals(docHierarchy))
						{
							documentData = docData;
							docData = IntPtr.Zero;
							break;
						}
					}
					finally
					{
						if (docData != IntPtr.Zero)
							Marshal.Release(docData);
					}
				}

				if (documentData != IntPtr.Zero)
				{
					object obj = Marshal.GetObjectForIUnknown(documentData);
					IVsTextLines txtLines = obj as IVsTextLines;

					int totalLines;
					ErrorHandler.ThrowOnFailure(txtLines.GetLineCount(out totalLines));
					StringBuilder sb = new StringBuilder();

					for (int line = 0; line < totalLines; ++line)
					{
						int lineLen;
						ErrorHandler.ThrowOnFailure(txtLines.GetLengthOfLine(line, out lineLen));

						string lineText;
						ErrorHandler.ThrowOnFailure(txtLines.GetLineText(line, 0, line, lineLen, out lineText));

						sb.AppendLine(lineText);
					}

					return sb.ToString();
				}
			}

			return null;
		}
	}
}
