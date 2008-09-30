using System;
using System.IO;
using System.CodeDom;
using System.CodeDom.Compiler;

using System.Runtime.InteropServices;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Compiler;
using NCU = Nemerle.Compiler.Utils;

using Nemerle.VisualStudio.LanguageService;

namespace Nemerle.VisualStudio.Project
{
	internal class NemerleCodeGeneratorProxy : NemerleCodeGenerator, ICodeGenerator
	{
		NemerleFileNodeCodeDomProvider _fileNodeProvider; // save this hook

		public NemerleCodeGeneratorProxy(NemerleFileNodeCodeDomProvider fileNodeProvider)
		{
			_fileNodeProvider = fileNodeProvider;
		}

		public void GenerateCodeFromCompileUnit(CodeCompileUnit e, TextWriter w, CodeGeneratorOptions o)
		{
			_fileNodeProvider.GenerateCodeFromCompileUnit(e,w,o);
		}

		public void GenerateCodeFromExpression(CodeExpression e, TextWriter w, CodeGeneratorOptions o)
		{
			//(base as ICodeGenerator).GenerateCodeFromExpression(e, w, o);
			throw new NotImplementedException("GenerateCodeFromExpression");
		}

		public void GenerateCodeFromNamespace(CodeNamespace e, TextWriter w, CodeGeneratorOptions o)
		{
			//base.GenerateCodeFromNamespace(e, w, o);
			throw new NotImplementedException("GenerateCodeFromNamespace");
		}

		public void GenerateCodeFromStatement(CodeStatement e, TextWriter w, CodeGeneratorOptions o)
		{
			//base.GenerateCodeFromStatement(e, w, o);
			throw new NotImplementedException("GenerateCodeFromStatement");
		}

		public void GenerateCodeFromType(CodeTypeDeclaration e, TextWriter w, CodeGeneratorOptions o)
		{
			//base.GenerateCodeFromType(e, w, o);
			throw new NotImplementedException("GenerateCodeFromType");
		}
	}

	internal class NemerleFileNodeCodeDomProvider : NemerleCodeProvider, ICodeParser
		// ICodeGenerator
	{
		readonly FileNode			_fileNode;
		NCU.FormCodeDomParser		_codeDomParser;
		NCU.FormCodeDomGenerator	_codeDomGenerator;
		NemerleCodeGeneratorProxy	_codeGenProxy;

		private void Init()
		{
			_codeDomParser = new NCU.FormCodeDomParser();
			_codeDomGenerator = new NCU.FormCodeDomGenerator();
			_codeGenProxy = new NemerleCodeGeneratorProxy(this);
		}

		// AKhropov: In fact these 2 constructors only to restrict possible file nodes,
		//           could be simply NemerleFileCodeDomProvider(FileNode fileNode)

		internal NemerleFileNodeCodeDomProvider(NemerleFileNode fileNode)
		{
			_fileNode = fileNode;
			Init();
		}

		internal NemerleFileNodeCodeDomProvider(NemerleDependentFileNode fileNode)
		{
			_fileNode = fileNode;
			Init();
		}

		#region helper functions

		private bool IsFormSubType
		{
			get { return _fileNode.HasDesigner; }
		}

		private string PathOfMainFile()
		{
			return Path.Combine(Path.GetDirectoryName(_fileNode.GetMkDocument()), _fileNode.FileName);
		}

		private string PathOfDesignerFile()
		{
			return Path.Combine(Path.GetDirectoryName(_fileNode.GetMkDocument()),
				Path.GetFileNameWithoutExtension(_fileNode.FileName) +
					".Designer" + NemerleConstants.FileExtension);
		}

		// AKhropov : I had to grab some code from FileNode implementation because it was protected there
		void UpdateGeneratedCodeFile(string data, string filePath)
		{
			IVsRunningDocumentTable rdt = _fileNode.ProjectMgr.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;

			// (kberes) Shouldn't this be an InvalidOperationException instead with some not to annoying errormessage to the user?
			if (rdt == null)
			{
				ErrorHandler.ThrowOnFailure(VSConstants.E_FAIL);
			}

			IVsHierarchy hier;
			uint itemid, cookie;
			IntPtr docData = IntPtr.Zero;
			//Getting a edit lock on the document. Must be released later.
			ErrorHandler.ThrowOnFailure(rdt.FindAndLockDocument((uint)(_VSRDTFLAGS.RDT_EditLock), filePath, out hier, out itemid, out docData, out cookie));
			if (docData != IntPtr.Zero)
			{
				IVsPersistDocData persistDocData = Marshal.GetObjectForIUnknown(docData) as IVsPersistDocData;
				Marshal.Release(docData);

				try
				{
					// Try to get the Text lines
					IVsTextLines srpTextLines = persistDocData as IVsTextLines;
					if (srpTextLines == null)
					{
						// Try getting a text buffer provider first
						IVsTextBufferProvider srpTextBufferProvider = persistDocData as IVsTextBufferProvider;
						if (srpTextBufferProvider != null)
						{
							ErrorHandler.ThrowOnFailure(srpTextBufferProvider.GetTextBuffer(out srpTextLines));
						}
						// TODO : handle null case
					}

					int endLine, endIndex;
					srpTextLines.GetLastLineIndex(out endLine, out endIndex);

					// Lock the buffer before changing its content.
					ErrorHandler.ThrowOnFailure(srpTextLines.LockBuffer());
					try
					{
						GCHandle handle = GCHandle.Alloc(data, GCHandleType.Pinned);
						try
						{
							TextSpan[] span = new TextSpan[1];
							ErrorHandler.ThrowOnFailure(srpTextLines.ReplaceLines(0, 0, endLine, endIndex, handle.AddrOfPinnedObject(), data.Length, span));
						}
						finally
						{
							// Free the memory.
							handle.Free();
						}
					}
					finally
					{
						// Make sure that the buffer is unlocked also in case of exception.
						srpTextLines.UnlockBuffer();
					}
				}
				finally
				{
					ErrorHandler.ThrowOnFailure(rdt.UnlockDocument((uint)(_VSRDTFLAGS.RDT_ReadLock | _VSRDTFLAGS.RDT_Unlock_NoSave), cookie));
				}
			}
			else
			{
				using (StreamWriter sw = new StreamWriter(filePath,false))
				{
					sw.Write( data );
				}

				EnvDTE.ProjectItem projectItem = _fileNode.GetAutomationObject() as EnvDTE.ProjectItem;
				if (projectItem != null && (_fileNode.ProjectMgr.FindChild(_fileNode.FileName) == null))
				{
					projectItem.ProjectItems.AddFromFile(filePath);
				}
			}
		}

		#endregion

		#region Parser implementation

		public override CodeCompileUnit Parse(TextReader codeStream)
		{
			// AKhropov - in fact codeStream is ignored for now
			string mainFilePath = PathOfMainFile();

			ProjectInfo projectInfo = ProjectInfo.FindProject(mainFilePath);

			if (projectInfo != null)
			{
				// TODO : can _project change for _fileNode?
				return _codeDomParser.CreateCodeCompileUnit(
					projectInfo.Project, mainFilePath,
					(IsFormSubType) ? PathOfDesignerFile() : null);
			}
			else
				return null;
		}
		
		#endregion

		#region Generator implementation

		public override void GenerateCodeFromCompileUnit(CodeCompileUnit e, TextWriter w, CodeGeneratorOptions o)
		{
			if (o == null)
				o = GetCodeGeneratorOptions();

			if (IsFormSubType)
			{
				string mainFilePath = PathOfMainFile();
				string designerFilePath = PathOfDesignerFile();

				// Find designer FileNode
				NemerleDependentFileNode designerFileNode = 
					_fileNode.FindChild(designerFilePath) as NemerleDependentFileNode;

				if (designerFileNode == null)
					throw new ApplicationException("Can't find designer file node");

				// Distribute changes to Form.n and Form.designer.n files
				using (RDTFileTextMerger mainMerger = new RDTFileTextMerger(_fileNode))
				using (RDTFileTextMerger designerMerger = new RDTFileTextMerger(designerFileNode))
				{
					//ProjectInfo.FindProject(designerFilePath).Project.CompileUnits.
					ProjectInfo projectInfo = ProjectInfo.FindProject(mainFilePath);
					if (projectInfo == null)
						throw new ApplicationException("The component is not in the project!");

					_codeDomGenerator.MergeFormCodeFromCompileUnit(
						projectInfo.Project,
						mainFilePath, designerFilePath,
						e, mainMerger, designerMerger, o);
				}
			}
			else
				using (StringWriter sw = new StringWriter())
				{
					(_codeDomGenerator as ICodeGenerator).GenerateCodeFromCompileUnit(e, sw, o);

					UpdateGeneratedCodeFile(sw.ToString(), PathOfMainFile());
				}
		}

		private CodeGeneratorOptions GetCodeGeneratorOptions()
		{
			NemerleLanguageService langService = (_fileNode.GetService(typeof(NemerleLanguageService)) as NemerleLanguageService);

			if(langService == null)
				throw new ApplicationException("Language service not found");

			CodeGeneratorOptions codeGenOptions = new CodeGeneratorOptions();

			LanguagePreferences prefs = langService.GetLanguagePreferences();

			if(prefs.InsertTabs)
				codeGenOptions.IndentString = "\t";
			else
				codeGenOptions.IndentString = new string(' ', prefs.IndentSize );

			// Set deliberately (TODO)
			codeGenOptions.BlankLinesBetweenMembers = true;
			codeGenOptions.BracingStyle = "Block";
			codeGenOptions.ElseOnClosing = false;

			return codeGenOptions;
		}
		
		#endregion

		#region Provided (obsolete) interfaces

		[Obsolete("Callers should not use the ICodeParser interface.")]
		public override ICodeParser CreateParser()
		{
			return this;
		}

		[Obsolete("Callers should not use the ICodeGenerator interface.")]
		public override ICodeGenerator CreateGenerator()
		{
			return _codeGenProxy;
		}

		[Obsolete("Callers should not use the ICodeCompiler interface.")]
		public override ICodeCompiler CreateCompiler()
		{
			/* TODO - maybe return base.CreateCompiler();
			   but now doesn't make much sense
			*/

			throw new NotImplementedException();
			//return null;
		}
		

		#endregion

		#region Overridden properties

		public override string FileExtension
		{
			// TODO: Extension with '.' ?
			get { return NemerleConstants.FileExtension; }
		}

		#endregion
	}
}
