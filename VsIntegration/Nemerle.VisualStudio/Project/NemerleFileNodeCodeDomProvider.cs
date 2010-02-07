using System;
using System.IO;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Compiler;
using NCU = Nemerle.Compiler.Utils;
using TupleStringIntInt = Nemerle.Builtins.Tuple<string, int, int>;
using CodeGenerator = Nemerle.Compiler.Utils.FormCodeDomGenerator;

using Nemerle.VisualStudio.LanguageService;
using System.Diagnostics;
using Nemerle.VisualStudio.Helpers;
using Microsoft.VisualStudio.Shell.Design.Serialization;
using Nemerle.Compiler.Parsetree;

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
		readonly FileNode         _fileNode;
		NCU.FormCodeDomParser     _codeDomParser;
		NCU.FormCodeDomGenerator  _codeDomGenerator;
		NemerleCodeGeneratorProxy _codeGenProxy;

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
			// codeStream  используется для получения DesignerDocDataService...
			string mainFilePath = PathOfMainFile();

			ProjectInfo projectInfo = ProjectInfo.FindProject(mainFilePath);

			if (projectInfo != null)
			{
				var source          = projectInfo.GetSource(mainFilePath);
				var provider        = (IServiceProvider)codeStream;
				var docDataService  = (DesignerDocDataService)provider.GetService(typeof(DesignerDocDataService));

				var result          = projectInfo.Engine.CreateCodeCompileUnit(source);

				var codeCompileUnit = result.CodeCompileUnit;

				// Дизайнер форм должен следить за изменением файлов в которых расположен
				// класс формы. Чтобы он знал за какакими файлами нужно следить, информацию 
				// о них нужно запихать в RelatedDocDataCollection и поместить ссылку на ее
				// в codeCompileUnit.UserData[typeof(RelatedDocDataCollection)].
				var relatedDocDatas = new RelatedDocDataCollection();
				var sourcesInf = new List<TupleStringIntInt>();

				foreach (int index in result.FilesIndices)
				{
					var filePath     = Location.GetFileName(index);
					var data         = docDataService.GetFileDocData(filePath, FileAccess.Read, null);
					relatedDocDatas.Add(data);

					var textVerIndex = projectInfo.GetSource(index).GetTextCurrentVersionAndFileIndex();
					sourcesInf.Add(textVerIndex);
				}

				codeCompileUnit.UserData[typeof(RelatedDocDataCollection)] = relatedDocDatas;
				codeCompileUnit.UserData["NemerleSources"] = sourcesInf;

				return codeCompileUnit;
			}
			else
				return null;
		}
		
		#endregion

		#region Generator implementation

		public override void GenerateCodeFromCompileUnit(CodeCompileUnit codeCompileUnit, TextWriter writer, CodeGeneratorOptions options)
		{
			if (options == null)
				options = GetCodeGeneratorOptions();

      ProjectInfo projectInfo = ProjectInfo.FindProject(PathOfMainFile());

      if (projectInfo == null)
        throw new ApplicationException("The component is not in the project!");

      var changes = projectInfo.Engine.MergeCodeCompileUnit(codeCompileUnit);
			var sourcesInf = (List<TupleStringIntInt>)codeCompileUnit.UserData["NemerleSources"];

			// Для упрощения реализации генерации кода при генерации кода не происходит 
			// обновление сгенерированных ранее веток CodeDom-а. Однако дизайнер форм может 
			// вызывать генерацию кода по CodeDom-у множество раз. При этом он не пытается перечитать
			// содержимое формы, если только пользователь вручную не изменим файлы в которые 
			// сериализуется форма. При первой сериалзиции фалы хранящие код формы изменяются,
			// и при повтоной попытке серилизовать CodeDom формы элементы CodeDom-а будут 
			// указывать на неверные позиции, так как предыдущая сериализация изменила код.
			// Чтобы не мучиться с синхронизацией перед сериализацией просто востанавливается 
			// состояние (которое было на момент генерации CodeDom-элементов) файлов содержащих код 
			// формы. 
			// Если один из файлов будет изменен пользователем, то CodeDom будет автоматически пересоздан.
			// Таким образом изменение файла могут быть вызваны только сериализацией CodeDom-а в код.
			foreach (var si in sourcesInf)
			{
				var fileIndex = si.Field2;
				var fileVertion = si.Field1;
				var code = si.Field0;
				var source = projectInfo.GetEditableSource(fileIndex, WindowFrameShowAction.DoNotShow);
				if (source.CurrentVersion != fileVertion)
					source.SetText(code); // файл изменился с момента генерации по нему CodeDom-а! Восстанавливаем его.
			}

			using (var helper = new NemerleProjectSourcesButchEditHelper(projectInfo, "form designer update"))
			{
				var text = CodeGenerator.ToString(changes.NewInitializeComponentStatements);
				// обновляем исходники...
				var initializeComponent = changes.InitializeComponent;
				helper.ReplaseMethodBody(initializeComponent, text);

				var mainFilePath = PathOfMainFile();
				var mainFileIndex = Location.GetFileIndex(mainFilePath);
				var ty = initializeComponent.DefinedIn.TypeBuilder;
				var x = new TopDeclaration[0];
				var mainPart = ty.AstParts.First(td => td.Location.FileIndex == mainFileIndex);

				foreach (CodeMemberField codeMemberField in changes.InsertedFields)
					helper.AddField(initializeComponent.DefinedIn, codeMemberField);

				foreach (CodeMemberMethod codeMemberMethod in changes.InsertedMethods)
					helper.AddMethod(mainPart, codeMemberMethod, changes.Declaration);

				foreach (ClassMember.Field field in changes.DelitedFields)
					helper.RemoveField(field);

				helper.ApplyEdits();

				var relatedDocDatas = (RelatedDocDataCollection)codeCompileUnit.UserData[typeof(RelatedDocDataCollection)];
				foreach (DocData docData in relatedDocDatas)
				{
					docData.Modify();
					break;
				}
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
