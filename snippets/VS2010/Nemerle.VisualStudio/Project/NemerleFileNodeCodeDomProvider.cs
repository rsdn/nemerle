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
using Nemerle.Completion2;
using System.Threading;
using Nemerle.Completion2.CodeFormatting;

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
				DesignerDocDataService docDataService = null;

				// IServiceProvider передается только при вызове метода Parse() дизайнером Windows Forms
				if (codeStream is IServiceProvider)
				{
					var provider = (IServiceProvider)codeStream;
					docDataService = (DesignerDocDataService)provider.GetService(typeof(DesignerDocDataService));
				}

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

					if (docDataService != null)
					{
						var data = docDataService.GetFileDocData(filePath, FileAccess.Read, null);
						relatedDocDatas.Add(data);
					}

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

			var indentInfo = ToIndentInfo(projectInfo.LanguageService.Preferences);

			using (var helper = new NemerleProjectSourcesButchEditHelper(projectInfo, "form designer update", indentInfo))
			{
				var definedIn = changes.Declaration.UserData["Member"] as TopDeclaration;
				var typeBuilder = definedIn.TypeBuilder;

				var mainFilePath = PathOfMainFile();
				var mainFileIndex = Location.GetFileIndex(mainFilePath);
				var mainPart = typeBuilder.AstParts.First(td => td.Location.FileIndex == mainFileIndex);

				var cls = changes.Class;

				if (changes.InitializeComponent != null)
				{
					var text = CodeGenerator.ToString(changes.NewInitializeComponentStatements);
					// update InitializeComponent() body...
					helper.ReplaseMethodBody(changes.InitializeComponent, text);
				}

				foreach (CodeMemberField codeMemberField in changes.InsertedFields)
					helper.AddField(cls, codeMemberField);

				foreach (CodeMemberMethod codeMemberMethod in changes.InsertedMethods)
					helper.AddMethod(mainPart, codeMemberMethod, changes.Declaration);

				foreach (ClassMember.Field field in changes.DelitedFields)
					helper.RemoveField(field);

				helper.ApplyEdits();

				var relatedDocDatas = (RelatedDocDataCollection)codeCompileUnit.UserData[typeof(RelatedDocDataCollection)];
				if (relatedDocDatas != null)
				{
					foreach (DocData docData in relatedDocDatas)
					{
						docData.Modify();
						break;
					}
				}
			}
		}

		public static IndentInfo ToIndentInfo(LanguagePreferences languagePreferences)
		{
			return new IndentInfo(languagePreferences.InsertTabs, languagePreferences.IndentSize, languagePreferences.TabSize);
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
			get { return NemerleConstants.FileExtension; }
		}

		#endregion
	}
}
