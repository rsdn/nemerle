using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Builtins;
using Nemerle.Compiler;
using Nemerle.Completion2;

using Nemerle.VisualStudio.Project;

namespace Nemerle.VisualStudio.LanguageService
{
	public class NemerleScanner : IScanner
	{
		public NemerleScanner(NemerleLanguageService languageService, IVsTextLines buffer)
		{
			_languageService = languageService;
			_buffer		  = buffer;
		}

		private readonly	NemerleLanguageService _languageService;
		private readonly	IVsTextLines		   _buffer;
		internal			int					_currentLine = -1;
		internal			TokenColor			 _lastColor;
		internal			bool				   _colorizeEnd;
		internal			NemerleSource		  _source;

		private  ScanLexer _lexer;
		internal ScanLexer GetLexer()
		{
			if (_lexer == null)
			{
				Source source = _languageService.GetSource(_buffer);

				if (source == null)
					return null;

				ProjectInfo projectInfo = ProjectInfo.FindProject(source.GetFilePath());

				Engine engine;

				if (projectInfo == null)
				{
					// TODO: We need to create hidden project for files which are not included
					// in any project.
					engine = new Engine(EngineCallbackStub.Default, 
						new ProjectManager(_languageService), new TraceWriter());
					object tmp = engine.Project;
				}
				else
					engine = projectInfo.Engine;

				_lexer = new ScanLexer(engine);
			}

			return _lexer;
		}

		public bool ScanTokenAndProvideInfoAboutIt(TokenInfo tokenInfo, ref int state)
		{
			if (_lexer == null)
				return false;

			ScanTokenInfo info = _lexer.GetToken((ScanState)state);

			state		= (int)info.State;

			_lastColor   = (TokenColor)info.Color;
			_colorizeEnd = info.ColorizeEnd;

			tokenInfo.Color	  = _lastColor;
			tokenInfo.Type	   = (TokenType)	info.Type;
			tokenInfo.Trigger	= (TokenTriggers)info.Triggers;
			tokenInfo.StartIndex = info.Token.Location.Column	- 1;
			tokenInfo.EndIndex   = info.Token.Location.EndColumn - 2;

			return !info.IsEndOfLine;
		}

		Nemerle.Completion2.Project _prevProject;
		GlobalEnv				   _env;
		TypeBuilder				 _type;
		int						 _envStartLine;
		int						 _envEndLine = -1;

		public void SetSource(string source, int offset)
		{
			//System.Diagnostics.Debug.WriteLine(string.Format("Scan line {0}", _currentLine));

			ScanLexer lexer = GetLexer();

			if (lexer != null)
			{
				if (_currentLine >= 0 && source.Length > 0)
				{
					if (_source != null && _source.ProjectInfo != null)
					{
#pragma warning disable 618 // Obsolete
						Nemerle.Completion2.Project project = _source.ProjectInfo.Engine.RawProject;
#pragma warning restore 618

						// Project is set to null after it's recompiled.
						//
						if (_prevProject != project)
						{
							_prevProject = project;
							_envEndLine  = -1;
						}

						// kliss: somehow it works wrong way. I disable it for a while. Let's see what happens
						//if (_currentLine < _envStartLine || _currentLine > _envEndLine)
						{
							Tuple<GlobalEnv, TypeBuilder, int, int> ret =
								project.GetActiveEnv(_source.FileIndex, _currentLine + 1);

							_env		  = ret.Field0;
							_type		 = ret.Field1;
							_envStartLine = ret.Field2 - 1;
							_envEndLine   = ret.Field3 - 1;
						}
					}
				}

				lexer.SetLine(_currentLine + 1, source, offset, _env, _type);
			}
		}
	}
}
