using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;

using Nemerle.Compiler;
using Nemerle.Completion2;

using Nemerle.VisualStudio.Project;
using System;
using System.Diagnostics;

namespace Nemerle.VisualStudio.LanguageService
{
	public class NemerleScanner : IScanner
	{
		public NemerleScanner(NemerleLanguageService languageService, IVsTextLines buffer)
		{
			_languageService = languageService;
			_buffer		  = buffer;
		}

		private readonly	NemerleLanguageService  _languageService;
		private readonly	IVsTextLines		        _buffer;
		internal			    int					            _currentLine = -1;
		internal			    TokenColor			        _lastColor;
		internal			    bool				            _colorizeEnd;
		internal			    NemerleSource		        _source;
		private           ScanLexer               _lexer;

		internal ScanLexer GetNewLexer()
		{
			var source = (NemerleSource)_languageService.GetSource(_buffer);

			if (source == null)
				source = _source;
			if (source == null)
				return null;

			var engine = source.GetEngine();

			if (engine.RequestOnInitEngine())
				return new ScanLexer((ManagerClass)engine);
			else
				return null;
		}

		internal ScanLexer GetLexer()
		{
			if (_lexer == null)
				_lexer = GetNewLexer();

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

			tokenInfo.Color	     = _lastColor;
			tokenInfo.Type	     = (TokenType)	info.Type;
			tokenInfo.Trigger	   = (TokenTriggers)info.Triggers;
			tokenInfo.StartIndex = info.Token.Location.Column	- 1;
			tokenInfo.EndIndex   = info.Token.Location.EndColumn - 2;

			return !info.IsEndOfLine;
		}

		GlobalEnv				   _env;
		TypeBuilder				 _type;

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
						var ret = _source.ProjectInfo.Engine.GetActiveEnv(_source.FileIndex, _currentLine + 1);

						if (ret.Field0 != null)
						{
							_env = ret.Field0;
							_type = ret.Field1;
						}
					}
				}

				lexer.SetLine(_currentLine + 1, source, offset, _env, _type);
			}
		}
	}
}
