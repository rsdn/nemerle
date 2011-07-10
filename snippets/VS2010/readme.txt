Build requirements :

	1. Visual Studio 2010 (non-express edition).
	2. Visual Studio 2010 SDK.

Build steps:

	1. Uninstall Nemerle 1.0
	2. Remove all files from %ProgramFiles%\Nemerle if it exists.
	3. Build current compiler version: run DevBuildQuick-4.cmd.
	4. Copy Nemerle\bin\Debug\net-4.0\Stage1 folder contents into %ProgramFiles%\Nemerle.
	5. Run  Visual Studio 2010 with administrator rights (right click on VS2010 shortcut and select 'Run as administrator')
	6. Open Nemerle\snippets\VS2010\Nemerle.VS2010.sln and build (use "Rebuild All" command).
	7. Choose one of the two following:
	  a) Install Nemerle\snippets\VS2010\bin\Debug\Nemerle.VisualStudio.vsix (for using purpose)
	  b) Set up Nemerle.VisualStudio.csproj as a StartUp project and set following parameters in Debug tab of Project properties: 
             Start external program: path to another copy of VS 2010 (f.e. C:\Program Files\Microsoft Visual Studio 10.0\Common7\IDE\devenv.exe)
             Comand line arguments: /rootSuffix Exp
	8. Press F5 and voila!

Note: this is an early preview version. Please, send your bug reports to http://code.google.com/p/nemerle/issues/list.
