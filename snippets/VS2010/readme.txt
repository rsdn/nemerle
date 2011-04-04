Build requirements 

	1. Visual Studio 2010 (not express edition)
	2. Visual Studio 2010 SDK

Build steps

	1. Uninstall Nemerle 1.0
	2. Remove all files from %ProgramFiles%\Nemerle if exists.
	3. Build current compiler version: run DevBuildQuick-4.cmd.
	4. Copy Nemerle\bin\Debug\net-4.0\Stage1 folder contents into %ProgramFiles%\Nemerle.
	5. Open VS 2010 with administrator rights (right click on VS2010 shortcut and select 'Run as administrator')
	6. Open Nemerle\snippets\VS2010\Nemerle.VS2010.sln and build (use Rebuild All command).
	7. Install Nemerle\snippets\VS2010\bin\Debug\Nemerle.VisualStudio.vsix (for using purpose)
      or set up Nemerle.VisualStudio.csproj as StrtUp project, another copy of VS 2010 as startup external program, specify "/rootSuffix Exp" command line parameters to it and press F5.

Note: this is an early preview version. Send bug reports to http://code.google.com/p/nemerle/issues/list, please.
