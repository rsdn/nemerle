@echo off

set NemerleRoot=%~dp0.
set GacUtil="%ProgramFiles%\Microsoft SDKs\Windows\v7.0A\bin\NETFX 4.0 Tools\gacutil.exe"
set NemerleInstall=%ProgramFiles%\Nemerle\Net-4.0

IF NOT "%PROCESSOR_ARCHITECTURE%" == "x86" goto b64
IF NOT "%PROCESSOR_ARCHITEW6432%" == "" goto b64
set NGen="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\ngen.exe"
goto b32
:b64
"%NemerleRoot%\ExternalDependences\junction.exe" "%ProgramW6432%\Nemerle" "%ProgramFiles(x86)%\Nemerle"
:skipJunction
set NGen="%SystemRoot%\Microsoft.NET\Framework64\v4.0.30319\ngen.exe"
:b32

set errors=no
goto skip
:err_check
set errors=yes
IF %1 == 0 set errors=no
exit /b %1
:skip


@echo NemerleInstall=%NemerleInstall%
@echo GacUtil=%GacUtil%
@echo NGen=%NGen%

md "%NemerleInstall%"

%GacUtil% /u Nemerle
%GacUtil% /u Nemerle.Compiler
%GacUtil% /u Nemerle.MSBuild.Tasks
%GacUtil% /u Nemerle.Macros

%NGen% uninstall "%NemerleInstall%\Nemerle.dll"
%NGen% uninstall "%NemerleInstall%\Nemerle.Compiler.dll"
%NGen% uninstall "%NemerleInstall%\Nemerle.Macros.dll"
%NGen% uninstall "%NemerleInstall%\Nemerle.MSBuild.Tasks.dll"
%NGen% uninstall "%NemerleInstall%\ncc.exe"

@echo errorlevel=%errorlevel%

@echo --------------------------- Update registry ---------------------------

set NemerleMSBuildTargets=%NemerleInstall%\Nemerle.MSBuild.targets

@echo Add NemerleTarget=%NemerleMSBuildTargets% 
@echo     into HKLM\SOFTWARE\Microsoft\VisualStudio\10.0\MSBuild\SafeImports
reg.exe add HKLM\SOFTWARE\Microsoft\VisualStudio\10.0\MSBuild\SafeImports /v NemerleTarget /d "%NemerleInstall%\Nemerle.MSBuild.targets" /f

@echo     into HKCU\Software\Microsoft\VisualStudio\10.0\Configuration\MSBuild\SafeImports
reg.exe add HKCU\Software\Microsoft\VisualStudio\10.0\Configuration\MSBuild\SafeImports /v NemerleTarget /d "%NemerleInstall%\Nemerle.MSBuild.targets" /f

@echo     into HKCU\Software\Microsoft\VisualStudio\10.0Exp\Configuration\MSBuild\SafeImports
reg.exe add HKCU\Software\Microsoft\VisualStudio\10.0Exp\Configuration\MSBuild\SafeImports /v NemerleTarget /d "%NemerleInstall%\Nemerle.MSBuild.targets" /f

@echo --------------------------- Registry updated ---------------------------

%NGen% install "%NemerleInstall%\Nemerle.dll"
%NGen% install "%NemerleInstall%\Nemerle.Compiler.dll"
%NGen% install "%NemerleInstall%\Nemerle.Macros.dll"
%NGen% install "%NemerleInstall%\Nemerle.MSBuild.Tasks.dll"
%NGen% install "%NemerleInstall%\ncc.exe"

IF NOT "%NoPause%"=="true" pause

exit /b 0

:strong_fail
exit /b 1

:Error
call :strong_fail
