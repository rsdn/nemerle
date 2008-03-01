@echo off

IF "%Type%"=="" set Type=Debug

set NemerleBin=%~dp0bin\%Type%
set NemerleRoot=%~dp0.
set GacUtil="%VS80COMNTOOLS%..\..\SDK\v2.0\Bin\gacutil.exe"
set NemerleInstall=%ProgramFiles%\Nemerle

IF NOT "%PROCESSOR_ARCHITECTURE%" == "x86" goto b64
IF NOT "%PROCESSOR_ARCHITEW6432%" == "" goto b64
set NGen="%SystemRoot%\Microsoft.NET\Framework\v2.0.50727\ngen.exe"
goto b32
:b64
set NGen="%SystemRoot%\Microsoft.NET\Framework64\v2.0.50727\ngen.exe"
:b32

set errors=no
goto skip
:err_check
set errors=yes
IF %1 == 0 set errors=no
exit /b %1
:skip


@echo NemerleInstall=%NemerleInstall%
@echo VS80COMNTOOLS=%VS80COMNTOOLS%
@echo GacUtil=%GacUtil%
@echo NGen=%NGen%
@echo NemerleBin=%NemerleBin%

md "%NemerleInstall%"
cd /D "%NemerleInstall%"

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

copy /Y "%NemerleBin%\*.dll" "%NemerleInstall%\*.dll"

call :err_check %errorlevel%
IF %errors% == yes (
@echo errorlevel=%errorlevel%
@echo !!! ERROR: copy files !!!
pause
exit /b 1
)

copy /Y "%NemerleBin%\*.exe" "%NemerleInstall%\*.exe"

call :err_check %errorlevel%
IF %errors% == yes (
@echo !!! ERROR: copy files !!!
pause
exit /b 1
)

copy /Y "%NemerleRoot%\tools\msbuild-task\Nemerle.MSBuild.targets" "%NemerleInstall%\*.*"
copy /Y "%NemerleBin%\*.pdb" "%NemerleInstall%\*.pdb"
copy /Y "%NemerleBin%\*.xml" "%NemerleInstall%\*.xml"

@echo --------------------------- Update registry ---------------------------

set NemerleMSBuildTargets=%NemerleInstall%\Nemerle.MSBuild.targets

@echo Add NemerleTarget=%NemerleMSBuildTargets% 
@echo     into HKLM\SOFTWARE\Microsoft\VisualStudio\9.0\MSBuild\SafeImports
reg.exe add HKLM\SOFTWARE\Microsoft\VisualStudio\9.0\MSBuild\SafeImports /v NemerleTarget /d "%NemerleInstall%\Nemerle.MSBuild.targets" /f

@echo     into HKCU\Software\Microsoft\VisualStudio\9.0\Configuration\MSBuild\SafeImports
reg.exe add HKCU\Software\Microsoft\VisualStudio\9.0\Configuration\MSBuild\SafeImports /v NemerleTarget /d "%NemerleInstall%\Nemerle.MSBuild.targets" /f

@echo     into HKCU\Software\Microsoft\VisualStudio\9.0Exp\Configuration\MSBuild\SafeImports
reg.exe add HKCU\Software\Microsoft\VisualStudio\9.0Exp\Configuration\MSBuild\SafeImports /v NemerleTarget /d "%NemerleInstall%\Nemerle.MSBuild.targets" /f

@echo --------------------------- Registry updated ---------------------------

%NGen% install "%NemerleInstall%\Nemerle.dll"
%NGen% install "%NemerleInstall%\Nemerle.Compiler.dll"
%NGen% install "%NemerleInstall%\Nemerle.Macros.dll"
%NGen% install "%NemerleInstall%\Nemerle.MSBuild.Tasks.dll"
%NGen% install "%NemerleInstall%\ncc.exe"

cd "%~dp0"

pause
