@echo off

IF "%Type%"=="" set Type=Debug

set NemerleBin=%~dp0bin\%Type%
set GacUtil="%VS80COMNTOOLS%..\..\SDK\v2.0\Bin\gacutil.exe"
set NemerleInstall=boot

IF NOT "%PROCESSOR_ARCHITECTURE%" == "x86" goto b64
IF NOT "%PROCESSOR_ARCHITEW6432%" == "" goto b64
set NGen="%SystemRoot%\Microsoft.NET\Framework\v2.0.50727\ngen.exe"
goto b32
:b64
set NGen="%SystemRoot%\Microsoft.NET\Framework64\v2.0.50727\ngen.exe"
:b32

@echo NemerleInstall=%NemerleInstall%
@echo VS80COMNTOOLS=%VS80COMNTOOLS%
@echo GacUtil=%GacUtil%
@echo NGen=%NGen%

%NGen% uninstall "%NemerleInstall%\Nemerle.dll"
%NGen% uninstall "%NemerleInstall%\Nemerle.Compiler.dll"
%NGen% uninstall "%NemerleInstall%\Nemerle.Macros.dll"
%NGen% uninstall "%NemerleInstall%\Nemerle.MSBuild.Tasks.dll"
%NGen% uninstall "%NemerleInstall%\ncc.exe"

@echo errorlevel=%errorlevel%

%NGen% install "%NemerleInstall%\Nemerle.dll"
%NGen% install "%NemerleInstall%\Nemerle.Compiler.dll"
%NGen% install "%NemerleInstall%\Nemerle.Macros.dll"
%NGen% install "%NemerleInstall%\Nemerle.MSBuild.Tasks.dll"
%NGen% install "%NemerleInstall%\ncc.exe"
