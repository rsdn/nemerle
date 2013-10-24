@echo off

IF "%Type%"=="" set Type=Debug

set GacUtil="%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.0A\Bin\NETFX 4.0 Tools\gacutil.exe"
set NemerleInstall=%ProgramFiles%\Nemerle\Net-4.0

@echo NemerleInstall=%NemerleInstall%
@echo VSXXXCOMNTOOLS=%VS100COMNTOOLS%
@echo GacUtil=%GacUtil%

md "%NemerleInstall%"

%GacUtil% /u Nemerle
%GacUtil% /u Nemerle.Compiler
%GacUtil% /u Nemerle.MSBuild.Tasks
%GacUtil% /u Nemerle.Macros

IF "%1"=="u" goto end

@echo errorlevel=%errorlevel%

%GacUtil% /i "%NemerleInstall%\Nemerle.dll"
%GacUtil% /i "%NemerleInstall%\Nemerle.Compiler.dll"
%GacUtil% /i "%NemerleInstall%\Nemerle.Macros.dll"
%GacUtil% /i "%NemerleInstall%\policy.1.2.Nemerle.dll"
%GacUtil% /i "%NemerleInstall%\policy.1.2.Nemerle.Compiler.dll"
%GacUtil% /i "%NemerleInstall%\policy.1.2.Nemerle.Macros.dll"

rem It doesn't have a strong name
rem %GacUtil% /i "%NemerleInstall%\Nemerle.MSBuild.Tasks.dll"

:end

IF NOT "%NoPause%"=="true" pause
