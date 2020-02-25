@echo off

IF "%~1"=="" (
@echo The parameter 1 [Config] is empty. Use Debug or Release.
EXIT /B -1
)

IF "%~2"=="" (
@echo The parameter 2 [Target] is empty. Use Build or Rebuild.
EXIT /B -1
)

IF "%~3"=="" (
@echo The parameter 2 [Features] is empty. 
EXIT /B -1
)

SET Name=Nemerle
SET Config=%~1
SET Target=%~2
SET Features=%~3
SET Verbosity=m
SET MSBUILDENABLEALLPROPERTYFUNCTIONS=1
SET NemerleInstallDir=%~dp0bin\%Config%\net-4.0
SET KEY_NAME="HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\SxS\VS7"
SET VALUE_NAME=15.0
setlocal ENABLEEXTENSIONS
@echo Features=%Features%

@echo Starting build %Name% Target=%Target% Config=%Config% Features=%Features%

for /f "tokens=2,*" %%a in ('reg query %KEY_NAME% /V %VALUE_NAME%  ^|findstr /ri "REG_SZ"') DO set Value=%%b

IF defined Value (
call "%Value%Common7\Tools\VsDevCmd.bat"
msbuild "%~dp0NemerleAll.nproj" "/t:%Features%" "/p:NTargetName=%Target%" "/p:Configuration=%Config%" "/p:NInstall=%NemerleInstallDir%"
@echo Nemerle installed to "%~dp0bin\%Config%\net-4.0"
) else (
@echo %KEY_NAME%\%VALUE_NAME% not found in Windows Registry. Possibly no Visual Studio 2017 installed.
)
rem pause
