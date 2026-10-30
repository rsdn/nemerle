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
setlocal ENABLEEXTENSIONS
@echo Features=%Features%

@echo Starting build %Name% Target=%Target% Config=%Config% Features=%Features%

rem Locate MSBuild.exe via vswhere (any recent Visual Studio with the MSBuild component).
SET "VsWhere=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
IF NOT EXIST "%VsWhere%" (
@echo ERROR: vswhere.exe not found at "%VsWhere%". A Visual Studio installation with the MSBuild component is required to build Nemerle.
EXIT /B -1
)

SET VsInstallPath=
for /f "usebackq delims=" %%a in (`"%VsWhere%" -latest -products * -requires Microsoft.Component.MSBuild -property installationPath`) DO SET VsInstallPath=%%a

IF NOT DEFINED VsInstallPath (
@echo ERROR: vswhere.exe did not find any Visual Studio installation with the Microsoft.Component.MSBuild component.
EXIT /B -1
)

SET "MsBuildExe=%VsInstallPath%\MSBuild\Current\Bin\MSBuild.exe"
IF NOT EXIST "%MsBuildExe%" (
SET "MsBuildExe=%VsInstallPath%\MSBuild\15.0\Bin\MSBuild.exe"
)
@echo Using MSBuild: "%MsBuildExe%"

"%MsBuildExe%" "%~dp0NemerleAll.nproj" "/t:%Features%" "/p:NTargetName=%Target%" "/p:Configuration=%Config%" "/p:NInstall=%NemerleInstallDir%"
SET NemerleBuildExitCode=%ERRORLEVEL%
@echo Nemerle installed to "%~dp0bin\%Config%\net-4.0"
EXIT /B %NemerleBuildExitCode%
rem pause
