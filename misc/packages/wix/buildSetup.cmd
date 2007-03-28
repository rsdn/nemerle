@echo off
rem Copyright (c) 2003-2007 The University of Wroclaw.
rem All rights reserved.
rem
rem Redistribution and use in source and binary forms, with or without
rem modification, are permitted provided that the following conditions
rem are met:
rem    1. Redistributions of source code must retain the above copyright
rem       notice, this list of conditions and the following disclaimer.
rem    2. Redistributions in binary form must reproduce the above copyright
rem       notice, this list of conditions and the following disclaimer in the
rem       documentation and/or other materials provided with the distribution.
rem    3. The name of the University may not be used to endorse or promote
rem       products derived from this software without specific prior
rem       written permission.
rem
rem THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
rem IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
rem OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
rem NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
rem SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
rem TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
rem PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
rem LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
rem NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
rem SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

echo.

rem
rem Ensure we have all required tools
rem

if "%VisualStudioIntegration%"=="" goto errEnvVarVSSDK

if not "%WixDir%"=="" goto wixSet

rem Check default wix folder
if not exist "%ProgramFiles%\Windows Installer XML v3\bin\light.exe" goto errEnvVarWix

echo light.exe found in "%ProgramFiles%\Windows Installer XML v3\bin" folder
set WixDir=%ProgramFiles%\Windows Installer XML v3\bin

:wixSet

rem
rem Ready to build the setup.exe
rem

set RegPkgDir=%VisualStudioIntegration%\Tools\Bin

set NemerleSetupContent=%~dp0dist
set GeneratedFile=%~dp0src\Generated.wxi
set MsiFile=%~dp0redist\Nemerle.msi

rem
rem Add some assemblies to GAC.
rem

GacUtil -i %NemerleSetupContent%\bin\Nemerle.dll
GacUtil -i %NemerleSetupContent%\bin\Nemerle.Macros.dll
GacUtil -i %NemerleSetupContent%\bin\Nemerle.Compiler.dll

"%RegPkgDir%\RegPkg.exe" /root:Software\Microsoft\VisualStudio\8.0 "/wixfile:%GeneratedFile%" /codebase "%NemerleSetupContent%\vs-plugin\Nemerle.VisualStudio.dll"
if errorlevel 1 goto done

"%WixDir%\candle.exe" -ext WixNetFxExtension -sw1080 src/*.wxs
if errorlevel 1 goto done

"%WixDir%\light.exe"  -ext WixNetFxExtension *.wixobj -ext WixUIExtension -out "%MsiFile%" -cultures:en-us 
if errorlevel 1 goto done

echo Building NemerleSetup.exe. Please wait...
iexpress /N /Q Bootstrapper.sed

rem Clean up
del "%GeneratedFile%"
del *.wixobj
del "%MsiFile%"

echo.
echo Done.
goto done

:errEnvVarVSSDK
echo Please specify environment variable "VisualStudioIntegration".
goto done

:errEnvVarWix
echo Please specify environment variable "WixDir".
goto done

:done
pause