@echo off

setlocal

rem %1 is version of framework you need
rem %2... parameters passed to MSBuild
rem If .NET Framework has not been installed, MSBuild variable will be empty

if "%~1" equ "" (
  echo Provide .NET Framework version
  exit /b
)

if "%PROCESSOR_ARCHITECTURE%" equ "x86" ( 
  set MSBuild=%SystemRoot%\Microsoft.NET\Framework\v%1\msbuild.exe
) else (
  set MSBuild=%SystemRoot%\Microsoft.NET\Framework64\v%1\MSBuild.exe
)

if not exist "%MSBuild%" (
  echo Install .NET Framework v%1
  set MSBuild=
  exit /b 1
)

call :takeAllExceptFirst %*
"%MSBuild%" %RESULT%

endlocal

goto :eof

rem Return value stored in %RESULT%
:takeAllExceptFirst
SET RESULT=
for /f "tokens=1*" %%i in ("%*") do (
  SET RESULT=%%j
)
goto :eof
