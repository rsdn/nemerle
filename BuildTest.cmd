@echo off

IF NOT "%PROCESSOR_ARCHITECTURE%" == "x86" goto b64
IF NOT "%PROCESSOR_ARCHITEW6432%" == "" goto b64
set MSBuild=%SystemRoot%\Microsoft.NET\Framework\v2.0.50727\MSBuild.exe
goto b32
:b64
set MSBuild=%SystemRoot%\Microsoft.NET\Framework64\v2.0.50727\MSBuild.exe
:b32

@echo MSBuild=%MSBuild%

set errors=no
goto skip
:err_check
set errors=yes
IF %1 == 0 set errors=no
exit /b %1
:skip

IF "%Type%"=="" set Type=Debug

%MSBuild% Tests.nproj /p:Configuration=%Type%
call :err_check %errorlevel%
IF %errors% == yes goto Error

IF NOT "%NoPause%"=="true" pause
exit /b 0

:strong_fail
exit /b 1

:Error
IF NOT "%NoPause%"=="true" pause
call :strong_fail
