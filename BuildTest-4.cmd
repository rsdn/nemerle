@echo off
title %~nx0

setlocal ENABLEEXTENSIONS
set KEY_NAME="HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\SxS\VS7"
set VALUE_NAME=15.0

for /f "tokens=2,*" %%a in ('reg query %KEY_NAME% /V %VALUE_NAME%  ^|findstr /ri "REG_SZ"') DO set Value=%%b

call "%Value%Common7\Tools\VsDevCmd.bat"

set MSBuild=MSBuild.exe	

@echo MSBuild=%MSBuild%

set errors=no
goto skip
:err_check
set errors=yes
IF %1 == 0 set errors=no
exit /b %1
:skip

IF "%Type%"=="" set Type=Debug

%MSBuild% NemerleAll.nproj /target:Stage1;CompilerTests /p:Configuration=%Type% /verbosity:n /p:NTargetName=Build  /tv:15.0 /p:TargetFrameworkVersion=v4.5
call :err_check %errorlevel%
IF %errors% == yes goto Error

IF NOT "%NoPause%"=="true" pause
exit /b 0

:strong_fail
exit /b 1

:Error
IF NOT "%NoPause%"=="true" pause
call :strong_fail

pause