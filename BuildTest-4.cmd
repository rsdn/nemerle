@echo off

set errors=no
goto skip
:err_check
set errors=yes
IF %1 == 0 set errors=no
exit /b %1
:skip

IF "%Type%"=="" set Type=Debug

call MSBuild-4.0.cmd Tests.nproj /p:Configuration=%Type% /tv:4.0 /p:TargetFrameworkVersion=v4.0
call :err_check %errorlevel%
IF %errors% == yes goto Error

IF NOT "%NoPause%"=="true" pause
exit /b 0

:strong_fail
exit /b 1

:Error
IF NOT "%NoPause%"=="true" pause
call :strong_fail
