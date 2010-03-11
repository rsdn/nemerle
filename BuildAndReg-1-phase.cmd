@echo off

set SkipPhase2=true
set NoPause=true

IF "%Type%"=="" set Type=Debug

call Build.cmd

@echo ERRORLEVEL

set errors=no
goto skip
:err_check
set errors=yes
IF %1 == 0 set errors=no
exit /b %1
:skip

call :err_check %errorlevel%
IF %errors% == no call Reg-bins.cmd
call :err_check %errorlevel%
IF %errors% == no call BuildTest.cmd
pause
