@echo off

IF NOT "%PROCESSOR_ARCHITECTURE%" == "x86" goto b64
IF NOT "%PROCESSOR_ARCHITEW6432%" == "" goto b64
set MSBuild=%SystemRoot%\Microsoft.NET\Framework\v2.0.50727\MSBuild.exe
goto b32
:b64
set MSBuild=%SystemRoot%\Microsoft.NET\Framework64\v2.0.50727\MSBuild.exe
:b32

set errors=no
goto skip
:err_check
set errors=yes
IF %1 == 0 set errors=no
exit /b %1
:skip

@echo MSBuild=%MSBuild%

IF "%Type%"=="" set Type=Debug

@echo ### Backup initials boot files #########################
IF EXIST boot\old\ RMDIR /S /Q boot\old

MKDIR boot\old\
call :err_check %errorlevel%
IF %errors% == yes goto Error

copy /Y boot\*.dll boot\old
call :err_check %errorlevel%
IF %errors% == yes goto Error

copy /Y boot\*.exe boot\old
call :err_check %errorlevel%
IF %errors% == yes goto Error

copy /Y boot\*.pdb boot\old

@echo !!! Backup success !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

@echo ### Phase 1 ############################################
%MSBuild% Nemerle-2005.sln /p:Configuration=%Type%

call :err_check %errorlevel%
IF %errors% == yes goto Error
@echo !!! Phase 1 success !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IF "%SkipPhase2%"=="true" goto Success

@echo ### Phase 2 ############################################
@echo ### Copy new binaries to boot
copy /Y bin\%Type%\*.dll boot
call :err_check %errorlevel%
IF %errors% == yes goto Error
copy /Y bin\%Type%\*.exe boot
call :err_check %errorlevel%
IF %errors% == yes goto Error
copy /Y bin\%Type%\*.pdb boot
@echo !!! Copy success!

@echo ### Build solution (phase 2)
%MSBuild% Nemerle-2005.sln /p:Configuration=%Type%
call :err_check %errorlevel%
IF %errors% == yes goto Error
@echo !!! Build solution (phase 2) success!

copy /Y bin\%Type%\*.dll boot
call :err_check %errorlevel%
IF %errors% == yes goto Error
copy /Y bin\%Type%\*.exe boot
call :err_check %errorlevel%
IF %errors% == yes goto Error
copy /Y bin\%Type%\*.pdb boot

@echo !!! Phase 2 success !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
goto Success

@echo Phase 1 failed!
goto Error

:Success
@echo Build success!
IF NOT "%NoPause%"=="true" pause
exit /b 0

:strong_fail
exit /b 1

:Error
@echo !!! Build FAILED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
copy /Y boot\old\*.dll boot
copy /Y boot\old\*.exe boot
copy /Y boot\old\*.pdb boot
IF NOT "%NoPause%"=="true" pause
call :strong_fail
