@echo off

set MSBuild=%SystemRoot%\Microsoft.NET\Framework\v2.0.50727\MSBuild.exe
@echo MSBuild=%MSBuild%

IF "%Type%"=="" set Type=Debug

@echo ### Backup initials boot files #########################
IF EXIST boot\old\ RMDIR /S /Q boot\old

MKDIR boot\old\

IF errorlevel 1 goto Error
copy /Y boot\*.dll boot\old
IF errorlevel 1 goto Error
copy /Y boot\*.exe boot\old
IF errorlevel 1 goto Error
copy /Y boot\*.pdb boot\old

@echo !!! Backup success !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

@echo ### Phase 1 ############################################
%MSBuild% Nemerle.sln /p:Configuration=%Type%

IF errorlevel 1 goto Error
@echo !!! Phase 1 success !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

@echo ### Phase 2 ############################################
@echo ### Copy new binaries to boot
copy /Y bin\%Type%\*.dll boot
IF errorlevel 1 goto Error
copy /Y bin\%Type%\*.exe boot
IF errorlevel 1 goto Error
copy /Y bin\%Type%\*.pdb boot
@echo !!! Copy success!

@echo ### Build solution (phase 2)
%MSBuild% Nemerle.sln /p:Configuration=%Type%
IF errorlevel 1 goto Error
@echo !!! Build solution (phase 2) success!

copy /Y bin\%Type%\*.dll boot
IF errorlevel 1 goto Error
copy /Y bin\%Type%\*.exe boot
IF errorlevel 1 goto Error
copy /Y bin\%Type%\*.pdb boot

@echo !!! Phase 2 success !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
goto Success

@echo Phase 1 failed!
goto Error

:Error
@echo !!! Build FAILED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
copy /Y boot\old\*.dll boot
copy /Y boot\old\*.exe boot
copy /Y boot\old\*.pdb boot
pause
exit /b 1

:Success

IF NOT "%NoPause%"=="true" pause