@echo on
title %~nx0

IF "%1" == "Rebuild" call CleanUp.cmd

setlocal ENABLEEXTENSIONS
set KEY_NAME="HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\SxS\VS7"
set VALUE_NAME=15.0

for /f "tokens=2,*" %%a in ('reg query %KEY_NAME% /V %VALUE_NAME%  ^|findstr /ri "REG_SZ"') DO set Value=%%b

IF defined Value (
call "%Value%Common7\Tools\VsDevCmd.bat"
set NoPause=true
MSBuild.exe %~dp0NemerleAll.nproj /target:DevBuildQuick /p:Configuration=Debug /verbosity:n /p:NTargetName=Build  /tv:15.0 /p:TargetFrameworkVersion=v4.0
rem /verbosity:n /p:TargetName=Build
IF %errorlevel% == 0 call Reg-bins-VS2015.cmd
) else (
@echo %KEY_NAME%\%VALUE_NAME% not found in Windows Registry.
)

vsixinstaller /uninstall:FFFFeaae-d2c0-461d-8ff6-b3bc8d67bcfe
%~dp0bin\Debug\net-4.0\VsIntegration\Nemerle.VisualStudio.vsix
pause