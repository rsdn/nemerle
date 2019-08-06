@echo off
title %~nx0
cd /D %~dp0

setlocal ENABLEEXTENSIONS
set KEY_NAME="HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\SxS\VS7"
set VALUE_NAME=15.0

for /f "tokens=3" %%a in ('reg query %KEY_NAME% /V %VALUE_NAME%  ^|findstr /ri "REG_SZ"') DO set Value=%%a

call "%Value%Common7\Tools\VsDevCmd.bat"
set NoPause=true
MSBuild.exe NemerleAll.nproj /target:Stage2;CompilerTests /p:Configuration=Debug /verbosity:n /p:NTargetName=Build /tv:15.0 /p:TargetFrameworkVersion=v4.0

pause