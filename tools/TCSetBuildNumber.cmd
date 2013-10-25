@echo off

setlocal enableextensions
if not defined GIT_PATH set GIT_PATH=git
for /f "tokens=*" %%a in ('%GIT_PATH% describe --tag --long') do ( 
  set GitRevision=%%a
  goto done
) 
endlocal 
:done
echo ##teamcity[buildNumber '%GitRevision%']