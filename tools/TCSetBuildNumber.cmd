@echo off

setlocal enableextensions 
for /f "tokens=*" %%a in ('git describe --tag --long') do ( 
  set GitRevision=%%a
  goto done
) 
endlocal 
:done
echo ##teamcity[buildNumber '%GitRevision%']