@echo off

set MSBuild=%SystemRoot%\Microsoft.NET\Framework\v2.0.50727\MSBuild.exe
@echo MSBuild=%MSBuild%

IF "%Type%"=="" set Type=Debug

%MSBuild% Tests.nproj /p:Configuration=%Type%

pause