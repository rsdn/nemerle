IF NOT "%PROCESSOR_ARCHITECTURE%" == "x86" goto b64
IF NOT "%PROCESSOR_ARCHITEW6432%" == "" goto b64
set MSBuild=%SystemRoot%\Microsoft.NET\Framework\v2.0.50727\MSBuild.exe
goto b32
:b64
set MSBuild=%SystemRoot%\Microsoft.NET\Framework64\v2.0.50727\MSBuild.exe
:b32

@echo MSBuild=%MSBuild%

IF "%Type%"=="" set Type=Debug

%MSBuild% Nemerle.MSBuild.Tasks.nproj /p:Configuration=%Type%

 pause