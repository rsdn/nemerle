%SystemRoot%\Microsoft.NET\Framework\v3.5\MSBuild.exe %1 Nemerle.VSIP.sln
cd bin\Debug
copy ComInteropHelper.dll "%ProgramFiles%\Nemerle\*.*"
copy Nemerle*.dll         "%ProgramFiles%\Nemerle\*.*"
cd ..\..

IF NOT "%NoPause%"=="true" pause
