set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"

rem First you must run one of these two commands to compile and reg compiler
%MSBuild% NemerleAll.nproj /t:NccFast
%MSBuild% NemerleAll.nproj /t:NccFull

rem then you can compile ohter packages
%MSBuild% NemerleAll.nproj /t:VSIntegration
%MSBuild% NemerleAll.nproj /t:Shell
%MSBuild% NemerleAll.nproj /t:Tools

rem Installer target calls NccFull, VSIntegration, Tools and Shell first
%MSBuild% NemerleAll.nproj /t:Installer /p:Configuration=Release

rem Support targets for developers only
%MSBuild% NemerleAll.nproj /t:RegProgFiles
%MSBuild% NemerleAll.nproj /t:RunTests
