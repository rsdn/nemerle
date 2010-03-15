set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"
%MSBuild% NemerleAll.nproj /target:DevBuildQuick /p:Configuration=Debug /verbosity:n /p:NTargetName=Build
rem /verbosity:n /p:TargetName=Build
pause