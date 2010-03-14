set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"
%MSBuild% NemerleAll.nproj /target:DevBuildFast /p:Configuration=Debug 
rem /verbosity:n /p:TargetName=Build
pause