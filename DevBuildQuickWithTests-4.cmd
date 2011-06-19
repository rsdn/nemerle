set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"
%MSBuild% NemerleAll.nproj /target:DevBuildQuickWithTests /p:Configuration=Debug /verbosity:n /p:NTargetName=Build /tv:4.0 /p:TargetFrameworkVersion=v4.0
rem /verbosity:n /p:TargetName=Build
pause