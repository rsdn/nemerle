set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"
set NoPause=true
%MSBuild% NemerleAll.nproj /target:DevBuildFull /p:Configuration=Debug /verbosity:n /p:NInstallIntegration=True /tv:4.0 /p:TargetFrameworkVersion=v4.0
rem  /p:TargetName=ReBuild
pause