IF NOT "%1" == "Build" call CleanUp.cmd
set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"
set NoPause=true
%MSBuild% NemerleAll.nproj /target:DevBuildQuick /p:Configuration=Debug /verbosity:n /p:NTargetName=Build /p:NInstallIntegration=True /tv:4.0 /p:TargetFrameworkVersion=v4.0

rem /verbosity:n /p:TargetName=Build
IF %errorlevel% == 0 call Reg-bins-VS2010.cmd
pause