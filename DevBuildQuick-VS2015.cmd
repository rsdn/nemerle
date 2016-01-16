IF NOT "%1" == "Build" call CleanUp.cmd
set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"
set NoPause=true
%MSBuild% NemerleAll.nproj /target:DevBuildQuick /p:Configuration=Debug /verbosity:n /p:NTargetName=Build  /tv:14.0 /p:TargetFrameworkVersion=v4.0

rem /verbosity:n /p:TargetName=Build
IF %errorlevel% == 0 call Reg-bins-VS2015.cmd
pause