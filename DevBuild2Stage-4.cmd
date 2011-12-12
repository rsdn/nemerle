set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"
set NoPause=true
%MSBuild% NemerleAll.nproj /target:Stage2 /p:Configuration=Debug /verbosity:d /tv:4.0 /p:TargetFrameworkVersion=v4.0
rem /p:NTargetName=Build
IF %errorlevel% == 0 call Reg-bins-2-4.0.cmd
pause
