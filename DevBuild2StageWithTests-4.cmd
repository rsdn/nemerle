set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"
%MSBuild% NemerleAll.nproj /target:DevBuild2StageWithTests /p:Configuration=Debug /verbosity:n /tv:4.0 /p:TargetFrameworkVersion=v4.0
rem /p:NTargetName=Build
IF %errorlevel% == 0 call Reg-bins-2.cmd
pause
