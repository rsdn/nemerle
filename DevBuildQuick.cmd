set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"
set NoPause=true
%MSBuild% NemerleAll.nproj /target:DevBuildQuick /p:Configuration=Debug /verbosity:n /p:NTargetName=Build
rem /verbosity:n /p:TargetName=Build
IF %errorlevel% == 0 call Reg-bins-2.cmd
pause