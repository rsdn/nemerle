set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v3.5\msbuild.exe"
set NoPause=true
%MSBuild% NemerleAll.nproj /target:DevBuild2StageWithTests /p:Configuration=Debug /verbosity:n
rem /p:NTargetName=Build
IF %errorlevel% == 0 call Reg-bins-2.cmd
pause