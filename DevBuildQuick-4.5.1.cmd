set MSBuild="%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe"
set NoPause=true
%MSBuild% NemerleAll.nproj /target:DevBuildQuick /p:Configuration=Debug /verbosity:n /p:NTargetName=Build  /tv:4.0 /p:TargetFrameworkVersion=v4.5.1

rem /verbosity:n /p:TargetName=Build
IF %errorlevel% == 0 call Reg-bins-2-4.5.1.cmd
pause