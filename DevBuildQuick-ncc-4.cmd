set NoPause=true
call MSBuild-4.0.cmd NemerleAll.nproj /target:DevBuildQuickNccOnly /p:Configuration=Debug /verbosity:n /p:NTargetName=Build /tv:4.0 /p:TargetFrameworkVersion=v4.0
rem /verbosity:n /p:TargetName=Build
IF %errorlevel% == 0 call Reg-bins-2-4.0.cmd
pause