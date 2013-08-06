set NoPause=true
call MSBuild-3.5.cmd NemerleAll.nproj /target:DevBuildQuick /p:Configuration=Debug /verbosity:n /p:NTargetName=Build
rem /verbosity:n /p:TargetName=Build
IF %errorlevel% == 0 call Reg-bins-2.cmd
pause