set NoPause=true
call MSBuild-4.0.cmd NemerleAll.nproj /target:DevBuild2StageWithTests /p:Configuration=Debug /verbosity:n /tv:4.0 /p:TargetFrameworkVersion=v4.0
rem /p:NTargetName=Build
IF %errorlevel% == 0 call Reg-bins-2-4.0.cmd
pause
