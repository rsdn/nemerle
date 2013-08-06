set NoPause=true
call MSBuild-3.5.cmd NemerleAll.nproj /target:DevBuild2StageWithTests /p:Configuration=Debug /verbosity:n
rem /p:NTargetName=Build
IF %errorlevel% == 0 call Reg-bins-2.cmd
pause