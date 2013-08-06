set NoPause=true
call MSBuild-4.0.cmd NemerleAll.nproj /target:DevBuildFull /p:Configuration=Debug /verbosity:n  /tv:4.0 /p:TargetFrameworkVersion=v4.0
rem  /p:TargetName=ReBuild
pause