set MSBuild="%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe"
%MSBuild% NemerleAll.nproj /target:DevBuildQuickWithTests /p:Configuration=Debug /verbosity:n /p:NTargetName=Build /tv:4.0 /p:TargetFrameworkVersion=v4.5.1
rem /verbosity:n /p:TargetName=Build
pause