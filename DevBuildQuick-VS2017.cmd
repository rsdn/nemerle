IF NOT "%1" == "Build" call CleanUp.cmd
call "C:\VS2017\Common7\Tools\VsDevCmd.bat
set MSBuild="MSBuild.exe"
set MSBuild="%VS150COMNTOOLS%\..\..\MSBuild\15.0\Bin\MSBuild.exe"
set NoPause=true
%MSBuild% NemerleAll.nproj /target:DevBuildQuick /p:Configuration=Debug /verbosity:n /p:NTargetName=Build  /tv:15.0 /p:TargetFrameworkVersion=v4.0

rem /verbosity:n /p:TargetName=Build
rem IF %errorlevel% == 0 call Reg-bins-VS2015.cmd
pause