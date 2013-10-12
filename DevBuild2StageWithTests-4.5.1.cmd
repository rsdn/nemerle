@if not exist "%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe" (
  @echo Install Microsoft Build Tools 2013
  @echo http://www.microsoft.com/en-us/download/details.aspx?id=40014  
  @echo And possibly .NET 4.5.1
  @echo http://www.microsoft.com/en-us/download/details.aspx?id=40270
  @pause
  @exit /b 1
)

set MSBuild="%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe"
set NoPause=true
%MSBuild% NemerleAll.nproj /target:DevBuild2StageWithTests /p:Configuration=Debug /verbosity:n /tv:4.0 /p:TargetFrameworkVersion=v4.5.1
rem /p:NTargetName=Build
IF %errorlevel% == 0 call Reg-bins-2-4.5.1.cmd
pause
