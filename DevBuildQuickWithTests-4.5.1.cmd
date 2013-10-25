@if not exist "%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe" (
  @echo Install Microsoft Build Tools 2013
  @echo http://www.microsoft.com/en-us/download/details.aspx?id=40014  
  @echo And possibly .NET 4.5.1
  @echo http://www.microsoft.com/en-us/download/details.aspx?id=40270
  @pause
  @exit /b 1
)

set MSBuild="%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe"
%MSBuild% NemerleAll.nproj /target:DevBuildQuickWithTests /p:Configuration=Debug /verbosity:n /p:NTargetName=Build /tv:4.0 /p:TargetFrameworkVersion=v4.5.1
rem /verbosity:n /p:TargetName=Build
pause