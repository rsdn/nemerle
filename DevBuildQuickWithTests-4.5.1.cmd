@echo off

if %PROCESSOR_ARCHITECTURE%==x86 (
            set MSBuild="%ProgramFiles%\MSBuild\12.0\Bin\MSBuild.exe"
) else (    set MSBuild="%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe"
)

@if not exist %MSBuild% (
  @echo Install Microsoft Build Tools 2013
  @echo http://www.microsoft.com/en-us/download/details.aspx?id=40014  
  @echo And possibly .NET 4.5.1
  @echo http://www.microsoft.com/en-us/download/details.aspx?id=40270
  @pause
  @exit /b 1
)

%MSBuild% NemerleAll.nproj /target:DevBuildQuickWithTests /p:Configuration=Debug /verbosity:n /p:NTargetName=Build /tv:4.0 /p:TargetFrameworkVersion=v4.5.1
rem /verbosity:n /p:TargetName=Build
pause