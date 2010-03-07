@echo off

rem Set Type to Release, NoPause to true
set Type=Release
set NoPause=true

rem Build MSBuild Task
call Build-MSBuildTask.cmd

set ProgFiles=%ProgramFiles(x86)%
if "%ProgFiles%" == "" (
    set ProgFiles=%ProgramFiles%
)

rem Create %ProgFiles%\Nemerle
rd /s /q "%ProgFiles%\Nemerle" > nul
md "%ProgFiles%\Nemerle"

rem Copy needed files to %ProgFiles%\Nemerle
rem 1. Copy from boot

copy /y boot\ncc.exe /b "%ProgFiles%\Nemerle"
copy /y boot\ncc.exe /b "%ProgFiles%\Nemerle"
copy /y boot\Nemerle.dll /b "%ProgFiles%\Nemerle"
copy /y boot\Nemerle.Compiler.dll /b "%ProgFiles%\Nemerle"
copy /y boot\Nemerle.Macros.dll /b "%ProgFiles%\Nemerle"
copy /y boot\Nemerle.MSBuild.Tasks.dll /b "%ProgFiles%\Nemerle"

rem 2. Copy MSBuild targets
copy "tools\msbuild-task\Nemerle.MSBuild.targets" /b "%ProgFiles%\Nemerle"

rem Build Installer
call BuildInstallerFull.cmd

rem Done