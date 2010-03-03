@echo off

rem Set Type to Release, NoPause to true
set Type=Release
set NoPause=true

rem Build MSBuild Task
call Build-MSBuildTask.cmd

rem Create %ProgramFiles%\Nemerle
rd /s /q "%ProgramFiles%\Nemerle" > nul
md "%ProgramFiles%\Nemerle"

rem Copy needed files to c:\Program Files\Nemerle
rem 1. Copy from boot
copy /y boot\ncc.exe /b "%ProgramFiles%\Nemerle"
copy /y boot\Nemerle.dll /b "%ProgramFiles%\Nemerle"
copy /y boot\Nemerle.Compiler.dll /b "%ProgramFiles%\Nemerle"
copy /y boot\Nemerle.Macros.dll /b "%ProgramFiles%\Nemerle"
copy /y boot\Nemerle.MSBuild.Tasks.dll /b "%ProgramFiles%\Nemerle"

rem 2. Copy MSBuild targets
copy "tools\msbuild-task\Nemerle.MSBuild.targets" /b "%ProgramFiles%\Nemerle"

rem Build Installer
call BuildInstallerFull.cmd

rem Done