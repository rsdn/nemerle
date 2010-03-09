@echo off

set Type=Release
set NoPause=true
set Nemerle=%~dp0\boot

rem Build MSBuild task
call Build-MSBuildTask.cmd

rem Copy MSBuild targets to boot
copy "tools\msbuild-task\Nemerle.MSBuild.targets" /b "boot\"

rem Build Installer
call BuildInstallerFull.cmd

rem Done
set Type=
set NoPause=
set Nemerle=