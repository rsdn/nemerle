@echo off
call MSBuild-3.5.cmd NemerleAll.nproj /tv:3.5 /p:TargetFrameworkVersion=v3.5 /t:%*
