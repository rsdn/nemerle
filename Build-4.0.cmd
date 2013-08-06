@echo off
call MSBuild-4.0.cmd NemerleAll.nproj /tv:4.0 /p:TargetFrameworkVersion=v4.0 /t:%*