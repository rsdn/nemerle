@echo off

set NoPause=true

IF "%Type%"=="" set Type=Debug

call Build.cmd

@echo ERRORLEVEL

IF NOT ERRORLEVEL 1 call Reg.cmd


