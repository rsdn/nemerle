@echo off

set Type=Release

call BuildAndReg.cmd
call Reg-bins.cmd

IF NOT "%NoPause%"=="true" pause
