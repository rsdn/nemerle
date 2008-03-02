@echo off

set Type=Release

call BuildAndReg.cmd
call Reg.cmd

IF NOT "%NoPause%"=="true" pause
