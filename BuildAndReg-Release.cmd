@echo off

set Type=Release

call BuildAndReg.cmd
call Reg-boot.cmd

pause