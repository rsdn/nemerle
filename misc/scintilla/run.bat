@echo off
if "%1" == "" goto end
if "%1" == "DOC" goto doc
if "%1" == "DEBUG" goto debug
if "%1" == "RUN" goto run
goto end

:doc
echo .NET Framework SDK Documentation . . .
"c:\program files\common files\Microsoft Shared\Help\dexplore.exe"
echo .NET Framework SDK Documentation closed.
goto end

:debug
echo GuiDebbuger . . .
echo Tip: Choose your EXE file in DEBUG menu and open corresponding source file
"C:\Program Files\Microsoft.NET\SDK\v1.1\GuiDebug\DbgCLR.exe"
echo GuiDebbuger closed.
goto end

:end