@echo off
mkdir tests\
del tests\ /Q /F
copy "..\..\Lib\bin\Release\Nemerle.Statechart.Lib.dll" tests\ 
copy "c:\Program Files\Nemerle\Net-4.0\Nemerle.dll" tests\
copy "c:\Program Files\Nemerle\Net-4.0\Nemerle.Peg.dll" tests\
copy "..\..\bin\Release\Nemerle.Statechart.dll" tests\ 
copy "..\..\Nemerle.Statechart.Runtime\bin\Release\Nemerle.Statechart.Runtime.dll" tests\ 
copy "..\fsmtest\bin\Release\fsmtest.exe" tests\ 
"c:\Program Files\Nemerle\Net-4.0\Nemerle.Compiler.Test.exe" -ref:"tests\Nemerle.Statechart.dll" -ref:"tests\Nemerle.Statechart.Lib.dll" -ref:"tests\fsmtest.exe" -ref:"tests\Nemerle.Statechart.Runtime.dll" -output:tests\ positive\*.n negative\*.n
pause