ren Tests.exe Tests.exe.bak
ren Nemerle.dll Nemerle.dll.bak
ren Nemerle.Compiler.dll Nemerle.Compiler.dll.bak
ren Nemerle.Macros.dll Nemerle.Macros.dll.bak
ren System.Core.dll System.Core.dll.bak

ren Nemerle
del *.exe
del *.dll
del *.xml
del test_error.log
del test_out.txt
del data.dat
del positive\*.dll

ren Tests.exe.bak Tests.exe
ren Nemerle.dll.bak Nemerle.dll
ren Nemerle.Compiler.dll.bak Nemerle.Compiler.dll
ren Nemerle.Macros.dll.bak Nemerle.Macros.dll
ren System.Core.dll.bak System.Core.dll