include ../../config.mak

NPATH   = ../../boot
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

antslib.dll: antscompiler.n
	$(COMPILE) antscompiler.n -tdll -o antslib.dll

antsmacro.dll: antsmacro.n antslib.dll
	$(COMPILE) antsmacro.n -r antslib.dll -r ../../boot/Nemerle.Compiler.dll -tdll -o antsmacro.dll


ants.exe: ants.n antslib.dll antsmacro.dll
	$(COMPILE) ants.n -r antslib.dll -r antsmacro.dll -o ants.exe

run: ants.exe
	$(MONO) ants.exe

clean:
	rm -f ants.exe antslib.dll
