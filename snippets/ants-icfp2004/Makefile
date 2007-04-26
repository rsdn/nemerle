include ../../config.mak

NPATH   = ../../ncc/out.stage3
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

MAP = 0


all: ants.exe wanderers.exe

antslib.dll: antscompiler.n
	$(COMPILE) antscompiler.n -tdll -o antslib.dll

antsmacro.dll: antsmacro.n antslib.dll
	$(COMPILE) antsmacro.n -r antslib.dll -r $(NPATH)/Nemerle.Compiler.dll -tdll -o antsmacro.dll

serializer.dll: unvar.n serializer.n antslib.dll antsmacro.dll 
	$(COMPILE) -tdll unvar.n serializer.n -r antslib.dll -r antsmacro.dll -o $@

ants.exe: ants.n serializer.dll
	$(COMPILE) ants.n -r serializer.dll -r antsmacro.dll -o $@

wanderers.exe: wanderers.n serializer.dll
	$(COMPILE) wanderers.n -r serializer.dll -r antsmacro.dll -o $@

t: all
	$(MONO) ./ants.exe > the-ant 2>/dev/null
	$(MONO) simulator/simulator.exe simulator/worlds/sample$(MAP).world \
		the-ant simulator/brain.2.in 10000 dump-results 1000

run: Nemerle.dll ants.exe
	$(MONO) ./ants.exe

Nemerle.dll: $(NPATH)/Nemerle.dll
	cp -f $(NPATH)/Nemerle.dll .

clean:
	$(MAKE) -C simulator clean
	rm -f ants.exe antslib.dll antsmacro.dll serializer.dll Nemerle.dll
