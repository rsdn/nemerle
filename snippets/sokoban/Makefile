#
# Makefile
#

include ../../config.mak

NPATH   = ../../ncc/out.stage3
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

export PATH = $(shell echo $$PATH):$(shell pwd)/$(NPATH)/
export MONO_PATH = $(shell echo $$MONO_PATH):$(shell pwd)/$(NPATH)/

SOURCES = sokoban.n main.n treesearch.n 
MACROS = macros.n

all: sokoban.exe
	$(MONO) sokoban.exe zestaw1.xml 1 IDFS
	$(MONO) sokoban.exe zestaw1.xml 2 IDFS
	$(MONO) sokoban.exe zestaw1.xml 3 IDFS
	$(MONO) sokoban.exe zestaw1.xml 4 IDFS
	$(MONO) sokoban.exe zestaw1.xml 5 IDFS

sokoban.exe: $(SOURCES) macros.dll
	$(COMPILE) -texe -r:macros.dll -o:$@ $(SOURCES)

macros.dll: $(MACROS)
	$(COMPILE) -tdll -r:Nemerle.Compiler -o:$@ $(MACROS)

clean:
	rm -f *.exe *.pdb *.dll
