#
# Makefile
#

include ../../config.mak

NPATH   = ../../ncc/out.stage3
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

SOURCES = sokoban.n main.n treesearch.n splayheap.n localsearch.n
MACROS = macros.n

all: sokoban.exe
	cp -f $(NPATH)/Nemerle.dll .
	$(MONO) ./sokoban.exe zestaw1.xml 1 IDFS
	$(MONO) ./sokoban.exe zestaw1.xml 2 IDFS
	$(MONO) ./sokoban.exe zestaw1.xml 3 IDFS
	$(MONO) ./sokoban.exe zestaw1.xml 4 IDFS
	$(MONO) ./sokoban.exe zestaw1.xml 5 IDFS
	make -C . clean

sokoban.exe: $(SOURCES) macros.dll
	$(COMPILE) -texe -r:macros.dll -o:$@ $(SOURCES)

macros.dll: $(MACROS)
	$(COMPILE) -tdll -r:Nemerle.Compiler -o:$@ $(MACROS)

clean:
	rm -f *.exe *.pdb *.dll
