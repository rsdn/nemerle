#
# Makefile
#

NCC = ncc.exe

SOURCES = sokoban.n main.n treesearch.n 
MACROS = macros.n

all: sokoban.exe

sokoban.exe: $(SOURCES) macros.dll
	$(NCC) -texe -r:macros.dll -o:$@ $(SOURCES)

macros.dll: $(MACROS)
	$(NCC) -tdll -r:Nemerle.Compiler -o:$@ $(MACROS)

clean:
	rm -f *.exe *.pdb *.dll
