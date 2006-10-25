# Nemerle Documentation Project

SOURCES = NemerleDoc.n XMLMacro.n DataTree.n AssemblyAnalyzer.n
DIST_FILES = $(SOURCES) Makefile COPYRIGHT
EXE = nemerledoc.exe
CLEAN_FILES = $(EXE) $(EXE:.exe=.exe.mdb)
DOC = nemerledoc.xml

$(EXE): $(SOURCES) 
	ncc -g -Ot -texe -doc:$(DOC) -out:$@ $^
#	mono $(EXE) $(EXE) $(DOC)

clean:
	rm  $(CLEAN_FILES)

dist:
	echo $(DIST_FILES)
	tar -zcf nemerle-doc.tgz $(DIST_FILES)
