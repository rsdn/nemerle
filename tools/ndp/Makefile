# Nemerle Documentation Project
#
# $ make test
# tests this program on itself.

SOURCES = NemerleDoc.n XMLMacro.n DataTree.n AssemblyAnalyzer.n TestNemerledoc.n
DIST_FILES = $(SOURCES) Makefile COPYRIGHT
EXE = nemerledoc.exe
CLEAN_FILES = $(EXE) $(EXE:.exe=.exe.mdb)
DOC = nemerledoc.xml

$(EXE): $(SOURCES) 
	ncc -g -Ot -texe -resource:nemerle-doc.css -doc:$(DOC) -out:$@ $^

test:
	mono --debug $(EXE) -dest:"wynik" -title:"Nemerle Documentation Project" -s -d -p $(EXE) $(DOC)

clean:
	rm  $(CLEAN_FILES)

dist:
	echo $(DIST_FILES)
	tar -zcf nemerle-doc.tgz $(DIST_FILES)
