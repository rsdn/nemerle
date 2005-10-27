include ../../config.mak
NCC = $(EXECUTE) ../../ncc/out.stage3/ncc.exe
SOURCES = console.n generator.n map.n person.n territory.n dice.n

all: TheGame.exe

TheGame.exe: $(SOURCES)
	$(NCC) -i -o $@ $(SOURCES)


clean:
	rm -f TheGame.exe
