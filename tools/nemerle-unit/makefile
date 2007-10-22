TOP_LEVEL = ../..
NCC_DIR = $(TOP_LEVEL)/ncc/out.stage3
NCC = $(NCC_DIR)/ncc.exe -q

include $(TOP_LEVEL)/config.mak

build: bin/NemerleUnit.dll

bin/NemerleUnit.dll: bin src/macros/*.n
	$(NCC) -g -i  -t:library -r Nemerle.Compiler.dll \
		$(NUNIT_LIB_FLAG)\
		-o:$@ src/macros/*.n

test: bin/NemerleUnit.dll bin/fixture.dll bin/UnitTests.dll
	cd bin; $(NUNIT_CONSOLE) UnitTests.dll

bin/UnitTests.dll: src/tests/**.n src/example.n
	$(NCC) $^ -g -i -r bin/NemerleUnit.dll  -nowarn:10005 \
		-o:$@ -t:library -r Nemerle.Compiler.dll \
		-r lib/nunit.core.interfaces.dll \
		$(NUNIT_LIB_FLAG)\
		-r lib/nunit.core.dll

bin/fixture.dll: src/fixtures_for_tests/*.n
	$(NCC) $^ -i -nowarn:10005 -r bin/NemerleUnit.dll $(NUNIT_LIB_FLAG) \
		-t:library -o:$@


bin:
	mkdir -p bin

.PHONY: clean

clean:
	rm -r bin
