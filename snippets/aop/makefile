TOP_LEVEL = ../..

include $(TOP_LEVEL)/config.mak

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = $(TOP_LEVEL)/ncc/out.stage3
NCC = $(EXECUTE) $(NCC_DIR)/ncc.exe -q


build: bin bin/DevMacros.dll bin/Nemerle.Aop.dll

bin/DevMacros.dll: bin src/dev_macros/parser_operators.n 
	$(NCC) -g -t:library -r Nemerle.Compiler.dll -o:$@\
		src/dev_macros/parser_operators.n

bin/Nemerle.Aop.dll: bin bin/DevMacros.dll src/macros/*.n src/helper/*.n src/pubapi/*.n
	$(NCC) --no-color -g -t:library -r Nemerle.Compiler.dll \
		-m bin/DevMacros.dll -o:$@\
		src/macros/*.n src/helper/*.n src/pubapi/*.n


test: utests ftests

# unit tests
utests: bin/UnitTests.dll
	cp $(NCC_DIR)/Nemerle.dll bin/
	cp $(NCC_DIR)/Nemerle.Compiler.dll bin/
	cd bin;	$(NUNIT_CONSOLE) UnitTests.dll

bin/UnitTests.dll: bin/Nemerle.Aop.dll bin/NemerleUnit.dll src/utests/*.n
	$(NCC) src/utests/*.n -r bin/NemerleUnit.dll -m bin/DevMacros.dll -nowarn:10005 \
		-o:$@ -t:library -r Nemerle.Compiler.dll -r bin/Nemerle.Aop.dll \
		$(NUNIT_LIB_FLAG)

bin/NemerleUnit.dll: $(TOP_LEVEL)/tools/nemerle-unit/bin/NemerleUnit.dll
	cp $< bin/NemerleUnit.dll

../../tools/nemerle-unit/bin/NemerleUnit.dll:
	cd $(TOP_LEVEL)/tools/nemerle-unit; make

# functional tests
ftests: 
	make functional_tests

# to run a single functional test with verbose param
# you must give value for NAME var (part of the filename)
vtest: 
	make functional_tests ARG=-vv

functional_tests: testbin testbin/test.exe bin/Nemerle.Aop.dll
	cp bin/Nemerle.Aop.dll testbin/Nemerle.Aop.dll
	cp $(NCC_DIR)/Nemerle.Macros.dll testbin/Nemerle.Macros.dll
	cp $(NCC_DIR)/Nemerle.dll testbin/
	cp $(NCC_DIR)/Nemerle.Compiler.dll testbin/
	cd testbin; find ../src/ftests -name  "*$(NAME)*_positive.n" \
		-exec $(EXECUTE) ./test.exe $(ARG) -ref Nemerle.Aop.dll {} +
	cd testbin; find ../src/ftests -name "*$(NAME)*_negative.n" \
		-exec $(EXECUTE) ./test.exe $(ARG) -ref Nemerle.Aop.dll {} +

testbin/test.exe: testbin
	$(NCC) -o:$@ -r Nemerle.Compiler.dll $(TOP_LEVEL)/ncc/testsuite/test.n

bin:
	mkdir -p bin 
testbin:
	mkdir -p testbin

.PHONY: clean build test ftests utests functional_tests

clean:
	rm -fr bin testbin
