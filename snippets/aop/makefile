
# I use only nunit vars, I don't know how to utilize others yet
include ../../config.mak

build: bin bin/DevMacros.dll bin/Nemerle.Aop.dll

bin/DevMacros.dll: bin src/dev_macros/parser_operators.n 
	ncc -g -t:library -r Nemerle.Compiler.dll -o:$@\
		src/dev_macros/parser_operators.n

bin/Nemerle.Aop.dll: bin bin/DevMacros.dll src/macros/*.n src/helper/*.n
	ncc -g -t:library -r Nemerle.Compiler.dll -m bin/DevMacros.dll -o:$@\
		src/macros/*.n src/helper/*.n


test: utests ftests

# unit tests
utests: bin/UnitTests.dll
	cd bin; $(NUNIT_CONSOLE) UnitTests.dll

bin/UnitTests.dll: bin/Nemerle.Aop.dll bin/NemerleUnit.dll src/utests/*.n
	ncc src/utests/*.n -g -r bin/NemerleUnit.dll -m bin/DevMacros.dll -nowarn:10005 \
		-o:$@ -t:library -r Nemerle.Compiler.dll -r bin/Nemerle.Aop.dll \
		$(NUNIT_LIB_FLAG)

bin/NemerleUnit.dll: ../../tools/nemerle-unit/bin/NemerleUnit.dll
	cp $< bin/NemerleUnit.dll

../../tools/nemerle-unit/bin/NemerleUnit.dll:
	cd ../../tools/nemerle-unit; make

# functional tests
ftests: 
	make functional_tests

# to run a single functional test with verbose param
# you must give value for NAME var (part of the filename)
vtest: 
	make functional_tests ARG=-vv

functional_tests: testbin testbin/test.exe bin/Nemerle.Aop.dll
	cp {bin,testbin}/Nemerle.Aop.dll
	cp {../../ncc/out.stage3,testbin}/Nemerle.Macros.dll
	cd testbin; find ../src/ftests -name  "*$(NAME)*_positive.n" \
		-exec ./test.exe $(ARG) -ref Nemerle.Aop.dll {} +
	cd testbin; find ../src/ftests -name "*$(NAME)*_negative.n" \
		-exec ./test.exe $(ARG) -ref Nemerle.Aop.dll {} +

testbin/test.exe: testbin
	ncc -o:$@ -r Nemerle.Compiler.dll ../../ncc/testsuite/test.n

bin:
	mkdir -p bin 
testbin:
	mkdir -p testbin

.PHONY: clean build test ftests utests functional_tests

clean:
	rm -r bin testbin
