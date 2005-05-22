THISDIR = $(shell if cygpath --help 2>/dev/null 1>&2; then echo `cygpath -m $(CURDIR)`; else echo $(CURDIR); fi)

############################################################
# TARGETS
############################################################

all:
	$(MAKE) TEST_FILES=$(THISDIR)/*.n -C ../../ncc/testsuite/ test

clean:
	rm -f *.exe *.il *.dll *.pdb *.netmodule ext_test.out core core.[0-9]*
	rm -f test.err test_error.log a.out
