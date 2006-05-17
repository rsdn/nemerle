TOP=../..

include $(TOP)/config.mak

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
THISDIR = $(shell if cygpath --help 2>/dev/null 1>&2; then echo `cygpath -m $(CURDIR)`; else echo $(CURDIR); fi)
UNIT_SRC = linkedlist.n list.n heap.n set.n

############################################################
# TARGETS
############################################################

all: nunit
	$(MAKE) TEST_FILES=$(THISDIR)/*.n -C $(TOP)/ncc/testsuite/ test

clean:
	rm -f *.exe *.il *.dll *.pdb *.netmodule ext_test.out core core.[0-9]*
	rm -f test.err test_error.log a.out *xml

nunit: lib-unit-tests.dll
	$(Q)if test $(NUNIT_CONSOLE); then \
		cp -f $(TOP)/ncc/out.stage3/Nemerle.dll .; \
		$(NUNIT_CONSOLE) lib-unit-tests.dll; \
	fi

lib-unit-tests.dll: $(UNIT_SRC) 
	$(Q)if test $(NUNIT_CONSOLE); then \
		$(EXECUTE) $(TOP)/ncc/out.stage3/ncc.exe $(UNIT_SRC) -def:NUNIT $(NUNIT_LIB_FLAG) -t:library -o:lib-unit-tests.dll; \
	fi
