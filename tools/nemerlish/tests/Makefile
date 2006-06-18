
TOPDIR = ../../..

############################################################
# VARIABLES
############################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
THISDIR = $(shell if cygpath --help 2>/dev/null 1>&2; then echo `cygpath -m $(CURDIR)`; else echo $(CURDIR); fi)


############################################################
# TARGETS
############################################################

all: 
	cp ../Nemerle.Evaluation.dll $(TOPDIR)/ncc/testsuite/
	$(Q)$(MAKE) TEST_FILES=$(THISDIR)/*.n -C $(TOPDIR)/ncc/testsuite/ test

clean:
	rm -f *.exe *.il *.dll 
