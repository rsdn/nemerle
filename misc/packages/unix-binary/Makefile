TOP_LEVEL=../../..
include $(TOP_LEVEL)/config.mak

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = $(TOP_LEVEL)/ncc/out.stage3
NCC = $(EXECUTE) $(NCC_DIR)/ncc.exe

RELEASE = 1

DIR = nemerle-bin-$(VERSION).$(REVISION)-$(RELEASE)
SRCDIR = nemerle-$(VERSION).$(REVISION)


all: assembly-name.exe
	rm -rf $(DIR)/*
	install -d $(DIR)/{gac,bin,misc}
	tar zxf ../../../$(SRCDIR).tar.gz
	test -f $(SRCDIR)/doc/html/Grokking_Nemerle.html
	cp -rf $(SRCDIR)/doc/html $(DIR)/doc
	cp -f ../../../ncc/out.stage3/*.dll $(DIR)/gac/
	cp -f ../../../tools/cs2n/{antlr.runtime,Nemerle.CSharp{,.CS}}.dll $(DIR)/gac/
	cp -f ../../../tools/nemerlish/Nemerle.Evaluation.dll $(DIR)/gac/
	cp -f ../../../ncc/out.stage3/ncc.exe $(DIR)/bin/
	cp -f ../../../tools/cs2n/cs2n.exe $(DIR)/bin/
	cp -f ../../../tools/nemerlish/nemish.exe $(DIR)/bin/
	cp -f ../../../doc/ncc.1 $(DIR)/misc/
	cp -f ../../../tools/nant-task/Nemerle.NAnt.Tasks.dll $(DIR)/misc/
	cp -f ../../print-dir.build $(DIR)/misc/
	cp -f README $(DIR)/
	sed \
		-e 's/@version@/$(VERSION).$(REVISION)/' \
		-e "s/@build_info@/`date` on `hostname` by `whoami`/" \
		install-nemerle.sh > $(DIR)/install-nemerle.sh
	chmod 755 $(DIR)/install-nemerle.sh
	cp -f uninstall-nemerle.sh.head $(DIR)/misc/uninstall-nemerle.sh
	for f in $(DIR)/gac/*.dll ; do \
		echo -n . 1>&2 ; \
		echo "gacutil -package nemerle -u \"`$(EXECUTE) ./assembly-name.exe $$f`\"" ; \
	done >> $(DIR)/misc/uninstall-nemerle.sh
	@echo done
	cat uninstall-nemerle.sh.foot >> $(DIR)/misc/uninstall-nemerle.sh
	chmod 755 $(DIR)/misc/uninstall-nemerle.sh
	tar zcf $(DIR).tgz $(DIR)
	rm -rf $(DIR) $(SRCDIR)

assembly-name.exe: assembly-name.n
	$(NCC) -o $@ $<

clean:
	rm -rf assembly-name.exe nemerle-* install.log
