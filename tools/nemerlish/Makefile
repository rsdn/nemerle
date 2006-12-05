TOP_LEVEL = ../..

include $(TOP_LEVEL)/config.mak

INST	= @echo INSTALL $1

######################################################
#VARIABLES
######################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = $(TOP_LEVEL)/ncc/out.stage3
NCC = $(EXECUTE) $(NCC_DIR)/ncc.exe
SNK_COMPILER = -keyfile:$(TOP_LEVEL)/misc/keys/Nemerle.Compiler.snk

NEMISH_SRC = main.n interp.n readline.n

all:	Nemerle.Evaluation.dll nemish.exe

Nemerle.Evaluation.dll: eval.n $(NCC_DIR)/Nemerle.dll $(NCC_DIR)/Nemerle.Compiler.dll $(NCC_DIR)/Nemerle.Macros.dll 
	$(NCC) -r Nemerle.Compiler.dll -t library $(SNK_COMPILER) -o Nemerle.Evaluation.dll eval.n

nemish.exe: $(NEMISH_SRC) Nemerle.Evaluation.dll
	$(NCC) -r Nemerle.Evaluation.dll -o nemish.exe $(SNK_COMPILER) $(NEMISH_SRC)

check: all
	$(MAKE) -C tests all

withdoc: withdoc-dll withdoc-exe

withdoc-dll: eval.n
	$(NCC) -r Nemerle.Compiler.dll -t library -doc "Nemerle.Evaluation.dll.xml" -o Nemerle.Evaluation.dll eval.n

withdoc-exe: $(NEMISH_SRC)
	$(NCC) -r Nemerle.Evaluation.dll -doc "nemish.exe.xml" -o nemish.exe $(NEMISH_SRC)

install: all
	$(Q)$(GACUTIL_COMMAND) Nemerle.Evaluation.dll 
	$(INST) "[$(BINDIR)]" nemish.exe
	$(Q)$(INSTALL) -m 755 nemish.exe $(DESTDIR)$(BINDIR)/nemish.exe
# Set a `nemish' script if binfmt_misc is not available.
	$(Q)if [ "$(NET_ENGINE)" != "" ] ; then \
		echo "#!/bin/sh" > $(DESTDIR)$(SCRDIR)/nemish && \
		echo '$(NET_ENGINE) $(BINDIR)/nemish.exe "$$@"' >> $(DESTDIR)$(SCRDIR)/nemish ; \
		chmod 755 $(DESTDIR)$(SCRDIR)/nemish ; \
	fi


test-nemish: symlinks
	perl tests/test-nemish.pl "$(EXECUTE)" tests/*.test

uninstall:
	$(RM) Nemerle.Evaluation
	$(GACUTIL_UNINSTALL_COMMAND) Nemerle.Evaluation
	$(RM) $(BINDIR)/nemish
	$(Q)rm -f $(BINDIR)/nemish
	$(RM) $(BINDIR)/nemish.exe
	$(Q)rm -f $(BINDIR)/nemish.exe

symlinks:
	ln -sf ../../ncc/out.stage3/*.{dll,exe} .

clean:
	rm -f *.dll *.exe *~

