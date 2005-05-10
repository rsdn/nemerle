TOP_LEVEL = ../..

include $(TOP_LEVEL)/config.mak

INST	= @echo INSTALL $1

######################################################
#VARIABLES
######################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = $(TOP_LEVEL)/ncc/out.stage3
NCC = $(EXECUTE) $(NCC_DIR)/ncc.exe -debug

NEMISH_SRC = interp.n main.n readline.n

all:	Nemerle.Evaluation.dll nemish.exe

Nemerle.Evaluation.dll: 
	$(NCC) -r Nemerle.Compiler.dll -t library -o Nemerle.Evaluation.dll eval.n

nemish.exe: $(NEMISH_SRC)
	$(NCC) -r Nemerle.Evaluation.dll -o nemish.exe $(NEMISH_SRC)

install: all
	$(Q)$(GACUTIL_COMMAND) Nemerle.Evaluation.dll 
	$(INST) "[$(BINDIR)]" nemish.exe
	$(Q)$(INSTALL) -m 755 nemish.exe $(DESTDIR)$(BINDIR)/nemish.exe
	$(Q)if [ "$(NET_ENGINE)" != "" ] ; then \
		echo "#!/bin/sh" > $(DESTDIR)$(BINDIR)/nemish && \
		echo '$(NET_ENGINE) $(BINDIR)/nemish.exe "$$@"' >> $(DESTDIR)$(BINDIR)/nemish ; \
		chmod 755 $(DESTDIR)$(BINDIR)/nemish ; \
	fi

uninstall:
	$(RM) Nemerle.Evaluation
	$(GACUTIL_UNINSTALL_COMMAND) Nemerle.Evaluation
	$(RM) $(BINDIR)/nemish
	$(Q)rm -f $(BINDIR)/nemish
	$(RM) $(BINDIR)/nemish.exe
	$(Q)rm -f $(BINDIR)/nemish.exe

clean:
	rm -f *.dll *.exe *~

