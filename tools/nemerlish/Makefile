TOP_LEVEL = ../..

include $(TOP_LEVEL)/config.mak

INST	= @echo INSTALL $1

######################################################
#VARIABLES
######################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = $(TOP_LEVEL)/ncc/out.stage3
NCC = $(EXECUTE) $(NCC_DIR)/ncc.exe -debug

all:	nemish.exe

nemish.exe:  main.n eval.n
#	$(NCC) -r Nemerle.Compiler.dll -o nemish.exe eval.n main.n
	$(NCC) -r Nemerle.Compiler.dll -o nemish.exe eval.n interp.n main.n

install: nemish.exe
	$(INST) "[$(BINDIR)]" nemish.exe
	$(Q)$(INSTALL) -m 755 nemish.exe $(DESTDIR)$(BINDIR)/nemish.exe
	$(Q)if [ "$(NET_ENGINE)" != "" ] ; then \
		echo "#!/bin/sh" > $(DESTDIR)$(BINDIR)/nemish && \
		echo '$(NET_ENGINE) $(BINDIR)/nemish.exe "$$@"' >> $(DESTDIR)$(BINDIR)/nemish ; \
		chmod 755 $(DESTDIR)$(BINDIR)/nemish ; \
	fi

uninstall:
	$(RM) $(BINDIR)/nemish
	$(Q)rm -f $(BINDIR)/nemish
	$(RM) $(BINDIR)/nemish.exe
	$(Q)rm -f $(BINDIR)/nemish.exe


clean:
	rm -f *.dll *.exe *~

### These are for development purposes only
devel: evallib interplib mainexe

evallib: eval.n
	$(NCC) -r Nemerle.Compiler.dll -t library -o eval.dll eval.n

interplib: interp.n
	$(NCC) -r Nemerle.Compiler.dll -t library -o interp.dll interp.n

mainexe: main.n
	$(NCC) -d READLINE -r Mono.ReadLine.dll -r eval.dll -r interp.dll main.n
