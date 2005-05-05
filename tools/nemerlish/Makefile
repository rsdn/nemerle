TOP_LEVEL = ../..

include $(TOP_LEVEL)/config.mak

INST	= @echo INSTALL $1

######################################################
#VARIABLES
######################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = $(TOP_LEVEL)/ncc/out.stage3
NCC = $(EXECUTE) $(NCC_DIR)/ncc.exe -debug

all:	Nemerle.Evaluation.dll Nemerle.Evaluation.Interpreter.dll nemish.exe

Nemerle.Evaluation.dll: eval.n
	$(NCC) -r Nemerle.Compiler.dll -t library -o Nemerle.Evaluation.dll eval.n

Nemerle.Evaluation.Interpreter.dll: interp.n
	$(NCC) -r Nemerle.Compiler.dll -t library -o Nemerle.Evaluation.Interpreter.dll interp.n

nemish.exe: main.n
	$(NCC) -r Nemerle.Evaluation.dll -r Nemerle.Evaluation.Interpreter.dll -o nemish.exe main.n

devel-nemish: main.n
	$(NCC) -d READLINE -r Mono.ReadLine.dll -r Nemerle.Evaluation.dll -r Nemerle.Evaluation.Interpreter.dll -o nemish.exe main.n

install: all
	$(Q)$(GACUTIL_COMMAND) Nemerle.Evaluation.dll 
	$(Q)$(GACUTIL_COMMAND) Nemerle.Evaluation.Interpreter.dll 
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
	$(RM) Nemerle.Evaluation.Interpreter
	$(GACUTIL_UNINSTALL_COMMAND) Nemerle.Evaluation.Interpreter
	$(RM) $(BINDIR)/nemish
	$(Q)rm -f $(BINDIR)/nemish
	$(RM) $(BINDIR)/nemish.exe
	$(Q)rm -f $(BINDIR)/nemish.exe

clean:
	rm -f *.dll *.exe *~

