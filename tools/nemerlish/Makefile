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
# Set a `nemish' script if binfmt_misc is not available.
	$(Q)if [ "$(NET_ENGINE)" != "" ] ; then \
		echo "#!/bin/sh" > $(DESTDIR)$(BINDIR)/nemish && \
		echo '$(NET_ENGINE) $(BINDIR)/nemish.exe "$$@"' >> $(DESTDIR)$(BINDIR)/nemish ; \
		chmod 755 $(DESTDIR)$(BINDIR)/nemish ; \
	fi
	$(INST) "[$(HOME)]" .nemerlish_profile 
# Copy .nemerlish_profile to the users home directory setting apropriate ownership, based on
# the ownership of $HOME in case they used sudo for `make install'.
	$(Q)if [ ! -f "$(HOME)/.nemerlish_profile" ]; then \
		$(INSTALL) -m 644 -o `ls -ld "$(HOME)"|cut -d " " -f 4` \
                -g `ls -ld "$(HOME)"|cut -d " " -f 5` .nemerlish_profile $(HOME); \
        fi

# ~/.nemerlish_profile is _not_ deleted during uninstallation.
uninstall:
	$(RM) Nemerle.Evaluation
	$(GACUTIL_UNINSTALL_COMMAND) Nemerle.Evaluation
	$(RM) $(BINDIR)/nemish
	$(Q)rm -f $(BINDIR)/nemish
	$(RM) $(BINDIR)/nemish.exe
	$(Q)rm -f $(BINDIR)/nemish.exe

clean:
	rm -f *.dll *.exe *~

