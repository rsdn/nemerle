#
# Copyright (c) 2004 The University of Wroclaw.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#    1. Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#    3. The name of the University may not be used to endorse or promote
#       products derived from this software without specific prior
#       written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
# NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# Include configuration determined by configure script.
include ../config.mak

############################################################
# VARIABLES
############################################################

DISTFILES = \
	Makefile             \
	Nemerle.Compiler.dll \
	Nemerle.dll          \
	ncc.exe              \
	stdmacros.dll        \

############################################################
# OUTPUT
############################################################

INST = @echo INSTALL $1
CP   = @echo CP      $1
RM   = @echo RM      $1

############################################################
# TARGETS
############################################################

# Default rule is to do nothing.
all:


# This is necessary to make sure, that configuration file
# has been generated, and it is up to date.
../config.mak:
	$(Q)cd .. && $(MAKE) config.mak


dist:
	$(CP)
	$(Q)cp $(DISTFILES) $(DIR)

install:
	$(Q)if test ! -d $(LIBDIR); then mkdir $(LIBDIR); fi
	$(Q)if test ! -d $(BINDIR); then mkdir $(BINDIR); fi
	$(INST) [$(LIBDIR)] Nemerle.Compiler.dll
	$(Q)$(INSTALL) -m 644 Nemerle.Compiler.dll $(LIBDIR)/Nemerle.Compiler.dll
	$(INST) [$(LIBDIR)] Nemerle.dll
	$(Q)$(INSTALL) -m 644 Nemerle.dll $(LIBDIR)/Nemerle.dll
	$(INST) [$(BINDIR)] ncc
	$(Q)$(INSTALL) -m 755 ncc.exe $(BINDIR)/ncc
	$(INST) [$(LIBDIR)] stdmacros.dll
	$(Q)$(INSTALL) -m 644 stdmacros.dll $(LIBDIR)/stdmacros.dll

uninstall:
	$(RM) $(LIBDIR)/Nemerle.Compiler.dll
	$(Q)rm -f $(LIBDIR)/Nemerle.Compiler.dll
	$(RM) $(LIBDIR)/Nemerle.dll
	$(Q)rm -f $(LIBDIR)/Nemerle.dll
	$(RM) $(BINDIR)/ncc
	$(Q)rm -f $(BINDIR)/ncc
	$(RM) $(LIBDIR)/stdmacros.dll
	$(Q)rm -f $(LIBDIR)/stdmacros.dll

