#
# Copyright (c) 2005 The University of Wroclaw.
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
TOP = ../..
include $(TOP)/config.mak

############################################################
# VARIABLES
############################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)

############################################################
# OUTPUT
############################################################

MKDIR = @echo MKDIR $1
TAR   = @echo TAR   $1
CP    = @echo CP    $1
INST  = @echo INSTALL $1

############################################################
# TARGETS
############################################################

ifeq ($(MSBUILD),)
all:
	$(Q)true
	
install:
	$(Q)true
else
all: Nemerle.MSBuild.Tasks.dll

install: all
	$(INST) Nemerle.MSBuild.Tasks.dll
	$(Q)install -m755 Nemerle.MSBuild.Tasks.dll $(DESTDIR)$(BINDIR)
	$(Q)install -m644 Nemerle.MSBuild.targets $(DESTDIR)$(BINDIR)
endif

Nemerle.MSBuild.Tasks.dll: MSBuildTask.n
	$(EXECUTE) $(TOP)/ncc/out.stage3/ncc.exe -q -r:Microsoft.Build.Tasks.dll -out $@ -t:library MSBuildTask.n

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/Nemerle.MSBuild.Tasks.dll
	rm -f $(DESTDIR)$(BINDIR)/Nemerle.MSBuild.targets

clean:
	rm -f *.exe *.dll core core.[0-9]*
