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
include ../config.mak

############################################################
# TARGETS
############################################################

all:
	$(Q)if test $(ANTLR); then \
		$(MAKE) -C cs2n all; \
	elif find cs2n/CSharpParser.cs -cnewer cs2n/csharpgrammar.g | grep -q "" ; then \
		$(MAKE) -C cs2n all; \
	fi
	$(Q)$(MAKE) -C nemerlish all
	$(Q)$(MAKE) -C nant-task all
	$(Q)$(MAKE) -C msbuild-task all

install: all
	$(Q)if test -f cs2n/cs2n.exe ; then \
	  $(MAKE) -C cs2n install; fi
	$(Q)$(MAKE) -C nemerlish install
	$(Q)$(MAKE) -C nant-task all install
#	$(Q)$(MAKE) -C msbuild-task all install

uninstall:
	$(Q)-$(MAKE) -C cs2n uninstall
	$(Q)-$(MAKE) -C nemerlish uninstall
#	$(Q)-$(MAKE) -C nant-task uninstall
#	$(Q)-$(MAKE) -C msbuild-task uninstall

clean:
	$(MAKE) -C cs2n clean
	$(MAKE) -C nemerlish clean
	$(MAKE) -C nant-task clean
	$(MAKE) -C htmldumper clean
	$(MAKE) -C msbuild-task clean
