#
# Copyright (c) 2003, 2004 The University of Wroclaw.
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
include config.mak

############################################################
# VARIABLES
############################################################

DISTFILES = \
	AUTHORS   \
	COPYRIGHT \
	ChangeLog \
	INSTALL   \
	Makefile  \
	README    \
	NEWS	  \
	configure \

svn2log = LC_ALL=pl_PL.utf-8 $(PYTHON) misc/svn2log.py changelog.xml -u misc/users
nemroot = /nemerle/(trunk|(branches|tags)/[^/]+)

############################################################
# OUTPUT
############################################################

MKDIR = @echo MKDIR $1
TAR   = @echo TAR   $1
CP    = @echo CP    $1

############################################################
# TARGETS
############################################################

# This is the default target.  It bootstraps compiler, and
# builds standard library.
all:
	$(Q)$(MAKE) -C ncc boot

# This is necessary to make sure, that configuration file
# has been generated, and it is up to date.
config.mak: configure
	./configure

send: dist send-dist

changelog:
	svn up
	svn log -v --xml > changelog.xml
	$(svn2log) -p '$(nemroot)' -r npc/ncc=ncc
	rm -f changelog.xml

sync-boot:
	$(MAKE) -C ncc boot sync
	svn commit -m "Sync for release." boot/

dist: changelog tarball

tarball:
	$(Q)rm -rf $(PACKAGE)-$(VERSION).*
	$(MKDIR) $(PACKAGE)-$(VERSION).$(REVISION)/...
	$(Q)mkdir $(PACKAGE)-$(VERSION).$(REVISION)
	$(Q)mkdir $(PACKAGE)-$(VERSION).$(REVISION)/ncc
	$(Q)mkdir $(PACKAGE)-$(VERSION).$(REVISION)/doc
	$(Q)mkdir $(PACKAGE)-$(VERSION).$(REVISION)/misc
	$(Q)mkdir $(PACKAGE)-$(VERSION).$(REVISION)/boot
	$(Q)mkdir $(PACKAGE)-$(VERSION).$(REVISION)/lib
	$(Q)mkdir $(PACKAGE)-$(VERSION).$(REVISION)/snippets
	$(Q)mkdir $(PACKAGE)-$(VERSION).$(REVISION)/macros
	$(CP)
	$(Q)cp $(DISTFILES) $(PACKAGE)-$(VERSION).$(REVISION)
	$(Q)$(MAKE) -C ncc  dist DIR=../$(PACKAGE)-$(VERSION).$(REVISION)
	$(Q)$(MAKE) -C doc  dist DIR=../$(PACKAGE)-$(VERSION).$(REVISION)/doc
	$(Q)$(MAKE) -C misc dist DIR=../$(PACKAGE)-$(VERSION).$(REVISION)/misc
	$(Q)$(MAKE) -C boot dist DIR=../$(PACKAGE)-$(VERSION).$(REVISION)/boot
	$(Q)$(MAKE) -C macros dist DIR=../$(PACKAGE)-$(VERSION).$(REVISION)/macros
	$(Q)$(MAKE) -C snippets dist DIR=$(shell pwd)/$(PACKAGE)-$(VERSION).$(REVISION)/snippets
	$(TAR) $(PACKAGE)-$(VERSION).$(REVISION).tar.gz 
	@tar zcf $(PACKAGE)-$(VERSION).$(REVISION).tar.gz $(PACKAGE)-$(VERSION).$(REVISION)
	@echo "Looking for missing files."
	svn export . $(PACKAGE)-$(VERSION).$(REVISION).svn
	@rm -rf $(PACKAGE)-$(VERSION).$(REVISION).svn/{doc,boot/lock.txt,misc,snippets/sioux/fit}
	@cd $(PACKAGE)-$(VERSION).$(REVISION).svn ; find . | sed -e 's/^\.\///' | sort > ../files.svn
	@cd $(PACKAGE)-$(VERSION).$(REVISION) ; find . | sed -e 's/^\.\///' | sort > ../files.tar
	@diff -u files.{svn,tar} | grep '^-[^-]' | sed -e 's/^-//' || echo "No files missing."
	@rm -f files.{svn,tar}
	@rm -rf $(PACKAGE)-$(VERSION).$(REVISION)
	@rm -rf $(PACKAGE)-$(VERSION).$(REVISION).svn

install:
	$(Q)$(MAKE) -C doc install
	$(Q)if test -f ncc/out.stage3/ncc.exe ; then $(MAKE) -C ncc install; \
            else $(MAKE) -C boot install; fi

uninstall:
	$(Q)$(MAKE) -C boot uninstall
	$(Q)$(MAKE) -C doc  uninstall

check:
	$(Q)$(MAKE) -C ncc  tests
	$(Q)$(MAKE) -C snippets all
	$(Q)$(MAKE) -C snippets/shootout all
	$(Q)$(MAKE) -C snippets/sioux all

clean:
	$(MAKE) -C doc clean
	$(MAKE) -C ncc clean
	rm -f config.mak configure.log

snapshot:
	$(MAKE) tarball REVISION=$(REVISION).`svn info | grep 'Revision:'|sed -e 's/.*://'|xargs`
