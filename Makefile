#
# Copyright (c) 2003-2005 The University of Wroclaw.
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

svn2log = LC_ALL=pl_PL.utf-8 $(PYTHON) misc/svn2log.py changelog.xml -u misc/users
nemroot = /nemerle/(trunk|(branches|tags)/[^/]+)

# files that contain System.Reflection.AssemblyVersion attribute
version_files = \
	ncc/misc/AssemblyInfo.n \
	macros/AssemblyInfo.n \
	lib/AssemblyInfo.n \
	ncc/main.n

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
	last=`perl -e '$$_ = <>; /\[r(\d+-)?(\d+)\]/ and print "$$2\n"' ChangeLog 2>/dev/null`; \
	if [ X$$last = X ] ; then last=0 ; fi ; \
	last=$$(($$last + 1)) ; \
	echo "Loggin from $$last."; \
	svn log -r HEAD:$$last -v --xml > changelog.xml
	$(svn2log) -o ChangeLog.new -p '$(nemroot)' -r npc/ncc=ncc
	cat ChangeLog > ChangeLog.old 2>/dev/null || :
	cat ChangeLog.new ChangeLog.old > ChangeLog
	rm -f ChangeLog.old ChangeLog.new changelog.xml

sync-boot:
	$(MAKE) -C ncc boot sync
	svn commit -m "Sync for release." boot/

dist: changelog tarball

tarball:
	$(Q)rm -rf $(PACKAGE)-$(VERSION).*
	svn export . $(PACKAGE)-$(VERSION).$(REVISION)
	-cp ChangeLog $(PACKAGE)-$(VERSION).$(REVISION)
	$(Q)$(MAKE) -C $(PACKAGE)-$(VERSION).$(REVISION) dist-cleaner
	$(TAR) $(PACKAGE)-$(VERSION).$(REVISION).tar.gz 
	@tar zcf $(PACKAGE)-$(VERSION).$(REVISION).tar.gz $(PACKAGE)-$(VERSION).$(REVISION)
	$(Q)rm -rf $(PACKAGE)-$(VERSION).$(REVISION)

dist-cleaner:
	@echo Setting up html doc.
	$(Q)$(MAKE) -C doc dist-cleaner
	$(Q)$(MAKE) -C tools/cs2n dist
	@echo Cleaning non-dist junk.
	$(Q)$(MAKE) clean
	$(Q)rm -rf doc/course{,-src} doc/images
	$(Q)rm -rf doc/presentation
	$(Q)rm -f config.mak configure.log

install:
	$(Q)$(MAKE) -C doc install
	$(Q)if test -f ncc/out.stage3/ncc.exe ; then $(MAKE) -C ncc install; \
            else $(MAKE) -C boot install; fi
	$(Q)if test $(ANTLR); then \
	  $(MAKE) NCC_DIR=$(PWD)/boot/ -C tools/cs2n install; fi

uninstall:
	$(Q)$(MAKE) -C tools/cs2n uninstall
	$(Q)$(MAKE) -C boot uninstall
	$(Q)$(MAKE) -C doc  uninstall

check:
	$(Q)$(MAKE) -C ncc  tests
	$(Q)$(MAKE) -C snippets clean
	$(Q)$(MAKE) -C snippets all
	$(Q)$(MAKE) -C ncc/testsuite/frommcs all
	$(Q)$(MAKE) -C ncc/testsuite/ clean

clean:
	$(MAKE) -C doc clean
	$(MAKE) -C ncc clean
	$(MAKE) -C snippets clean
	rm -f config.mak configure.log

set-version: config.mak
	@echo -n "Setting version to $(VERSION).$(REVISION) "
	$(Q)perl -p -i -e 's/(\"Nemerle Compiler \(ncc\) version) [0-9.]+ \([^\)]+\)\\n"/$$1 $(VERSION).$(REVISION) (MARK-SET-VER)\\n"/' ncc/main.n
	$(Q)[ `grep -c MARK-SET-VER ncc/main.n` = 1 ] || \
		{ echo "Failed to set text version on ncc/main.n"; exit 1; }
	$(Q)kind=`if echo $(REVISION) | grep -q 99 ; then echo SVN ; else echo release ; fi` ; \
	    echo -n "($$kind) " ; \
	    perl -p -i -e "s/MARK-SET-VER/$$kind/" ncc/main.n
	$(Q)for f in $(version_files) ; do \
		echo -n "."; \
		perl -p -i -e \
		 's/AssemblyVersion\("[^"]+"\)/AssemblyVersion("$(VERSION).$(REVISION)")MARK-SET-VER/' \
		 $$f ; \
		: check if we succeed ; \
		grep -q MARK-SET-VER $$f || { echo "Failed to set version on $$f"; exit 1; } ; \
		: clean up ; \
		perl -p -i -e 's/MARK-SET-VER//' $$f ; \
	done
	@echo .

snapshot:
	$(MAKE) tarball REVISION=$(REVISION).`svn info | grep 'Revision:'|sed -e 's/.*://'|xargs`
