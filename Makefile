#
# Copyright (c) 2003 The University of Wroclaw.
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

include config.mak

all:
	$(MAKE) -C ncc boot

send: dist send-dist

name    = nemerle
svn2log = $(PYTHON) aux/svn2log.py changelog.xml -u aux/users

config.mak: configure
	./configure

changelog:
	svn up
	svn log -v --xml > changelog.xml
	$(svn2log) -p /nemerle/trunk -x npc -x doc -x ncc
	$(svn2log) -p /nemerle/trunk/ncc -o ncc/ChangeLog -r npc/ncc=ncc
	$(svn2log) -p /nemerle/trunk/doc -o doc/ChangeLog
	rm -f changelog.xml

sync-boot:
	$(MAKE) -C ncc boot sync

dist: sync-boot changelog
	if svn status 2>&1 | grep -qv '^?' ; then \
	  echo "Some files modified"; \
	  false; \
	else \
	  ver=`svn info . | awk '/^Revision:/ { print $$2 }'`; \
	  set -e; \
	  rm -rf $(name)-$$ver; \
	  svn export . $(name)-$$ver; \
	  for f in . ncc doc ; do \
	    cp $$f/ChangeLog $(name)-$$ver/$$f; \
	  done; \
	  cp boot/ncc.exe $(name)-$$ver/boot/; \
	  cp ncc/parser.cs $(name)-$$ver/ncc/; \
	  tar zcf $(name)-$$ver.tar.gz $(name)-$$ver; \
	  rm -rf $(name)-$$ver; \
	  ls -l $(name)-$$ver.tar.gz; \
	fi

send-dist:
	if [ "X`find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'`" = X ] ; then \
		echo "No files available" ; \
		false ; fi
	scp `find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'` \
		lilith:/home/services/httpd/html/download/
	scp `find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz' | sort | tail -1` \
		lilith:/home/services/httpd/html/download/nemerle-latest.tar.gz
	mv `find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'` ~/backup/nemerle/
	scp boot/ncc.exe lilith:/home/services/httpd/html/download/ncc-boot.exe

install:
	$(MAKE) -C ncc install
	$(MAKE) -C doc install

clean:
	$(MAKE) -C doc clean
	$(MAKE) -C ncc clean
	rm -f config.mak
