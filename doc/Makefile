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
include ../config.mak

############################################################
# VARIABLES
############################################################

tex_docs = metaprogramming intro

normal_docs = $(addsuffix .ps, $(tex_docs)) $(addsuffix .pdf, $(tex_docs))

ps = $(addsuffix .ps, $(docs)) $(addsuffix .ps, $(tex_docs))
pdf = $(addsuffix .pdf, $(docs)) $(addsuffix .pdf, $(tex_docs))
docb = $(addsuffix .docb, $(docs))

############################################################
# OUTPUT
############################################################

INST  = @echo INSTALL $1
MKDIR = @echo MKDIR   $1
TAR   = @echo TAR     $1
CP    = @echo CP      $1
RM    = @echo RM      $1

############################################################
# TARGETS
############################################################

.DELETE_ON_ERROR:

all: $(normal_docs)

# This is necessary to make sure, that configuration file
# has been generated, and it is up to date.
../config.mak:
	$(MAKE) -C .. config.mak

pdf: $(ps) $(pdf)

%.ps: %.dvi
	dvips -o $@ $<

%.pdf: %.ps
	ps2pdf $<

%.dvi: presentation/net-workshop-2004/%.tex
	TEXINPUTS=.:presentation/net-workshop-2004: latex $< < /dev/null
	TEXINPUTS=.:presentation/net-workshop-2004: latex $< < /dev/null

%.dvi: presentation/macros-gpce04/%.tex
	TEXINPUTS=.:presentation/macros-gpce04: latex $< < /dev/null
	TEXINPUTS=.:presentation/macros-gpce04: latex $< < /dev/null

install:
	$(Q)if test ! -d $(DESTDIR)$(MANDIR); then $(INSTALL) -d $(DESTDIR)$(MANDIR); fi
	$(INST) [$(MANDIR)] ncc.1
	$(Q)$(INSTALL) -m 644 ncc.1 $(DESTDIR)$(MANDIR)/ncc.1

uninstall:
	$(RM) $(MANDIR)/ncc.1
	$(Q)rm -f $(MANDIR)/ncc.1

clean:
	rm -f *.ps *.pdf *.aux *.log

.PHONY: www publish

www: nemerle-web.tar.gz

nemerle-web.tar.gz: $(normal_docs) $(wildcard styles/*.js styles/*.css styles/*.png styles/*.jpg)
	rm -rf www
	mkdir -p www/{styles,images}
	cp styles/*.{css,png,js} www/styles/
	cp images/* www/images/
	cp *.{ps,pdf} www/
	for f in www/*.pdf www/*.ps ; do gzip <$$f > $$f.gz ; done
	tar -C www -zcf $@ .
	rm -rf www

publish: publish-stamp

publish-stamp: nemerle-web.tar.gz
	cp nemerle-web.tar.gz /home/services/nemerle.org
	(cd /home/services/nemerle.org; tar zxf nemerle-web.tar.gz)
	touch publish-stamp

build-ndoc:
	$(MAKE) -C ../ncc doc
	ndoc -project=../misc/Nemerle.ndoc.xml

publish-ndoc: build-ndoc
	(cd ndoc.output; tar cf - .) | (cd /home/services/nemerle.org/doc; tar xf -)

dist-cleaner: all
	$(MKDIR) html
	$(Q)mkdir -p html
	$(Q)mkdir -p html/styles
	$(CP) html
	$(Q)cp *.pdf html
	$(CP) css
	$(Q)cp styles/*.{css,png,js} html/styles
