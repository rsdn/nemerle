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

include ../config.mak

XSPP = xspp
XSLT = xsltproc --xinclude

course_docs = index

root_docs = index faq license hacking macros reference

docs = $(root_docs) $(addprefix course/, $(course_docs))

tex_docs = intro

normal_docs = $(html) $(addsuffix .ps, $(tex_docs)) $(addsuffix .pdf, $(tex_docs))

html = $(addsuffix .html, $(docs))
ps = $(addsuffix .ps, $(docs)) $(addsuffix .ps, $(tex_docs))
pdf = $(addsuffix .pdf, $(docs)) $(addsuffix .pdf, $(tex_docs))
docb = $(addsuffix .docb, $(docs))

.DELETE_ON_ERROR:
.SECONDARY: mainStyle.xsl latex.xsl xml2tex.xsl docb.xsl $(addsuffix .tex, $(docs))

all: $(normal_docs)

pdf: $(ps) $(pdf)

%.html: src/%.xml mainStyle.xsl
	$(XSLT) -o $@ mainStyle.xsl $<

course/%.html: course-src/%.xml mainStyle.xsl
	$(XSLT) -o $@ mainStyle.xsl $<

%.docb: src/%.xml docb.xsl
	$(XSLT) -o - docb.xsl $< | xmllint --format - > $@

%.xsl: styles/%.xspp
	$(XSPP) $< > $@

%.xsl: tex/%.xspp
	$(XSPP) $< > $@

%.ps: %.dvi
	dvips -o $@ $<

%.pdf: %.ps
	ps2pdf $<

%.dvi: src/%.tex
	latex $< < /dev/null
	latex $< < /dev/null

%.dvi: %.tex
	latex $< < /dev/null
	latex $< < /dev/null

%.tex: %.docb styles/docb2latex.xsl
	$(XSLT) -o $@ styles/docb2latex.xsl $<

v: verify-html

verify-html:
	set -xe; for f in *.html course/*.html ; do xmllint --noout --valid $$f ; done

verify-docb:
	set -xe; for f in *.docb ; do xmllint --noout --valid $$f ; done

install: $(html) ncc.1
	$(INSTALL) -m 644 ncc.1 $(MANDIR)/ncc.1

../config.mak:
	$(MAKE) -C .. config.mak

clean:
	rm -f *.xsl *.html *.ps *.pdf *.aux *.log *.xml *.tex *.docb
	rm -f course/*.html
	$(MAKE) -C tex clean

.PHONY: www publish

www: nemerle-web.tar.gz

nemerle-web.tar.gz: $(normal_docs) $(wildcard styles/*.css styles/*.png)
	$(MAKE) verify-html
	rm -rf www
	mkdir -p www/{styles,course/styles}
	cp styles/*.{css,png} www/styles/
	cp styles/*.{css,png} www/course/styles/
	cp *.{html,ps,pdf} www/
	cp course/*.html www/course/
	for f in www/*.pdf www/*.ps ; do gzip <$$f > $$f.gz ; done
	tar -C www -zcf $@ .
	rm -rf www

publish: publish-stamp

publish-stamp: nemerle-web.tar.gz
	cp nemerle-web.tar.gz /home/services/httpd/html
	(cd /home/services/httpd/html; tar zxf nemerle-web.tar.gz)
	touch publish-stamp
