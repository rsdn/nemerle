XSPP = xspp
XSLT = xsltproc --xinclude
MAKE_GRAMMAR = perl scripts/grammar-txt-2-xml.pl $< | xmllint -o $@ --format -

course_docs = index

root_docs =  index nemerlePlanned \
	coreNemExamples coreNemGrammar coreNemCompiler coreNemDesc \
	TRSideas faq \
	absNem absNemSemantics absNemGrammar

docs = $(root_docs) $(addprefix course/, $(course_docs))

tex_docs = absNemOperationalSem

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
	
nemerleGrammar.tex nemerleGrammar.html: nemerleGrammar.xml
coreNemGrammar.tex coreNemGrammar.html: coreNemGrammar.xml
absNemGrammar.tex absNemGrammar.html: absNemGrammar.xml

nemerleGrammar.xml: src/nemerleGrammar.txt scripts/grammar-txt-2-xml.pl
	$(MAKE_GRAMMAR)

absNemGrammar.xml: src/absNemGrammar.txt scripts/grammar-txt-2-xml.pl
	$(MAKE_GRAMMAR)

coreNemGrammar.xml: src/coreNemGrammar.txt scripts/grammar-txt-2-xml.pl
	$(MAKE_GRAMMAR)
	
%.ps: %.dvi
	dvips -o $@ $<

%.pdf: %.ps
	ps2pdf $<

%.dvi: src/%.tex
	latex $< < /dev/null
	
%.dvi: %.tex
	latex $< < /dev/null

%.tex: %.docb styles/docb2latex.xsl
	$(XSLT) -o $@ styles/docb2latex.xsl $<

v: verify-html

verify-html:
	set -xe; for f in *.html course/*.html ; do xmllint --noout --valid $$f ; done
	
verify-docb:
	set -xe; for f in *.docb ; do xmllint --noout --valid $$f ; done

clean:
	rm -f *.xsl *.html *.ps *.pdf *.aux *.log *.xml *.tex *.docb
	$(MAKE) -C tex clean

.PHONY: www publish

www: nemerle-web.tar.gz

nemerle-web.tar.gz: $(normal_docs) $(wildcard styles/*.css styles/*.png)
	$(MAKE) verify-html
	rm -rf www
	mkdir -p www/{styles,course}
	cp styles/*.{css,png} www/styles/
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
