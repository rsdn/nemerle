latex:
	xspp xml2tex.xspp > xml2tex.xsl
	xspp latex.xspp > latex.xsl
	xsltproc latex.xsl ../src/coreNemSemantics.xml > tmp.xml
	xsltproc xml2tex.xsl tmp.xml > tmp.tex
	perl -i fixup-spaces.pl tmp.tex
	latex tmp.tex </dev/null
	dvi tmp.dvi
	
xsl-fo:
	xspp pdf.xspp > pdf.xsl
	xsltproc --xinclude pdf.xsl ../src/miniNemSemantics.xml | xmllint --format - > index.fo
	xmllint --noout --dtdvalid ./fo.dtd index.fo
	pdfxmltex index.fo
	gv index.pdf

clean:
	rm -f *.tex *.log *.xsl tmp.*
