all:
	latex fit.tex < /dev/null
	dvips -o fit.ps fit.dvi
	gv fit.ps

clean:
	rm -f *.aux *.dvi *.pdf *.ps *.log *.tmp *~
