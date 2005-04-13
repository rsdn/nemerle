all:
	pdflatex main.tex</dev/null
	gv main.pdf

clean:
	rm -f *.{aux,dvi,pdf,log,snm,out,toc,nav,ps}

notes:
	latex notes.tex </dev/null
	latex notes.tex </dev/null
	dvips -o notes.ps notes.dvi
	psnup -2 notes.ps notes2.ps
