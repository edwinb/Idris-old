all: idris.pdf

SOURCES = idris.tex literature.bib intro.tex conclusion.tex

idris.pdf: ${SOURCES}
#	dvipdf idris.dvi
	pdflatex idris
	-bibtex idris
	-pdflatex idris

idris.ps: idris.dvi
	dvips -o idris.ps idris

idris.dvi: $(SOURCES)
	-latex idris
	-bibtex idris
	-latex idris
	-latex idris

#package: idris.tar

#idris.tar: idris.dvi idris.ps idris.pdf .PHONY
#	mkdir -p idris
#	cp ../bib/*.bib *.tex *.ltx *.bib *.ps *.pdf *.dvi *.cls idris
#	tar cvf idris.tar idris

.PHONY:
