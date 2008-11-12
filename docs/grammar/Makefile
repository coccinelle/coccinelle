TEX=cocci_syntax.tex
HTML=$(TEX:.tex=.html)


.phony: all html clean check

all: $(TEX)
	pdflatex $<

html: $(HTML)

clean:
	rm -f *.aux *.dvi *.haux *.html *.htoc *.log *.out *.pdf *~

check: $(HTML)
	checklink $<

############################################
############################################

.SUFFIXES: .html .tex

.tex.html:
	hevea -exec /usr/share/hevea/xxdate.exe $<
