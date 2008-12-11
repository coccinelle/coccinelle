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
# For Gentoo: /usr/lib64/hevea
# For Ubuntu family: /usr/share/hevea
	if [ -f /usr/lib64/hevea/xxdate.exe ]; then \
	hevea -exec /usr/lib64/hevea/xxdate.exe  $< ; \
	elif [ -f /usr/share/hevea/xxdate.exe ]; then \
	hevea -exec /usr/share/hevea/xxdate.exe $< ; \
	else \
	echo -e "\n\n *** Hevea: Unable to find xxdate.exe ***\n\n"; \
	fi
