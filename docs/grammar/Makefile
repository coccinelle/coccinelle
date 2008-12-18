TEX=cocci_syntax.tex
WEB=/var/www/localhost/htdocs/coccinelle/

PDF=$(TEX:.tex=.pdf)
HTML=$(TEX:.tex=.html)


.phony: all html clean check

all: $(TEX)
	pdflatex $<

html: $(HTML)

clean:
	rm -f *.aux *.dvi *.haux *.html *.htoc *.log *.out *.pdf *~

check: $(HTML)
	checklink $<

world: all html

universe: world install

install:
	cp *.css *.gif *.html $(WEB)
	cp $(PDF) $(WEB)
	chown apache:apache -R $(WEB)
	chmod a-w -R $(WEB)

############################################
############################################

.SUFFIXES: .html .tex

.tex.html:
# For Gentoo: /usr/lib64/hevea
# For Ubuntu family: /usr/share/hevea
# For default installation
	@if [ -f /usr/lib64/hevea/xxdate.exe ]; then \
	hevea -exec /usr/lib64/hevea/xxdate.exe  $< ; \
	hevea -exec /usr/lib64/hevea/xxdate.exe  $< ; \
	elif [ -f /usr/share/hevea/xxdate.exe ]; then \
	hevea -exec /usr/share/hevea/xxdate.exe $< ; \
	hevea -exec /usr/share/hevea/xxdate.exe $< ; \
	elif [ -f /usr/local/lib/hevea/xxdate.exe ]; then \
	hevea -exec /usr/local/lib/hevea/xxdate.exe $< ; \
	hevea -exec /usr/local/lib/hevea/xxdate.exe $< ; \
	else \
	echo -e "\n\n *** Hevea: Unable to find xxdate.exe ***\n\n"; \
	fi
	hacha -o index.html cocci_syntax.html
