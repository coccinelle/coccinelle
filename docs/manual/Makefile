##############################################################################
# Variables
##############################################################################

ifneq ($(MAKECMDGOALS),distclean)
include ../../Makefile.config
endif

MAINSRC=main.tex
SRC=$(wildcard *.tex)
PDFLATEX_CMD=$(PDFLATEX) -halt-on-error

#tools:
# latex, pdflatex
# xfig
# emacs, auctex, ediff
# darcs
# latexdiff

##############################################################################
# Top rules
##############################################################################
.PHONY: all clean distclean
.SUFFIXES: .pdf .tex

all: manual.pdf main_grammar.pdf options.pdf

manual.pdf: $(SRC)
	$(PDFLATEX_CMD) manual.tex
	$(PDFLATEX_CMD) manual.tex

main_grammar.pdf: main_grammar.tex cocci_syntax.tex  macros_listing_cocci.tex macros_grammar.tex macros_common.tex examples.tex tips.tex
	$(PDFLATEX_CMD) main_grammar.tex
	$(PDFLATEX_CMD) main_grammar.tex

options.pdf: main_options.tex spatch_options.tex   macros_options.tex
	$(PDFLATEX_CMD) options.tex
	$(PDFLATEX_CMD) options.tex

distclean:: clean
	rm -f manual.pdf cocci_syntax.pdf options.pdf


# lindig trick
#RERUN =     Rerun (LaTeX|to get cross-references right)
#
#pdf:	    $(PDF)
#
#%.pdf:	%.tex
#	    $(PDFLATEX) $<
#	    if egrep -s '$(RERUN)' $*.log ;then $(PDFLATEX) $<; fi
#	    if egrep -s '$(RERUN)' $*.log ;then $(PDFLATEX) $<; fi

##############################################################################
# Html version
##############################################################################
.PHONY: html check

.SUFFIXES: .html .tex

TEX=main_grammar.tex
WEB=/var/www/localhost/htdocs/coccinelle/
EMNWEB=~/website

PDF=$(TEX:.tex=.pdf)
HTML=$(TEX:.tex=.html)

html: $(HTML)

clean::
	rm -f *.aux *.dvi *.haux *.htoc *.log *.out *~

distclean:: clean
	rm -f *.pdf *.html *.gif *.css

check: $(HTML)
	checklink $<

world: all html

universe: world install

install:
	cp *.css *.gif *.html $(WEB)
	cp $(PDF) $(WEB)
	chown apache:apache -R $(WEB)
	chmod a-w -R $(WEB)

emn_install: world
	cp *.css *.gif *.html $(EMNWEB)/docs
	cp $(PDF) $(EMNWEB)/docs
	cp options.pdf $(EMNWEB)/

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
	hacha -o index.html main_grammar.html
#	hacha -o index.html cocci_syntax.html


##############################################################################
# Install
##############################################################################


##############################################################################
# Developer rules
##############################################################################

LATEXDIFFOPT=--type=CTRADITIONAL

#if multi files ? sed sur les \input{} ? ou create in another dir ?
latexdiff:
	latexdiff $(LATEXDIFFOPT) main-old.tex main.tex > main-diff.tex
	$(MAKE) MAINSRC=main-diff
	rm -f main-diff.aux

replacediff:
	cp main-old.tex main-older.tex
	cp main.tex main-old.tex

clean::
	rm -f main-diff.*

update:

commit:


##############################################################################
# Generic rules
##############################################################################
FORMAT=letter

%.ps: %.dvi
	dvips -P cmz $< -o $@ -t $(FORMAT)

clean::
	rm -f   *.aux \
                *.bbl \
                *.blg \
                *.dvi \
                *.log \
                *.out \
                *.toc

include ../../Makefile.common
