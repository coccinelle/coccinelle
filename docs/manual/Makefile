##############################################################################
# Variables
##############################################################################

ifneq ($(MAKECMDGOALS),distclean)
include ../../Makefile.config
-include ../../Makefile.local
endif
HEVEA = hevea
HEVEA_XXDATE := $(shell ./find-xxdate)
HEVEA_CMD := $(HEVEA) -exec $(HEVEA_XXDATE)

MAINSRC=main.tex
SRC=$(wildcard *.tex)
PDFLATEX_CMD=$(PDFLATEX) -halt-on-error
PDF=manual.pdf main_grammar.pdf options.pdf

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

all: pdf html

pdf: $(PDF)

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
	@if test -z "${KEEP_GENERATED}"; then \
		rm -f manual.pdf cocci_syntax.pdf options.pdf; fi


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
WEBBASE?=~/website
WEBDOCS=$(WEBBASE)/docs

HTML=$(TEX:.tex=.html)

html: $(HTML)

clean::
	rm -f *.aux *.dvi *.pdf *.html *.haux *.htoc *.log *.out *~

distclean:: clean
	@if test -z "${KEEP_GENERATED}"; then \
		rm -f *.pdf *.html *.gif *.css; fi

check: $(HTML)
	checklink $<

.tex.html:
	$(HEVEA_CMD) $<
	$(HEVEA_CMD) $<
	hacha -o index.html main_grammar.html

##############################################################################
# Install
##############################################################################

install: all
	cp *.css *.gif *.html $(WEBDOCS)
	cp $(PDF) $(WEBDOCS)
#	chown apache:apache -R $(WEBDOCS)
#	chmod a-w -R $(WEBDOCS)

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
