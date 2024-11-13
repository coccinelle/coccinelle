# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/

# A fairly minimal-effort rule for compiling the documentation.
# Requires a few standard LaTeX packages.

ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(MAKECMDGOALS),clean)
include ../../../Makefile.config
-include ../../../Makefile.local
endif
endif


sources := $(wildcard *.tex)

manual := documentation.pdf

PDFLATEX_CMD=$(PDFLATEX) -halt-on-error

docs: $(manual)

$(manual): $(sources)
	$(PDFLATEX_CMD) documentation.tex
	$(PDFLATEX_CMD) documentation.tex

clean:
	rm -f *.aux *.log *.out *.toc *.pdf *.dvi *.ps *~
