# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

# a fairly minimal-effort rule for compiling the documentation.
# requires latexmk and a few packages (standard stuff).
docs:
	latexmk -pdf -pdflatex="pdflatex" -use-make documentation.tex

clean:
	rm -f *.aux *.fdb_latexmk *.log *.out *.toc *.pdf *~
