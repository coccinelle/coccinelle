# a fairly minimal-effort rule for compiling the documentation.
# requires latexmk and a few packages (standard stuff).
docs:
	latexmk -pdf -pdflatex="pdflatex" -use-make documentation.tex

clean:
	rm -f *.aux *.fdb_latexmk *.log *.out *.toc *.pdf *~
