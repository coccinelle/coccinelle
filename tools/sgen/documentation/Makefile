# a fairly minimal-effort rule for compiling the documentation.
# requires latexmk and a few packages (standard stuff).
docs:
	latexmk -pdf -pdflatex="pdflatex" -use-make documentation.tex

clean:
	find . -type f ! \( -name "*.tex" -o -name "Makefile" \) -delete
