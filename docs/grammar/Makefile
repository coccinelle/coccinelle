TEX=cocci_syntax.tex

all: $(TEX)
	pdflatex $<

html: $(TEX)
	hevea -exec /usr/share/hevea/xxdate.exe $<
