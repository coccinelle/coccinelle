SUBDIRS=grammar

TEX=options.tex
PDF=$(TEX:.tex=.pdf)
AUX=$(TEX:.tex=.aux)
LOG=$(TEX:.tex=.log)

.PHONY: all clean distclean $(SUBDIRS)
.SUFFIXES: .pdf .tex

all: $(PDF) $(SUBDIRS)

$(SUBDIRS):
	make -C $@

$(PDF): $(TEX)
	pdflatex $<

clean:
	make -C $(SUBDIRS) clean
	rm -f $(AUX) $(LOG)

distclean:
	make -C $(SUBDIRS) distclean
	rm -f $(PDF)
