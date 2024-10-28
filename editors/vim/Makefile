DATE = $(shell date +%Y%m%d)

all: cocci-syntax-$(DATE).tar.bz2

cocci-syntax-$(DATE).tar.bz2:
	git archive --prefix=cocci-syntax-$(DATE)/ HEAD | tar --delete cocci-syntax-$(DATE)/.gitignore cocci-syntax-$(DATE)/Makefile | bzip2 -c > $@

clean:
	rm -rf *.tar.bz2 *.tar || true

upload: cocci-syntax-$(DATE).tar.bz2
	scp $< dev.exherbo.org:public_html/pub/software/releases/cocci-syntax/

install:
	@mkdir -p ~/.vim/ftdetect/
	@mkdir -p ~/.vim/syntax/
	cp ftdetect/cocci.vim ~/.vim/ftdetect/
	cp syntax/cocci.vim ~/.vim/syntax/

.phony: clean upload install

.default: all
