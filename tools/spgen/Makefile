# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

all:
	cd source; make all; cd ..

# also compile documentation; this is not done by default
world: docs all

clean:
	cd source; make clean; cd ..; \
	cd documentation; make clean; cd ..;

install:
	cd source; make install; cd ..

uninstall:
	cd source; make uninstall; cd ..

docs:
	cd documentation; make docs; cd ..
