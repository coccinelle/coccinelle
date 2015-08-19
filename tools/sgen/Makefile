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
