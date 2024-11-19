#CFLAGS=--sp-file test.cocci -no_show_diff
CFLAGS=--sp-file test.cocci
#CFLAGS=--sp-file testpy.cocci
CC=../../tools/spp
LD=cat

all: clean test

test: test.o test2.o

test.o: test.c test.h
test2.o: test2.c

clean:
	rm -f test.o test.c.i test2.o test2.c.i
