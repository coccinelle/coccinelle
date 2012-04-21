#! /bin/sh

find `pwd`/* -name "*.[ch]" | glimpseindex -o -H . -F
