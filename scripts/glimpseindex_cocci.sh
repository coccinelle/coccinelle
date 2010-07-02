#!/bin/sh

if [ "$1" = "" ]; then
	DIR=`pwd`;
else
	DIR=$1;
fi

cd $DIR
find `pwd`/* -name "*.[ch]" | glimpseindex -o -H . -F
cd -
