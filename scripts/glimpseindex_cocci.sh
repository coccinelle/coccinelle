#! /bin/bash

if [ ! -z $1 ]; then
DIR=$1
else
DIR=`pwd`
fi

pushd $DIR
echo Indexing in $DIR
find * -name "*.[ch]" | glimpseindex -o -H $DIR -F
popd
