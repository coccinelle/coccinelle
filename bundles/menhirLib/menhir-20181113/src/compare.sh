#!/bin/bash

# Example usage: ./compare.sh mezzo
# This script processes mezzo.mly using two versions of Menhir:
# 1- the last committed version;
# 2- the current (uncommitted) version.
# It then compares the output.

# The variable OPT can be used to pass extra options to Menhir.
# Use as follows:
# OPT=--lalr ./compare.sh ocaml

BENCH=../test/good
MENHIR=_stage1/menhir.native
BASE="-v -lg 1"

if [ $# -eq 0 ]
then
  echo "$0: at least one argument expected"
  exit 1
fi

# Try the current version.
echo "Compiling (current uncommitted version)..."
make &> compile.new || { cat compile.new && exit 1 ; }
sleep 1
for FILE in "$@"
do
  FLAGS=`if [ -f $BENCH/$FILE.flags ] ; then cat $BENCH/$FILE.flags ; fi`
  echo "Running ($FILE.mly)..."
  { time $MENHIR $BASE $OPT $FLAGS --list-errors -la 2 $BENCH/$FILE.mly ; } >$FILE.out.new 2>$FILE.err.new
done

# Try the last committed version.
git stash
echo "Compiling (last committed version)..."
make &> compile.old || { cat compile.old && exit 1 ; }
sleep 1
for FILE in "$@"
do
  FLAGS=`if [ -f $BENCH/$FILE.flags ] ; then cat $BENCH/$FILE.flags ; fi`
  echo "Running ($FILE.mly)..."
  { time $MENHIR $BASE $OPT $FLAGS --list-errors -la 2 $BENCH/$FILE.mly ; } >$FILE.out.old 2>$FILE.err.old
done
git stash pop

# Diff.
for FILE in "$@"
do
  echo "Diffing stderr ($FILE.mly)..."
  diff $FILE.err.old $FILE.err.new
  echo "Diffing stdout ($FILE.mly)..."
  diff $FILE.out.old $FILE.out.new
done
