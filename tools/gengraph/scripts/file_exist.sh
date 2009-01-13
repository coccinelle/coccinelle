#!/bin/bash

PREFIX=/var/linuxes/linux-2.6.
MAJOR=28
OUT=`basename $1 .list`.csv

if [ -z `grep true $1` ]; then
	echo "`dirname $PREFIX` not found."
	ORIG=`basename $1 .list`.orig.csv
	if [ -f $ORIG ]; then
		echo "Using $ORIG."
		cp $ORIG $OUT
	else
		echo "$ORIG not found."
	fi
else

> $OUT

echo -n "file;" >> $OUT

for (( v=0 ; v <= $MAJOR ; v = $(($v + 1)) )); do
    echo -n "$v;" >> $OUT
done
echo "" >> $OUT

for f in `cat $1`; do
    echo -n "$f;" >> $OUT

    for (( v=0 ; v <= $MAJOR ; v = $(($v + 1)) )); do
      if [ -f $PREFIX$v/$f ]; then
	  echo -n "true;" >> $OUT
      else
	  echo -n "false;" >> $OUT
      fi
    done
    echo "" >> $OUT
done
fi
      
