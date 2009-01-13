#!/bin/bash

PREFIX=/var/linuxes/linux-2.6.
MAJOR=28
OUT=`basename $1 .orig.org`.edit.org
EDIT=`basename $1 .orig.org`.org

sed 's|[^:]*:/var/linuxes/[^/]*/\([^:]*\)::.*|\1|' $1 | sort -u > file_list.tmp

> $OUT

for f in `cat file_list.tmp`; do
    echo "* TODO $f" >> $OUT

    for (( v=0 ; v <= $MAJOR ; v = $(($v + 1)) )); do
      match=`grep "$PREFIX$v/$f" $1`
      if [ -n "$match" ]; then echo "*$match" | sed "s|TODO ||" >> $OUT ; fi
    done
done
rm -f file_list.tmp
echo "$OUT have been generated."
if [ ! -f $EDIT ]; then
	echo "$EDIT does not exist. Create a copy of $OUT."
else
	echo "$EDIT exists. Not overwritten."
fi
