#!/bin/bash

OUT=`basename $1 .org`.bugs
LIST=`basename $1 .org`.list

cp $1 $OUT.tmp
sed -i "s/^\* BUG \(.*\)$/\1/" $OUT.tmp
sed -i "s|^\*\* \[\[view:/var/linuxes/linux-2\.6\.\([0-9]*\).*$|\1|" $OUT.tmp

sed -i "s|^\* org config||" $OUT.tmp
sed -i "s|^#+SEQ_TODO:.*$||" $OUT.tmp
grep -v "^$" $OUT.tmp > $OUT
rm -f $OUT.tmp

grep -v "^[0-9]*$" $OUT | grep -v "^\*" | grep -v "^#" > $LIST.tmp
sed -i "s/^\([^ ]*\) .*$/\1/" $LIST.tmp
sort -u $LIST.tmp > $LIST
rm -f $LIST.tmp
