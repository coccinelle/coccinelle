#!/bin/sh

# preliminary test script
# simply runs sgen on each .cocci/.config combo and compares with .out file.

# The sgen program
SGEN=sgen

START=$(date +%s)
diffs=0
for f in ./*.cocci
do
   filename=$(basename "$f")
   actual="${filename}f"
   expected="${filename%.*}.sgencocci"

   sgen ${filename} -o ${actual}
   if diff ${expected} ${actual} > /dev/null ; then
       echo "${filename}: All good."
   else
       echo "\n${filename}: Different!"
       diff ${expected} ${actual}
       echo ""
       diffs=$((diffs + 1))
   fi
   rm ${actual}
done

END=$(date +%s)

echo "\nFailed: ${diffs}"
echo "\nTotal elapsed time: $(($END - $START)) seconds."
