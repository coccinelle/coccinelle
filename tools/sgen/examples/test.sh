#!/bin/sh

# preliminary test script
# simply runs sgen on each .cocci/.config combo and compares with _.cocci file.

# abs path to dir containing this script (http://stackoverflow.com/a/246128)
DIR=$( cd "$( dirname "$0" )" && pwd )

# The sgen program
SGEN=${DIR}/../source/sgen

START=$(date +%s)
diffs=0

for f in ${DIR}/*[!'_'].cocci
do
   filename="${DIR}/$(basename "$f")"
   actual="${filename}f"
   expected="${filename%.*}_.cocci"

   ${SGEN} ${filename} -o ${actual}
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
