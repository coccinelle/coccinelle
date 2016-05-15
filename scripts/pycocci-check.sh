#!/bin/bash
set -e

for i in tests/pycocci/patches/*.cocci; do
	TEST=${i%%.cocci}
	CODE_TEST=tests/pycocci/code/$(basename $TEST)
	./tools/pycocci --clean-proof $i $CODE_TEST
done
