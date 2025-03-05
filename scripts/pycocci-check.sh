#!/bin/bash
set -e

for i in tests/ctests/pycocci/patches/*.cocci; do
	TEST=${i%%.cocci}
	CODE_TEST=tests/ctests/pycocci/code/$(basename $TEST)
	./tools/pycocci --clean-proof $i $CODE_TEST
done
