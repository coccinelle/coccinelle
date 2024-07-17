#!/bin/bash

# Run this script in cpptests/ to get a report on the tests.

# shellcheck disable=SC2086
set -e
#set -x
spatch=../spatch.opt
declare -A FAILED_RUN
declare -A FAILED_PP
declare -A FAILED_CP
declare -A TEST_CASE_BROKEN
for cf in *.cocci; do
	tn=${cf/.cocci/};
	set +e
	$spatch --c++ --test $tn
	FAILED_RUN[$tn]=$?
	set -e
	set +e
	$spatch --c++ --parse-cocci $tn.cocci
	FAILED_PP[$tn]=$?
	set -e
	set +e
	$spatch --c++ --parse-c $tn.cocci
	FAILED_CP[$tn]=$?
	set -e
	if test ${FAILED_RUN[$tn]} = 0; then
		cmpfile=$tn.cmp
		$spatch --c++ --sp-file $tn.cocci $tn.cpp -o $cmpfile
		set +e
		cmp $tn.res $cmpfile
		TEST_CASE_BROKEN[$tn]=$?
		set -e
	fi
	rm -f $cmpfile
	set -e
done
echo -n 'TEST CASE BROKEN (spatch --test ... exits non-zero): '
for tn in ${!TEST_CASE_BROKEN[*]}; do
	if test ${TEST_CASE_BROKEN[$tn]} != 0; then
		echo "$tn "
	fi
done | sort | tr -d '\n'
echo
echo -n 'PASSED SPATCH PARSE: '
for tn in ${!FAILED_PP[*]}; do
	if test ${FAILED_PP[$tn]} = 0; then
		echo "$tn "
	fi
done | sort | tr -d '\n'
echo
echo -n 'FAILED SPATCH PARSE: '
for tn in ${!FAILED_PP[*]}; do
	if test ${FAILED_PP[$tn]} != 0; then
		echo "$tn "
	fi
done | sort | tr -d '\n'
echo
echo -n 'PASSED SOURCE PARSE: '
for tn in ${!FAILED_CP[*]}; do
	if test ${FAILED_CP[$tn]} = 0; then
		echo "$tn "
	fi
done | sort | tr -d '\n'
echo
echo -n 'FAILED SOURCE PARSE: '
for tn in ${!FAILED_CP[*]}; do
	if test ${FAILED_CP[$tn]} != 0; then
		echo "$tn "
	fi
done | sort | tr -d '\n'
echo
echo -n 'PASSED TESTS: '
for tn in ${!FAILED_RUN[*]}; do
	if test ${FAILED_RUN[$tn]} = 0; then
		echo "$tn "
	fi
done | sort | tr -d '\n'
echo
echo -n 'FAILED TESTS: '
for tn in ${!FAILED_RUN[*]}; do
	if test ${FAILED_RUN[$tn]} != 0; then
		echo "$tn "
	fi
done | sort | tr -d '\n'
echo
