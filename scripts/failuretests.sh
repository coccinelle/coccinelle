#!/bin/bash
# Regression tests for files that spatch cannot transform.  Such a file must
# be reported at the end of the run and make spatch exit with status 1, and
# the failure must not affect the other files handled by the same process.
#
# Run this script in failuretests/, or use "make check-failures".
set -u

spatch=${SPATCH:-../spatch${TOOLS_SUFFIX:-}}
[ -x "$spatch" ] || spatch=../spatch.opt
failures=0

pass() { echo "PASS: $*"; }
skip() { echo "SKIP: $*"; }
fail() {
	echo "FAIL: $*"
	failures=$((failures + 1))
	sed 's/^/  | /' "$tmp/err"
}

transformed() { # output file, source file name
	grep -qE "^--- (.*/)?$2\$" "$1"
}

tmp=$(mktemp -d) || exit 1
trap 'rm -rf "$tmp"' EXIT

# 1. A file whose processing fails is reported, and spatch exits with
#    status 1, while the other files are still transformed.  Check both the
#    sequential path and the parallel one, where the failures of several
#    worker processes have to be merged.
for jobs in 1 4; do
	"$spatch" --very-quiet --jobs "$jobs" --chunksize 1 --dir reported \
		--cocci-file reported.cocci > "$tmp/out" 2> "$tmp/err"
	status=$?
	name="failure reported (--jobs $jobs)"
	if [ "$status" -ne 1 ]; then
		fail "$name: exit status $status, expected 1"
	elif ! grep -q "^ERROR: the semantic patch was not applied to 1 file:" "$tmp/err" ||
	     ! grep -q "reported/fails\.c: .*injected failure" "$tmp/err"; then
		fail "$name: reported/fails.c is not reported"
	elif ! transformed "$tmp/out" works.c || transformed "$tmp/out" fails.c; then
		fail "$name: wrong set of files transformed"
	else
		pass "$name"
	fi
done

# 2. A timeout while inferring types in a header must not affect the next
#    file handled by the same process.  How long that phase takes depends
#    on the machine, so try longer and longer timeouts until the failure
#    message shows that one struck in it.
awk 'BEGIN {
	print "#ifndef BIG_H"
	print "#define BIG_H"
	for (i = 0; i < 150000; i++)
		printf "#define REG_FIELD_%d__SHIFT 0x%x\n", i, i % 32
	print "#endif"
}' > "$tmp/big.h"
cp state/first.c state/second.c "$tmp/"
printf '%s\n\n%s\n' "$tmp/first.c" "$tmp/second.c" > "$tmp/groups"
exercised=false
for timeout in 1 2 3 4 5 6 7 8 9 10; do
	"$spatch" --very-quiet --all-includes --include-headers-for-types \
		-I "$tmp" --timeout "$timeout" --jobs 1 --file-groups "$tmp/groups" \
		--cocci-file state.cocci > "$tmp/out" 2> "$tmp/err"
	status=$?
	grep -q "first\.c: timed out after ${timeout}s (while inferring types in " \
		"$tmp/err" || continue
	exercised=true
	name="next file unaffected by a timeout while inferring types (--timeout $timeout)"
	if [ "$status" -ne 1 ]; then
		fail "$name: exit status $status, expected 1"
	elif ! transformed "$tmp/out" second.c; then
		fail "$name: second.c was not transformed"
	else
		pass "$name"
	fi
	break
done
$exercised ||
	skip "no timeout from 1s to 10s struck while inferring types in the header"

# 3. A timeout while parsing a header must not stop the later files handled
#    by the same process from parsing that header.  The second file matches
#    only if the types in the header are known.  It may time out too while
#    parsing the header, which is fine as long as it is reported.
mkdir "$tmp/marked"
awk 'BEGIN {
	print "#ifndef BIG_H"
	print "#define BIG_H"
	print "struct item { int x; };"
	print "struct holder { struct item *buf; };"
	for (i = 0; i < 150000; i++)
		printf "#define REG_FIELD_%d__SHIFT 0x%x\n", i, i % 32
	print "#endif"
}' > "$tmp/marked/big.h"
cp marked/first.c marked/second.c "$tmp/marked/"
printf '%s\n\n%s\n' "$tmp/marked/first.c" "$tmp/marked/second.c" \
	> "$tmp/marked/groups"
exercised=false
for timeout in 1 2 3 4 5 6 7 8 9 10; do
	"$spatch" --very-quiet --all-includes --include-headers-for-types \
		-I "$tmp/marked" --timeout "$timeout" --jobs 1 \
		--file-groups "$tmp/marked/groups" --cocci-file marked.cocci \
		> "$tmp/out" 2> "$tmp/err"
	grep -q "marked/first\.c: timed out after ${timeout}s (while parsing .*big\.h)" \
		"$tmp/err" || continue
	exercised=true
	name="header parsed again after a timeout while parsing it (--timeout $timeout)"
	if transformed "$tmp/out" second.c ||
	   grep -q "^  .*/marked/second\.c: " "$tmp/err"; then
		pass "$name"
	else
		fail "$name: second.c was neither transformed nor reported"
	fi
	break
done
$exercised ||
	skip "no timeout from 1s to 10s struck while parsing the header"

# 4. Once a file has been handled, its timeout must not fire.  The finalize
#    rule of timer.cocci runs after the last file has been handled, and uses
#    more CPU time than --timeout allows.
"$spatch" --very-quiet --timeout 1 --cocci-file timer.cocci reported/works.c \
	> "$tmp/out" 2> "$tmp/err"
status=$?
name="no timeout after the last file has been handled"
if [ "$status" -ne 0 ]; then
	fail "$name: exit status $status, expected 0"
elif ! transformed "$tmp/out" works.c; then
	fail "$name: works.c was not transformed"
else
	pass "$name"
fi

if [ "$failures" -ne 0 ]; then
	echo "$failures test(s) failed"
	exit 1
fi
echo "all tests passed"
