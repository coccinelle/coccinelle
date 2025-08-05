#!/bin/bash

# Run this script in cpptests/ to get a report on the tests.

# shellcheck disable=SC2086
set -e
#set -x

WANT_DEBUG=''
while getopts "do:" NAME; do
	case $NAME in
		d)	WANT_DEBUG=1;;
		o) WANT_HTML=$OPTARG;
			which txt2html || { echo "Utility txt2html not found -- please install it to use this option!"; false; }
		;;
		# TODO: add -h (e.g. mention CPPREFERENCE_BASE_URL...)
		*) false;;
	esac
done
shift $((OPTIND-1))

spatch=../spatch${TOOLS_SUFFIX}
declare -A FAILED_RUN
declare -A FAILED_PP
declare -A FAILED_CP
declare -A TEST_CASE_BROKEN
declare -A TEST_CASE_FAILS
declare -A REFTAGS
declare -A REFTOTEST
declare    WANT_DEBUG
WARNINGS=''

cat_tags_file() {
cat << EOF
access_specifiers_0.cocci v20240610:language/access
access_specifiers_1.cocci v20240610:language/access
access_specifiers_1_class_failure.cocci v20240610:language/access v20240610:language/class
access_specifiers_2.cocci v20240610:language/access
access_specifiers_3.cocci v20240610:language/access
access_specifiers_4.cocci v20240610:language/access
addremvec_failure.cocci v20240610:language/template_parameters#Template_arguments
aggregate_initialization_failure.cocci v20240610:language/aggregate_initialization#Syntax
attributeu.cocci v20240610:language/attributes
auto_failure.cocci v20240610:language/auto
autoloop_failure.cocci v20240610:language/range-for
bool1.cocci v20240610:language/function
bracket.cocci v20240610:language/aggregate_initialization#Syntax
bracketed_expression_assignment.cocci v20240610:language/aggregate_initialization#Syntax
braced_init_list.cocci v20240610:language/explicit_cast#Explanation
braced_init_list_return.cocci v20240610:language/return
braced_init_list_assign.cocci v20240610:language/initialization#Initializer
braced_init_list_arg.cocci v20240610:language/initialization#Initializer v20240610:language/function
cdstr.cocci v20240610:language/constructor
co_return.cocci v20240610:language/coroutines
snip_field_bad.cocci v20240610:language/class
snip_field_and_keep_access_specifier.cocci v20240610:language/class v20240610:language/access
classfinal_failure.cocci v20240610:language/final
complexcpp.cocci v20240610:numeric/complex v20240610:header/complex
cuda1.cocci v20240610:language/operator_other#Built-in_function_call_operator
cuda.cocci v20240610:language/function#Function_definition
cuda_noattr.cocci v20240610:language/operator_other#Built-in_function_call_operator
decl_andand_ref.cocci v20240610:language/reference#Rvalue_references
decl_and_ref.cocci v20240610:language/reference#Rvalue_references
decl_ptr_ref.cocci v20240610:language/pointer#Pointers
decltype.cocci v20240610:language/decltype
decltype_matches_type.cocci v20240610:language/decltype
delete_array.cocci v20240610:language/delete#Syntax
delete.cocci v20240610:language/delete#Syntax
delete_replace.cocci v20240610:language/delete#Syntax
destructor_constructor_parse_failure v20240610:language/access v20240610:language/class v20240610:language/destructor
destructor_constructor_parse_smpl_failure v20240610:language/access v20240610:language/class v20240610:language/destructor
emptytmp_failure.cocci v20240610:language/template_parameters#Template_arguments
endcolon.cocci v20240610:language/qualified_lookup
endline.cocci v20240610:language/template_parameters#Template_arguments v20240610:language/qualified_lookup
enumcpp.cocci v20240610:language/enum
fieldtmp_failure.cocci v20240610:language/operator_member_access
finalclass.cocci v20240610:language/final#Syntax
forc.cocci v20240610:language/template_parameters#Template_arguments
forrange2.cocci v20240610:language/range-for
forrange_failure.cocci v20240610:language/range-for
inh1.cocci v20240610:language/derived_class
init3tst.cocci v20240610:language/aggregate_initialization#Syntax
instfour_failure.cocci v20240610:language/template_parameters#Template_arguments
instruct.cocci v20240610:language/using_declaration v20240610:preprocessor/impl
lambda_simple_failure.cocci v20240610:language/lambda
list_and_aggregate_initialization_isomorphism_off.cocci v20240610:language/aggregate_initialization#Syntax
list_and_aggregate_initialization_isomorphism_on.cocci v20240610:language/aggregate_initialization#Syntax
list_initialization.cocci v20240610:language/aggregate_initialization#Syntax
local_macro_fn_def_and_call.cocci v20240610:preprocessor
macro_stmt_when_fn_type.cocci v20240610:preprocessor
match_bracket_expressions_assignment_broken.cocci v20240610:language/aggregate_initialization#Syntax
miniclass.cocci v20240610:language/class
namespace_alias_definition_failure.cocci v20240610:language/namespace_alias
namespace_nested1_failure.cocci v20240610:language/namespace
namespace_nested2_failure.cocci v20240610:language/namespace
namespace_nested3_failure.cocci v20240610:language/namespace
namespace_nested4_failure.cocci v20240610:language/namespace
new2.cocci v20240610:language/new#Syntax
new3.cocci v20240610:language/new#Syntax
new_failure.cocci v20240610:language/new#Syntax
newsimple.cocci v20240610:language/new#Syntax
nm.cocci v20240610:language/constructor
noexcept_failure.cocci v20240610:language/noexcept_spec
notpp.cocci v20240610:keyword/not
opeq_failure.cocci v20240610:language/operators
preprocessor_elifdef_add.cocci v20240610:language/preprocessor
protocpp.cocci v20240610:language/reference#Rvalue_references
qualclass.cocci v20240610:language/derived_class v20240610:language/qualified_lookup
qual.cocci v20240610:language/qualified_lookup
sizet.cocci v20240610:types/size_t
snip_field.cocci v20240610:language/class
tempinstfour.cocci v20240610:language/template_parameters#Template_arguments
templates1.cocci v20240610:language/template_parameters#Template_arguments
template_test_failure.cocci v20240610:language/template_parameters#Template_arguments
templates_partial_specialization_failure v20240610:language/template_parameters#Template_arguments v20240610:language/partial_specialization
tmpinit.cocci v20240610:language/template_parameters#Template_arguments
tmpinst2_failure.cocci v20240610:language/template_parameters#Template_arguments
tmpinst4_failure.cocci v20240610:language/template_parameters#Template_arguments
tmpinst5_failure.cocci v20240610:language/template_parameters#Template_arguments
try_catch1.cocci v20240610:language/try v20240610:language/catch
try_catch2.cocci v20240610:language/try v20240610:language/catch
try_catch.cocci v20240610:language/try v20240610:language/catch
using1.cocci v20240610:language/using_declaration
using2_failure.cocci v20240610:language/using_declaration
using3.cocci v20240610:language/using_declaration
using4_failure.cocci v20240610:language/using_declaration
usingtest_failure.cocci v20240610:language/using_declaration
usingtype.cocci v20240610:language/using_declaration
vconstr_failure.cocci v20240610:language/virtual v20240610:language/destructor
virtual_constructor_failure.cocci v20240610:language/virtual v20240610:language/destructor
while_init_condition.cocci v20240610:language/while
while_init_condition_smpl.cocci v20240610:language/while
EOF
}

read_tags_file() {
	while read cf tags;
	do
		if test $cf = '#'; then continue; fi
		tn=${cf/.cocci/};
		REFTAGS[$tn]=$tags
		#echo ${REFTAGS[$cf]}
	done < <( cat_tags_file )
}

populate_ref_to_test_array() {
	for tn in ${!REFTAGS[*]}; do
	for tag in ${REFTAGS[$tn]}; do
		if [[ $tag =~ v20240610: ]] ; then
			url=${CPPREFERENCE_BASE_URL:-https://en.cppreference.com/w/cpp/}${tag/v20240610:/};
			if [[ $url =~ '#' ]] then
				url=${url%%#*}${CPPREFERENCE_PAGE_EXT}'#'${url##*#}
			else
				url=${url}${CPPREFERENCE_PAGE_EXT}
			fi
			REFTOTEST[$url]+=" $tn"
		fi
	done
	done
}

check_tags_sanity() {
	local rc=0;
	local UNTAGGED_TEST_FILES=''
	for cf in *.cocci; do
		tn=${cf/.cocci/};
		if test -z "${REFTAGS[$tn]}"; then UNTAGGED_TEST_FILES+=" $cf"; fi
	done
	local ORPHANED_TEST_LINES=''
	for tn in ${!REFTAGS[*]}; do
		if ! test -f $tn.cocci ; then ORPHANED_TEST_LINES+=" $tn"; fi
	done
	want_strict=0; # TODO: introduce option to make this 1
	if test -n "${UNTAGGED_TEST_FILES}"; then WARNINGS+="Untagged test files: ${UNTAGGED_TEST_FILES}! "; rc=1; fi
	if test -n "${ORPHANED_TEST_LINES}"; then WARNINGS+="Orphaned test lines: ${ORPHANED_TEST_LINES}! "; rc=1; fi
	if test "${want_strict}" != 0 -a "${rc}" != 0; then exit $rc; fi
}

read_tags_file
populate_ref_to_test_array
check_tags_sanity

for cf in *.cocci; do
	tn=${cf/.cocci/};
	if ! ( head -n1 $cf | grep -q -- --c++ ) ; then echo "ERROR: you forgot --c++ flag in $cf file"; false; fi # --c++ is required
	set +e
	$spatch --test $tn
	FAILED_RUN[$tn]=$?
	set -e
	set +e
	$spatch --parse-cocci $tn.cocci
	FAILED_PP[$tn]=$?
	set -e
	set +e
	( $spatch --c++ --parse-c++ $tn.cpp && $spatch --c++ --parse-c++ $tn.cpp | grep -q '100.*good.or.passed' );
	FAILED_CP[$tn]=$?;
	set -e
	if test ${FAILED_RUN[$tn]} = 0 || true; then # notice we always step in the conditional, even if we expect TEST_CASE_BROKEN and TEST_CASE_FAILS to fail (and FAILED_RUN be now same as TEST_CASE_BROKEN; this is less strict but more clear
		cmpfile=$tn.cmp
		set +e
		$spatch --sp-file $tn.cocci $tn.cpp -o $cmpfile
		TEST_CASE_BROKEN[$tn]=$?
		cmp $tn.res $cmpfile
		TEST_CASE_FAILS[$tn]=$?
		if test -n "$WANT_DEBUG" && ! cmp $tn.res $cmpfile ; then
			diff $tn.res $cmpfile ; read;
		fi
		set -e
	fi
	rm -f $cmpfile
	set -e
done

function maybe_html_arrow()
{
	if test -n "$WANT_HTML"; then echo '&rarr;'; else echo '->'; fi
}

function file_title()
{
	echo "<CODE CLASS=\"H4\">$1</CODE>"
}

function header() {
	local PRE='' POST=''
	if test -n "$WANT_HTML"; then
		PRE+="<H2>";
		POST+="</H2>";
		if test "$1" = '-n'; then shift; else POST+="<BR>"; fi
	else
		if test "$1" = '-n'; then PRE+='-n'; fi
	fi
	echo "$PRE" "$@" "$POST"
}

function to_href() {
	if test $# = 0 ; then cat ; else echo "${1}"; fi | \
		sed 's/^\(.\+\)$/\<A HREF="\1"\>\1\<\/A\>/g'
}

function to_href2() {
	echo -n '<A HREF="'$1'">'$2'</A>'
}

function maybe_to_href() {
	if test -n "$WANT_HTML"; then to_href $@; else echo $@; fi
}

function maybe_to_anchor_href() {
	if test -n "$WANT_HTML"; then
		while read -d ' ' td; do
			#if test -z "$tn" ; then continue; fi
			tn=${td//\*/}
			#echo -n "bu:$tn"
			to_href2 "#${tn}" "${td}"; echo -n ' '
		done | sed 's/$/<BR>/g'
	else
		cat
	fi
}

function cat_all_as_pre() {
	for f in $1.cocci $1.cpp $1.res; do
		#echo "<BR>"
		file_title "$f"; echo -n ':'
		#echo "<BR>"
		! txt2html --prebegin 0 --bold_delimiter '' --italic_delimiter '' --underline_delimiter '' --extract "$f" # TODO: new2.res and a few others are missing
		true # the above can fail
		#echo "<BR>"
	done
}

function test_reference() {
	if test -n "$WANT_HTML"; then
		tr -d ' ' | \
		while read tn; do \
			#echo "<BR>"
			echo -n "<A ID=\"$tn\">"'</A>';
			echo '<H3>Test: '
			to_href2 "#$tn" "${tn}";
			if test ${TEST_CASE_BROKEN[$tn]}${TEST_CASE_FAILS[$tn]}${FAILED_RUN[$tn]}${FAILED_PP[$tn]}${FAILED_CP[$tn]} == 00000 ; then
				echo ' (<SPAN CLASS="TESTPASS">PASS</SPAN>)'
			else
				echo -n ' (<SPAN CLASS="TESTFAIL">FAIL:'
				if test ${TEST_CASE_BROKEN[$tn]} != 0; then echo -n ' /*exits nonzero*/'; fi
				if test ${TEST_CASE_FAILS[$tn]} != 0; then echo -n ' /*patch differs*/'; fi
				if test ${FAILED_RUN[$tn]} != 0; then echo -n ' --test'; fi
				if test ${FAILED_PP[$tn]} != 0; then echo -n ' --parse-cocci'; fi
				if test ${FAILED_CP[$tn]} != 0; then echo -n ' --parse-c++'; fi
				echo '</SPAN>)'
			fi
			echo '</H3>'
			#echo "<BR>"
			cat_all_as_pre $tn
		done
	else
		tr -d '\n' | cat
	fi
}

function zeros_in_array ()  { eval 'echo ${'$1'[*]}'  | tr ' ' '\n' | grep '^0$' |wc -w  ; };
function nonzeros_in_array ()  { eval 'echo ${'$1'[*]}'  | tr ' ' '\n' | grep '^[1-9]' |wc -w  ; };

print_results() {
if test -n "$WANT_HTML"; then
	echo '<!DOCTYPE HTML><HTML><HEAD><TITLE>C++ Tests</TITLE>
<HEAD>
<STYLE>
BODY { COLOR: BLACK; BACKGROUND: WHITE; }
H1 { BACKGROUND: SILVER; COLOR: BLACK; }
H2 { BACKGROUND: SILVER; COLOR: BLACK; }
H3 { BACKGROUND: LIGHTGRAY; COLOR: BLACK; }
H4 { BACKGROUND: LIGHTGRAY; COLOR: BLACK; }
SPAN.TESTFAIL { BACKGROUND: LIGHTGRAY; COLOR: CRIMSON; FONT-FAMILY: MONOSPACE; }
SPAN.TESTPASS { BACKGROUND: LIGHTGRAY; COLOR: DARKCYAN; FONT-FAMILY: MONOSPACE; }
CODE.H4 { BACKGROUND: LIGHTGRAY; COLOR: BLACK; }
</STYLE>
</HEAD>
<BODY>
<H1> Coccinelle C++ Tests Results Summary</H1>
';
fi
echo
header -n 'CPPREFERENCE AND TESTS: '
for rn in ${!REFTOTEST[*]}; do
	if test "${REFTOTEST[$rn]}" != ''; then
		echo -n "	`maybe_to_href $rn` `maybe_html_arrow` "
		for tn in ${REFTOTEST[$rn]}; do
			echo -n ${tn};
			if test "${FAILED_PP[$tn]}" != 0 -o "${TEST_CASE_FAILS[$tn]}" != 0 -o "${FAILED_RUN[$tn]}" != 0; then echo -n '* '; else echo -n ' '; fi
		done | maybe_to_anchor_href
		echo
	fi
done | sort

echo
header -n 'TEST CASE BROKEN (spatch ... exits non-zero): '"`nonzeros_in_array TEST_CASE_BROKEN`/${#REFTAGS[*]}"
for tn in ${!TEST_CASE_BROKEN[*]}; do
	if test ${TEST_CASE_BROKEN[$tn]} != 0; then
		echo "$tn "
	fi
done | sort | maybe_to_anchor_href
echo
header -n 'TEST FAILS (patches differ): '"`nonzeros_in_array TEST_CASE_FAILS`/${#REFTAGS[*]}"
for tn in ${!TEST_CASE_FAILS[*]}; do
	if test ${TEST_CASE_FAILS[$tn]} != 0; then
		echo "$tn "
	fi
done | sort | maybe_to_anchor_href
echo
header -n 'PASSED SPATCH PARSE: '"`zeros_in_array FAILED_PP`/${#REFTAGS[*]}"
for tn in ${!FAILED_PP[*]}; do
	if test ${FAILED_PP[$tn]} = 0; then
		echo "$tn "
	fi
done | sort | maybe_to_anchor_href
echo
header -n 'FAILED SPATCH PARSE: '"`nonzeros_in_array FAILED_PP`/${#REFTAGS[*]}"
for tn in ${!FAILED_PP[*]}; do
	if test ${FAILED_PP[$tn]} != 0; then
		echo "$tn "
	fi
done | sort | maybe_to_anchor_href
echo
header -n 'PASSED SOURCE PARSE: '"`zeros_in_array FAILED_CP`/${#REFTAGS[*]}"
for tn in ${!FAILED_CP[*]}; do
	if test ${FAILED_CP[$tn]} = 0; then
		echo "$tn "
	fi
done | sort | maybe_to_anchor_href
echo
header -n 'FAILED SOURCE PARSE: '"`nonzeros_in_array FAILED_CP`/${#REFTAGS[*]}"
for tn in ${!FAILED_CP[*]}; do
	if test ${FAILED_CP[$tn]} != 0; then
		echo "$tn "
	fi
done | sort | maybe_to_anchor_href
echo
header -n 'PASSED TEST RUNS: '"`zeros_in_array FAILED_RUN`/${#REFTAGS[*]}"
for tn in ${!FAILED_RUN[*]}; do
	if test ${FAILED_RUN[$tn]} = 0; then
		echo "$tn "
	fi
done | sort | maybe_to_anchor_href
echo
header -n 'FAILED TEST RUNS: '"`nonzeros_in_array FAILED_RUN`/${#REFTAGS[*]}"
for tn in ${!FAILED_RUN[*]}; do
	if test ${FAILED_RUN[$tn]} != 0; then
		echo "$tn "
	fi
done | sort | maybe_to_anchor_href
echo
header -n 'SOURCES FOR ALL TESTS: '"${#REFTAGS[*]}"
for tn in ${!REFTAGS[*]}; do
	echo "$tn "
done | sort | test_reference
echo
if test -n "$WANT_HTML"; then echo '<BODY><HTML>';fi
}

if test -n "$WANT_HTML"; then
	# TODO: Or MD?
	print_results > "$WANT_HTML"
else
	print_results
fi
if test -n "${WARNINGS}"; then echo "WARNINGS: ${WARNINGS}"; fi
