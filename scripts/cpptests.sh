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
declare -A TEST_CASE_FAILS
declare -A REFTAGS
declare -A REFTOTEST

cat_tags_file() {
cat << EOF
access_specifiers_0.cocci v20240610:language/access
access_specifiers_1.cocci v20240610:language/access
access_specifiers_1_class.cocci v20240610:language/access v20240610:language/class
access_specifiers_2.cocci v20240610:language/access
access_specifiers_3.cocci v20240610:language/access
access_specifiers_4.cocci v20240610:language/access
addremvec.cocci v20240610:language/template_parameters#Template_arguments
aggregate_initialization.cocci v20240610:language/aggregate_initialization#Syntax
attributeu.cocci v20240610:language/attributes
auto.cocci v20240610:language/auto
autoloop.cocci v20240610:language/range-for
bool1.cocci v20240610:language/function
bracket.cocci v20240610:language/aggregate_initialization#Syntax
bracketed_expression_assignment.cocci v20240610:language/aggregate_initialization#Syntax
braced_init_list.cocci v20240610:language/explicit_cast#Explanation
cdstr.cocci v20240610:language/constructor
snip_field_bad.cocci v20240610:language/class
snip_field_and_keep_access_specifier.cocci v20240610:language/class v20240610:language/access
classfinal.cocci v20240610:language/final
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
destructor_constructor_parse v20240610:language/access v20240610:language/class v20240610:language/destructor
destructor_constructor_parse_smpl v20240610:language/access v20240610:language/class v20240610:language/destructor
emptytmp.cocci v20240610:language/template_parameters#Template_arguments
endcolon.cocci v20240610:language/qualified_lookup
endline.cocci v20240610:language/template_parameters#Template_arguments v20240610:language/qualified_lookup
enumcpp.cocci v20240610:language/enum
fieldtmp.cocci v20240610:language/operator_member_access
finalclass.cocci v20240610:language/final#Syntax
forc.cocci v20240610:language/template_parameters#Template_arguments
forrange2.cocci v20240610:language/range-for
forrange.cocci v20240610:language/range-for
inh1.cocci v20240610:language/derived_class
init3tst.cocci v20240610:language/aggregate_initialization#Syntax
instfour.cocci v20240610:language/template_parameters#Template_arguments
instruct.cocci v20240610:language/using_declaration v20240610:preprocessor/impl
list_and_aggregate_initialization_isomorphism_off.cocci v20240610:language/aggregate_initialization#Syntax
list_and_aggregate_initialization_isomorphism_on.cocci v20240610:language/aggregate_initialization#Syntax
list_initialization.cocci v20240610:language/aggregate_initialization#Syntax
local_macro_fn_def_and_call.cocci v20240610:preprocessor
macro_stmt_when_fn_type.cocci v20240610:preprocessor
match_bracket_expressions_assignment_broken.cocci v20240610:language/aggregate_initialization#Syntax
miniclass.cocci v20240610:language/class
namespace_alias_definition.cocci v20240610:language/namespace_alias
new2.cocci v20240610:language/new#Syntax
new3.cocci v20240610:language/new#Syntax
new.cocci v20240610:language/new#Syntax
newsimple.cocci v20240610:language/new#Syntax
noexcept.cocci v20240610:language/noexcept_spec
notpp.cocci v20240610:keyword/not
opeq.cocci v20240610:language/operators
protocpp.cocci v20240610:language/reference#Rvalue_references
qualclass.cocci v20240610:language/derived_class v20240610:language/qualified_lookup
qual.cocci v20240610:language/qualified_lookup
sizet.cocci v20240610:cpp/types/size_t
snip_field.cocci v20240610:language/class
tempinstfour.cocci v20240610:language/template_parameters#Template_arguments
templates1.cocci v20240610:language/template_parameters#Template_arguments
template_test.cocci v20240610:language/template_parameters#Template_arguments
templates_partial_specialization v20240610:language/template_parameters#Template_arguments v20240610:language/partial_specialization
tmpinit.cocci v20240610:language/template_parameters#Template_arguments
tmpinst2.cocci v20240610:language/template_parameters#Template_arguments
tmpinst4.cocci v20240610:language/template_parameters#Template_arguments
tmpinst5.cocci v20240610:language/template_parameters#Template_arguments
try_catch1.cocci v20240610:language/try v20240610:language/catch
try_catch2.cocci v20240610:language/try v20240610:language/catch
try_catch.cocci v20240610:language/try v20240610:language/catch
using1.cocci v20240610:language/using_declaration
using2.cocci v20240610:language/using_declaration
using3.cocci v20240610:language/using_declaration
using4.cocci v20240610:language/using_declaration
usingtest.cocci v20240610:language/using_declaration
usingtype.cocci v20240610:language/using_declaration
vconstr.cocci v20240610:language/virtual v20240610:language/destructor
virtual_constructor.cocci v20240610:language/virtual v20240610:language/destructor
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
			url='https://en.cppreference.com/w/cpp/'${tag/v20240610:/};
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
	if test -n "${UNTAGGED_TEST_FILES}"; then echo "ERROR: Untagged test files: ${UNTAGGED_TEST_FILES}"; rc=1; fi
	if test -n "${ORPHANED_TEST_LINES}"; then echo "ERROR: Orphaned test lines: ${ORPHANED_TEST_LINES}"; rc=1; fi
	if test "${rc}" != 0; then exit $rc; fi
}

read_tags_file
populate_ref_to_test_array
check_tags_sanity

for cf in *.cocci; do
	tn=${cf/.cocci/};
	if ! ( head -n1 $cf | grep -q -- --c++ ) ; then echo "you forgot --c++ flag in $cf file"; false; fi # --c++ is required
	set +e
	$spatch --test $tn
	FAILED_RUN[$tn]=$?
	set -e
	set +e
	$spatch --parse-cocci $tn.cocci
	FAILED_PP[$tn]=$?
	set -e
	set +e
	( $spatch --c++ --parse-c $tn.cpp && $spatch --c++ --parse-c $tn.cpp | grep -q '100.*good.or.passed' );
	FAILED_CP[$tn]=$?;
	set -e
	if test ${FAILED_RUN[$tn]} = 0; then
		cmpfile=$tn.cmp
		set +e
		$spatch --sp-file $tn.cocci $tn.cpp -o $cmpfile
		TEST_CASE_BROKEN[$tn]=$?
		cmp $tn.res $cmpfile
		TEST_CASE_FAILS[$tn]=$?
		set -e
	fi
	rm -f $cmpfile
	set -e
done
echo -n 'TEST CASE BROKEN (spatch ... exits non-zero): '
for tn in ${!TEST_CASE_BROKEN[*]}; do
	if test ${TEST_CASE_BROKEN[$tn]} != 0; then
		echo "$tn "
	fi
done | sort | tr -d '\n'
echo
echo -n 'TEST FAILS (patches differ): '
for tn in ${!TEST_CASE_FAILS[*]}; do
	if test ${TEST_CASE_FAILS[$tn]} != 0; then
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
echo -n 'PASSED TEST RUNS: '
for tn in ${!FAILED_RUN[*]}; do
	if test ${FAILED_RUN[$tn]} = 0; then
		echo "$tn "
	fi
done | sort | tr -d '\n'
echo
echo -n 'FAILED TEST RUNS: '
for tn in ${!FAILED_RUN[*]}; do
	if test ${FAILED_RUN[$tn]} != 0; then
		echo "$tn "
	fi
done | sort | tr -d '\n'
echo

echo 'REFERENCE TO TEST: '
for rn in ${!REFTOTEST[*]}; do
	if test "${REFTOTEST[$rn]}" != ''; then
		echo -n "	$rn -> "
		for tn in ${REFTOTEST[$rn]}; do
			echo -n ${tn};
			if test "${FAILED_PP[$tn]}" != 0 -o "${TEST_CASE_FAILS[$tn]}" != 0 -o "${FAILED_RUN[$tn]}" != 0; then echo -n '* '; else echo -n ' '; fi
		done
		echo
	fi
done | sort
echo
