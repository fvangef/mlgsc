#!/bin/sh

shopt -o -s nounset

declare -r PROGNAME=mlgsc
declare -r BINNAME=../src/$PROGNAME

function run_case
{
	
	local -r test_name=$1
	local -r test_cmd=$2

	local -r test_out="test_${PROGNAME}_$test_name.out"
	local -r test_err="test_${PROGNAME}_$test_name.err"
	local -r test_exp="test_${PROGNAME}_$test_name.exp"

	echo -n "Running test: $test_name..."
	eval "$BINNAME $test_cmd > $test_out 2> $test_err"
	if diff $test_out $test_exp > /dev/null ; then
		echo Ok.
		rm $test_out $test_err
	else
		echo FAIL!
	fi
}

# TODO: read test name and arguments from test_mlgsc_args

run_case "firmic-Spo0A-prot-def" "final_prot_seq.pep final_prot_aln.bcls"
