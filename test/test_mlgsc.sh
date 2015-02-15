#!/bin/sh

shopt -o -s nounset

declare -r PROGNAME=$(basename $0 | sed -e 's/^test_\(.*\)\.sh/\1/')
declare -r BINNAME=../src/$PROGNAME

function run_case
{
	
	local -r test_name=$1
	local -r test_cmd=$2

	local -r test_out="test_${PROGNAME}_$test_name.out"
	local -r test_err="test_${PROGNAME}_$test_name.err"
	local -r test_exp="test_${PROGNAME}_$test_name.exp"

	echo -n "Running test: $test_name... "
	eval "$BINNAME $test_cmd > $test_out 2> $test_err"
	if diff $test_out $test_exp > /dev/null ; then
		echo Ok.
		rm $test_out $test_err
	else
		echo FAIL!
	fi
}

while read -a args; do
	test_name=${args[0]}
	test_args=${args[*]:1}
	run_case "$test_name" "$test_args"
done < test_mlgsc_args
