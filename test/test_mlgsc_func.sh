function test_init
{
    echo "initializing test suite"
	if [ $BINDIR/mlgsc_train -nt final_prot_aln.bcls ] ; then
		echo "re-building classifier (binary is newer)"
		$BINDIR/mlgsc_train Prot final_prot_aln.msa final_prot_tree.nw
	fi
}
