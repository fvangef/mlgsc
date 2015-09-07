function test_init
{
    echo "initializing test suite"
	if [ $BINDIR/mlgsc_train -nt final_prot_aln.bcls ] ; then
		echo "re-building final_prot_aln classifier (binary is newer)"
		$BINDIR/mlgsc_train Prot final_prot_aln.msa final_prot_tree.nw
	fi
	if [ $BINDIR/mlgsc_train -nt idaln.bcls ] ; then
		echo "re-building idaln classifier (binary is newer)"
		$BINDIR/mlgsc_train -i Prot idaln.mfa idtree.nw
	fi
	if [ $BINDIR/mlgsc_train -nt frc_4train.bcls ] ; then
		echo "re-building frc taxo classifier (binary is newer)"
		$BINDIR/mlgsc_train -T Taxo Prot frc_4train.msa bacteria.taxo
	fi
}
