MLGSC - Maximum-likelihood  general sequence classifier
=====

MLGSC is a set of programs for classifying sequences according to OTU. It
consists of the following:

* `mlgsc_train`: trains a classifier using an alignment and a phylogenetic tree
* `mlgsc`: classifies unknown sequences according to a classifier produced by
  `mlgsc_train`
* `mlgsc_xval`: performs leave-one-out cross-validation on an alignment and a
  tree

Example
-------

Here is an example of MLGSC used for classifying protein sequences of Spo0A in
the Firmicutes clade. We need 
```bash
$ mlgsc_train Prot final_prot_aln.msa final_prot_tree.nw
```



