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

Here is an example of MLGSC used for classifying protein sequences of Spo0A to
genus level in the Firmicutes clade. We need a multiple alignment of Spo0A
protein sequences (`Spo0A.msa`) and a phylogenetic tree of the Firmicute genera
(`firmicute_genera.nw`).

Then we train the classifier with the following command:

```shell
$ mlgsc_train Prot Spo0A.msa firmicute_genera.nw
```

This produces a binary file named `Spo0A.bcls`, which contains the classifier.

We can now use it to classify Spo0A sequences of unknown genus:

```Bash
$ mlgsc queries.pep Spo0A.bcls
```

This produces output like the following:

    ID_201 Moraxella	->	Proteobacteria (79); Gammaproteobacteria (43); Pseudomonadales (26); Moraxellaceae (63); Moraxella (61)
    ID_202 Wolbachia	->	Proteobacteria (45); Alphaproteobacteria (23); Rickettsiales (78); Anaplasmataceae (234); Wolbachia (218)
    ID_203 Staphylococcus	->	Firmicutes (99); Bacilli (61); Bacillales (51); Staphylococcaceae (48); Staphylococcus (58)

Data
----


