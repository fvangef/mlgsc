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

    query_01	->	Proteobacteria (79); Gammaproteobacteria (43); Pseudomonadales (26); Moraxellaceae (63); Moraxella (61)
    query_02	->	Proteobacteria (45); Alphaproteobacteria (23); Rickettsiales (78); Anaplasmataceae (234); Wolbachia (218)
    query_03	->	Firmicutes (99); Bacilli (61); Bacillales (51); Staphylococcaceae (48); Staphylococcus (8)
    ...

Each line lists a query ID (`query_01`, etc.), then a `->`, then MLgsc's
classification of this query.  This consists of a list of tree nodes, from the
most general (leftmost) to the most specific (rightmost) - in this case, genus.
Each node is shown with a confidence measure (number in parentheses). High
values indicate high confidence. In the example above, query 01 is confidently
classified as _Moraxella_, and query 02 as _Wolbachia_; however, query 03 is
much more tentatively classified as _Staphylococcus_ (confidence measure at
genus level only 8), although its position within Staphylococcaceae seems well
supported.

Installation
------------

### Installing Haskell

MLgsc is written in the [Haskell](http://haskell.org) language.
Data
----


