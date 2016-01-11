% MLGSC(1) MLgsc User Manuals
% Thomas Junier
% December 16, 2015

# NAME

mlgsc_train - train a model for MLGSC, the maximum-likelihood general sequence classifier

# SYNOPSIS

mlgsc_train -h

mlgsc_train [*options*] **DNA|Prot** *multiple-alignment* *phylogeny* 

# DESCRIPTION

mlgsc_train trains a model for use with mlgsc(1). The arguments are as follows:

**DMA|Prot**
:   specifies whether the model is trained on nucleotide (DNA or RNA) or protein
    sequences.
*multiple-alignment*
:   the name of a file containing a Fasta-formatted mutliple alignment of the
    classifying region. The Fasta headers should have the following format:
    
    `><ID> <taxon-name> [rest]`
    
    that is, an ID, followed by the target taxon's name, followed by optional
    free-form information; the ID, name and rest (if any) should be separated by
    whitespace. For example, the following header

    `>ID_5756 Clostridium rpoB`

    states that the associated sequence belongs to taxon _Clostridium_. The ID
    is not used, but it *can* be used for cross-validation (see
    *mlgsc_xval*(1)), and it is simpler for *mlgsc_train* and *mlgsc_xval* to
    use the same format.
    
*phylogeny*
:   the name of a file containing a phylogeny of the target taxa (see option
    **-T**). The phylogeny can be either a tree (in Newick format) or a taxonomy
    file. The default is a tree.
    
    A tree should have its leaves labeled by reference taxa (but see option
    **-I**). Inner nodes can be labeled as well (this is encouraged). 
    
    A taxonomy file lists the taxonomic position of each target taxon, one
    taxon per line, from the most general to the most specific clade. Clade
    names are separated by semicolons. For genus _Clostridium_ this might look
    like:

    Bacteria; Firmicutes; Clostridia; Clostridiales; Clostridiaceae; Clostridium

    The taxonmy file is parsed into a tree structure by creating a tree node for
    each clade not already in the tree. This means that there should be no
    "gaps" in the taxonomy: for example given the two following lines

    ```
    Clostridiales; Peptococcaceae; Dehalobacter
    Clostridiales; Peptococcus
    ```
    
    Peptococcus will end up as sister node to Peptococcaceae, which is wrong.
    Instead, the taxonomy should read:
    
    ```
    Clostridiales; Peptococcaceae; Dehalobacter
    Clostridiales; Peptococcaceae; Peptococcus
    ```
# OUTPUT

A binary file containing the model. By default, the file's name derives from the
multiple alignment file's name, but it can be specified with option **-o**.

The program may also output warnings (unless verbosity is set to 0, see option
**-v**) in the following situations:

* if a taxon is found in the alignment but not in the phylogeny
* if a taxon is found in the phylogeny but not in the alignment

in both cases, the taxon is ignored.


# OPTIONS

-o, \--output-file *filename*
:   Set the name of the output file (model) to *filename*. By default, the
    output file's name is derived from the alignment file's name by removing any
    extension and adding `.bcls` (for Binary CLaSsifier). 
-v, \--verbosity **0|1|2**
:   Set the verbosity level. Level 0 is quiet: only fatal errors are reported.
    Level 1 (default) adds warning messages, and level 2 (verbose) adds
    information about the run, such as the parameters.
-W, \--no-Henikoff-Weighting
:   Do not apply Henikoff weighting. By default, the input sequences are
    weighted using Henikoff and Henikoff's algorithm [1]. Weighting often, but
    not always, improves accuracy by a few percentage points. Use mlgsc_xval(1)
    to estimate the effect of weighting.
-I, \--id-tree
:   The phylogeny is labeled by sequence ID instead of taxa. The use case for
    this option is when the phylogenetic tree has been computed directly from
    the alignment passed as second argument. This has the following
    consequences: (i) the tree is labeled by sequence ID instead of target
    taxon name; (ii) some taxa may not be monophyletic according to this
    particular tree; (iii) the tree may yet better reflect the phylogeny of the
    sequences at hand than would a phylogeny of the same taxa but computed from
    a different gene. The program then behaves as follows:
    
    1. from the headers of the alignment file, it constructs a (sequence ID ->
       taxon name) map.
    1. it relabels the input tree, replacing sequence IDs with the corresponding
       taxon names according to the map.
    1. it condenses any clade that consists entirely of the same taxon into a
       single leaf, labeled by that taxon
    1. if any taxon is found more than once (after condensing, thus indicating
       paraphyly), it gives a unique name to each leaf by appending a different
       number to each instance of a taxon that has multiple occurrences.

    For examle, imagine we have the following tree, where the leaves are
    labeled by sequence ID:

    ```
             /---------------+ ID1
     /-------+                    
     |       \---------------+ ID6
     |                            
    =+               /-------+ ID2
     |       /-------+            
     |       |       \-------+ ID3
     \-------+                    
             |       /-------+ ID4
             \-------+            
                     \-------+ ID5
    
    ```
    Suppose further that we have the following  ID -> taxon map:
    ```
    ID1 -> Solanum
    ID2 -> Atropa
    ID3 -> Datura
    ID4 -> Datura
    ID5 -> Mandragora
    ID6 -> Solanum
    ```

# EXIT STATUS

0 iff the program exited successfully.

# BUGS

* The program could try to determine if the sequences are DNA or protein

# EXAMPLES

Data for the examples are in the `data` directory of the distribution.

## Example 1

```
$ mlgsc_train Prot Spo0A.msa firmicute_genera.nw
```

This trains a model on the protein sequences of Spo0A (master regulator of
sporulation in Firmicutes), using Spo0A.msa and firmicutegenera.nw as
alignment and phylogeny, respectively. The output is a file named Spo0A.bcls.

## Example 2

```
$ mlgsc_train -o Firmicute_Spo0A Prot Spo0A.msa firmicute_genera.nw
```

As in Example 1, except that the model is named Firmicute_Spo0A (via option
**-o**).

# SEE ALSO

`mlgsc` (1), `mlgsc_xval` (1) and `mlgsc_dump` (1).

MLgsc is available form  <https://github.com/tjunier/mlgsc>

A technical description of how mlgsc_train trains the models is found in [2].
Please cite this paper if you use MLgsc in published research. Thanks!

# REFERENCES

[1] Henikoff S, Henikoff JG (1994) Position-based sequence weights. J Mol Biol 243(4): 574–57 pmid:7966282 doi: 10.1016/0022-2836(94)90032-9

[2] Junier T, Hervé V, Wunderlin T, and Junier P (2015) MLgsc: a
Maximum-Likelihood, general sequence classifier. PLoS One <http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0129384>

