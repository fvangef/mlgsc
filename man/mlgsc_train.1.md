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

-f, --output-format *STRING*
:   A printf(1)-like format string that specifies the format of the output
    elements. The recognized placeholders are:
   
    %%: literal '%'
    
    %a: the aligned query sequence (useful fo troubleshooting)

    %l: the length of the (unaligned) query sequence
    
    %h: the Fasta headerg
    
    %i: the query sequence's ID, taken to be the first word in the header line
    
    %m: the lowest evidence ratio found for the current query
    
    %P: the predicted taxon 
    
    %p: the whole path through the phylogeny, from the root to the predicted
    taxon. Path elements can be formatted via option `-s`.
    
    %q: the query sequence, unaligned
    
    %s: the score of the query against the predicted taxon
    
    The default is "%h -> %p", that is, the query header followed by the whole
    path. To see just the ID and the predicted taxon, for example, use `-f "%i
    %P"`.
    
-h, \--help
:   print a short help message, and exit successfully.

-M, --mask-mode *n|t*
:   How to mask the query sequence (if at all). This causes some portions of the
    query sequence to be ignored when scoring against the model (at all nodes in
    the tree). The possible values are:

    n (none): do not mask - use the whole sequence for scoring.
    
    t (trim): ignore the leading and trailing gapped regions. This is useful
    when the query sequence is substantially shorter than the model, yet has a
    good local match to it. This will divide the sequence into a matching region
    flanked by to regions which are mostly gaps. The boundaries between the
    regions are determined by a hidden Markov model, and those gapped regions
    will be effectively trimmed from the query sequence.
    
    The default is not to mask.

-m, --traversal-mode *b|a|INT*
:   Determines the search strategy, that is, how the model tree is traversed.
    Possible values are:

    b (best): at each inner node, choose the child with the _best_ score. This
    is the fastest option, but offers no error recovery.
    
    a (all): show _all_ paths through the tree. This is useful when
    troubleshooting, as it helps determine at which point the classifier makes a
    wrong choice. The nodes actually followed always shows an evidence ratio of
    zero.
    
    INT: the number supplied is used as an evidence ratio (ER) cutoff. Instead
    of searching only the child with the best score, as in "best" mode, search
    all children with an ER (with respect to the best-scoring child) not greater
    than the threshold (and take the best-scoring taxon among all these). This
    allows some error recovery, as it can happen that the correct taxon lies on
    a branch that is not attached to the best-scoring child. Of course,
    exploring more branches takes more time.

-s, --step-format
:   Like `-f`, but specifies how each element of the path (from the root to the
    target taxon) is formatted. Valid placeholders are:
    
    '%%': literal '%'
    
    '%t': taxon name (= tree node label)
    
    '%s': evidence ratio
    
    '%b': score of best-scoring child of the current node

    The default format is "%t (%s)", that is, the taxon name followed by the
    evidence ratio in parentheses.

# EXIT STATUS

0 iff the program exited successfully.

# BUGS

* The program could try to determine if the sequences are DNA or protein

# EXAMPLE

# SEE ALSO

`mlgsc` (1), `mlgsc_xval` (1) and `mlgsc_dump` (1).

MLgsc is available form  <https://github.com/tjunier/mlgsc>

A technical description of how mlgsc_train trains the models is found in [2].
Please cite this paper if you use MLgsc in published research. Thanks!

# REFERENCES

[1] Henikoff S, Henikoff JG (1994) Position-based sequence weights. J Mol Biol 243(4): 574–57 pmid:7966282 doi: 10.1016/0022-2836(94)90032-9

[2] Junier T, Hervé V, Wunderlin T, and Junier P (2015) MLgsc: a
Maximum-Likelihood, general sequence classifier. PLoS One <http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0129384>

