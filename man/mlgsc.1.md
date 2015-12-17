% MLGSC(1) MLgsc User Manuals
% Thomas Junier
% December 16, 2015

# NAME

mlgsc - maximum -likelihood general sequence classifier

# SYNOPSIS

mlgsc -h

mlgsc [*options*] <*queries filename*> <*model*> 

# DESCRIPTION

`mlgsc` classifies sequences according to a model produced by `mlgsc_train(1)`.
The queries file must be in Fasta format. Of course, they are expected to be
homologous to the reference sequences used in training the model.


# OPTIONS

-A, \--no-align
:   Do not align the input sequences. If you have input sequences that are
    already aligned (to the model, or from the same multiple alignment that the
    model was trained with) This can save substantial run time as the alignment
    step is often the bottleneck (especially with small numbers of target taxa),
    since it is performed by full dynamic programming.
    The default is to align.
-e, \--ER-cutoff *NUM*
:   Stop classifying a particular query if a node has an ER smaller than *NUM*.
    Intuitively, this means that `mlgsc` has reached a node at which cannot
    decide which of its children is best, and therefore stops. 
    The default is not to apply any cutoff (i.e., a cutoff of 0).
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

# SEE ALSO

`mlgsc_train` (1), `mlgsc_xval` (1) and `mlgsc_dump` (1).

MLgsc is available form  <https://github.com/tjunier/mlgsc>

A technical description of how __mlgsc__ classifies queries is found in the
following article:

Junier T, Hervé V, Wunderlin T, and Junier P (2015) MLgsc: a
Maximum-Likelihood, general sequence classifier. PLoS One <http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0129384>

Please cite this paper if you use MLgsc in published research. Thanks!


