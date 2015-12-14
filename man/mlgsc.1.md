% MLGSC(1) Pandoc User Manuals
% Thomas Junier
% December 14, 2015

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
    moel was trained with) lThis can save substantial run time as the alignment
    step is often the bottleneck (especially with small numbers of target taxa),
    since it is performed by full dynamic programming.
    The default is to align.
-e, \--ER-cutoff *NUM*
:   Stop classifying a particular query if a node has an ER smaller than *NUM*.
    Intuitively, this means that `mlgsc` has reached a node at which cannot
    decide which of its children is best, and therefore stops. 
    The default is not to apply any cutoff (i.e., a cutoff of 0).
-f, --outout-format *STRING*
    A printf(1)-like format string that specifies the format of the output
    elements. The recognized placeholders are:
    %%: literal '%'
    %a: the aligned query sequence (useful fo troubleshooting)
    %l: the length of the (unaligned) query sequence
    %h: the Fasta headerg
    %i: ID
    %m: MinER
    %P: Pred
    %p: Path
    %q: Query
    %s: Score
    %u: UPath
    
-h, \--help
:   print a short help message, and exit successfully.

# SEE ALSO

`mlgsc_train` (1), `mlgsc_xval` (1) and `mlgsc_dump` (1).

MLgsc is available form  <https://github.com/tjunier/mlgsc>


