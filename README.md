MLgsc - Maximum-likelihood general sequence classifier
======================================================

Summary
-------

MLgsc is a set of programs for classifying sequences into taxa (in other words,
recognizing taxa from sequences).

Compared to other programs, MLgsc

- can work on any conserved region (in particular, not just 16S rRNA)
- can work on any set of taxa (not just prokaryotes)
- can work on nucleic acid *and protein* sequences
- has a simple command-line interface
- has no dependencies to other packages (except possibly if you install from source)

A technical description of MLgsc can be found in the [article](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0129384).

Example
-------

Here is a short example of MLgsc in action, in which we classify the sequences
in file `data/queries.pep` (which is just a Fasta file) according to a
classifier for endospore-forming Firmicutes based on the protein sequence of the
Spo0A gene (file `data/Spo0A.bcls`):

```bash
$ mlgsc data/queries.pep data/Spo0A.bcls | head

```
```
ID_001 Bacillus -> Bacilli (56); Bacillaceae (97); Bacillus (87)
ID_002 Clostridium -> Clostridia (106); Clostridiales (138); Clostridium (83)
ID_003 Clostridium -> Clostridia (106); Clostridiales (138); Clostridium (83)
ID_004 Bacillus -> Bacilli (79); Bacillaceae (95); Bacillus (99)
ID_005 Bacillus -> Bacilli (84); Bacillaceae (89); Bacillus (104)
ID_006 Bacillus -> Bacilli (84); Bacillaceae (89); Bacillus (104)
ID_007 Exiguobacterium -> Bacilli (80); Exiguobacterium (279)
ID_008 Clostridium -> Clostridia (131); Clostridiales (200); Clostridium (179)
ID_009 Sporomusa -> Clostridia (74); Sporomusa (30)
ID_010 Clostridium -> Clostridia (170); Clostridiales (208); Clostridium (168)
```


The output has one line per query sequence. Each line contains the query's name
(`query_001`, etc.), followed by an arrow, and finally by `mlgsc`'s
classification of the query. The classification lists the taxa the query is
inferred to belong to, in order of increasing specificity; and each taxon name is followed by a measure of confidence (the higher the better) in parentheses.

For instance, `query_001` is inferred to belong to genus _Bacillus_, itself
within family Bacillaceae, which in turn belongs to class Bacilli. In this
example, confidence values above 10 are fairly reliable (for more about
confidence measures, see below), so all queries are confidently classified at
all levels.


Downloading and Installing
--------------------------

The package is available for download from
[GitHub](https://github.com/tjunier/mlgsc.git).  You can either download it from
the GitHub website using the "Download ZIP" button, or from the shell by `cd`ing to where you want to download, and doing:

~~~~ {.sourceCode .bash}
$ git clone https://github.com/tjunier/mlgsc.git
$ cd mlgsc
~~~~

The distribution contains  both **binaries** (Linux x86-64) and **source code**.

### Binaries

The package contains the following binaries (a.k.a "executables"):

-   `mlgsc`, the classifier program itself
-   `mlgsc_train`, used to train models for use with `mlgsc`
-   `mlgsc_xval`, used to validate models
-   `mlgsc_dump`, a helper program that prints information about models

The binaries are found in subdirectory `src` of the distribution. They can be
installed by issuing:

~~~~ {.sourceCode .bash}
$ cd src
$ sudo make install
~~~~

at the top-level directory of the distribution.  This will install them in `/usr/local/bin`.

That's it. You can jump to the tutorial.

### Source 

Compiling from source isn't extremely complicated. Basically, it involves:

1. Making sure you have a (recent) Haskell Compiler
1. (possibly) Installing additional libraries
1. Building

#### [Installing Haskell](#installing-haskell)

MLgsc is written in the [Haskell](https://www.haskell.org) language. You
will need a Haskell compiler and libraries; the easiest way of obtaining
this is to install the [Haskell
platform](https://www.haskell.org/platform) on your machine - it is
[available](https://www.haskell.org/platform/linux.html) as a
precompiled package for several major Linux distribution including
Ubuntu, Fedora, Mint, etc.

I use the Glasgow Haskell Compiler version 7.10.2, you may still be able to
compile with a slightly older version. To see which version you have, do

```bash
$ ghc --version

```
```
The Glorious Glasgow Haskell Compilation System, version 7.10.2
```


##### [Additional Libraries](#additional-libraries)

Libraries not included in the Haskell platform may be installed with the
`cabal` tool (which comes with the platform). The following command will
install all additional packages (you may need to run this as root):

~~~~ {.sourceCode .bash}
$ cabal install vector containers text binary text-binary array \
  parsec learning-hmm random-fu tuple random HUnit process directory \
  filepath MissingH vector-binary
~~~~

### Building


Issue the following commands in the top-level directory of the source
distribution:

~~~~ {.shell}
$ runhaskell Setup.hs configure # may need --user
$ cabal build
$ sudo cabal install --global
~~~~

Or, if you have GNU make, you can also just do

~~~~ {.shell}
$ make
$ sudo make install
~~~~

This will install the programs in `/usr/local/bin`.

**Note**: I'm still trying to figure out how to package the thing for use with
Cabal. Until then, I find it easier to use good old Make.

Tutorial
--------

This section will walk you through a complete example of using MLgsc to build a
model and use it to classify sequences.

File `data/unknown.pep` contains sequences of the Spo0A protein, the master
regulator of sporulation in some bacteria. It looks like this:

```bash
$ head data/unknown.pep

```
```
>query_001
IMPHLDGLAVLERLRESQLKKQPNVIMLTAFGQEDVTKKAVDLGASYFILKPFDMENLVG
HIRQVSGNGSQLTHRAPSSQSSVLRPQPESPKKNLDASITTIIHEIGVPAHIKGYLYLRE
AISMVYNDIELLGSITKVLYPDIAKKFNTTASRVERAIRHAIEVAWSRGNIDSISSLFGY
TVSMSKAKPTNSEFIAMVAD
>query_002
IMPHLDGLGVLEKIGATAISKRPLFIILSAVGQDKITQRALALGAEYYVVKPFDMEVLIS
RIRQLKNVNQPNVIRQDGLSGEVKSSYHPPQPKNLEAEVTNIMHEIGVPAHIKGYQYLRD
AIIMVVKDLDVINSITKQLYPTIAKEYNTTPSRVERAIRHAIEVAWSRGQIDTIDSLFGY
TINVGKGKPTNSEFIAMVAD
```


Our task is to determine which genus each sequence belongs to. For this, we
need _reference sequences_, that is, Spo0A sequences from bacteria of known
genus. Spo0A is only found in Firmicutes, a clade of bacteria that includes
well-known genera like _Bacillus_ and _Clostridium_. Here are the first few
line of a file with aligned Spo0A sequences from 38 genera of Firmicutes:

```bash
$ head data/reference.msa

```
```
>ID_001 Bacillus
IMPHLDGLAVLERLRE-SQLKK-QPN-VIMLTAFGQEDVTKKAVDLGASYFILKPFDMEN
LVGHIRQVSGNGSQL--THRAPS---SQSS------------VLR-PQPES------PKK
NLDASITTIIHEIGVPAHIKGYLYLREAISMVYNDIELLGSITKVLYPDIAKKFNTTASR
VERAIRHAIEVAWSRGNIDSISSLFGYTVSMSKAKPTNSEFIAMVAD
>ID_002 Clostridium
IMPHLDGLGVLEKIGA-TAISK-RPL-FIILSAVGQDKITQRALALGAEYYVVKPFDMEV
LISRIRQLKNVNQPN--VIRQ----------------DGLSGEVKSSYHPP------QPK
NLEAEVTNIMHEIGVPAHIKGYQYLRDAIIMVVKDLDVINSITKQLYPTIAKEYNTTPSR
VERAIRHAIEVAWSRGQIDTIDSLFGYTINVGKGKPTNSEFIAMVAD
```


Notice that the headers of each Fasta entry contains, after the ID, the name of
the genus the sequence belongs to: _Bacillus_ for `ID_001`, _Clostridium_ for
`ID_002`, and so on. These genera are our _target taxa_: given a sequence from
one of these genera, we expect our classifier to identify the genus it belongs
to - even if the sequence was not among the reference sequences.

MLgsc also needs a _phylogeny_ of the reference taxa. This is mostly to speed
up the computation, the details are in the
[article](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0129384).

One way of supplying this information is with a _taxonomy file_, such as this
one (another is with a phylogenetic tree, see in the Reference part below):

```bash
$ head data/firmicutes.taxo

```
```
Firmicutes; Clostridia; Sporomusa
Firmicutes; Clostridia; Clostridiales; Eubacteriaceae; Anaerofustis
Firmicutes; Clostridia; Clostridiales; Eubacteriaceae; Eubacterium
Firmicutes; Clostridia; Clostridiales; Eubacteriaceae
Firmicutes; Clostridia; Clostridiales; Lachnospiraceae; Anaerostipes
Firmicutes; Clostridia; Clostridiales; Lachnospiraceae; Dorea
Firmicutes; Clostridia; Clostridiales; Lachnospiraceae; Marvinbryantia
Firmicutes; Clostridia; Clostridiales; Lachnospiraceae
Firmicutes; Clostridia; Clostridiales; Clostridium
Firmicutes; Clostridia; Clostridiales; Peptococcaceae; Dehalobacter
```


This file simply states that, for example, genus _Anaerofustis_ (line 2)
belongs to family Eubacteriaceae, which belongs to order Clostridiales, which
is in class Clostridia, which is itself a subclade of phylum Firmicutes.

### Training

We can now train a model for Spo0A in our target taxa. 

```bash
$ mlgsc_train -T t Prot data/reference.msa data/firmicutes.taxo 

```
```
```


The output of this command is our Spo0A model for Firmicutes, contained in file
`data/reference.bcls`. We can now use it to classify our unknown sequences:

### Classifying

```bash
$ mlgsc data/unknown.pep data/reference.bcls | head

```
```
query_001 -> Firmicutes (*); Bacilli (56); Bacillaceae (97); Bacillus (87)
query_002 -> Firmicutes (*); Clostridia (106); Clostridiales (138); Clostridium (83)
query_003 -> Firmicutes (*); Clostridia (106); Clostridiales (138); Clostridium (83)
query_004 -> Firmicutes (*); Bacilli (80); Bacillaceae (95); Bacillus (99)
query_005 -> Firmicutes (*); Bacilli (84); Bacillaceae (89); Bacillus (104)
query_006 -> Firmicutes (*); Bacilli (84); Bacillaceae (89); Bacillus (104)
query_007 -> Firmicutes (*); Bacilli (79); Exiguobacterium (281)
query_008 -> Firmicutes (*); Clostridia (131); Clostridiales (200); Clostridium (179)
query_009 -> Firmicutes (*); Clostridia (73); Sporomusa (30)
query_010 -> Firmicutes (*); Clostridia (170); Clostridiales (208); Clostridium (168)
```


This means, for example, that `query_001` is inferred by MLgsc to belong to the
Firmicutes, and within these, to the Bacilli; within the Bacilli, to the
Bacillaceae, and finally to genus _Bacillus_.

The values between parentheses are measures of confidence. Higher values mean
higher confidence, and values above 1000 are shown as an asterisk, indicating
virtual certainty. In this case, values above 10 are already quite reliable (we
show below how we determine this). This means that, unsurprisingly, all the
queries shown above are classified to genus level with high confidence.

As a check, let's classify a set of sequences whose genus is known:

```bash
$ mlgsc -f "%h -> %p"  data/queries.pep data/reference.bcls | head

```
```
ID_001 Bacillus -> Firmicutes (*); Bacilli (56); Bacillaceae (97); Bacillus (87)
ID_002 Clostridium -> Firmicutes (*); Clostridia (106); Clostridiales (138); Clostridium (83)
ID_003 Clostridium -> Firmicutes (*); Clostridia (106); Clostridiales (138); Clostridium (83)
ID_004 Bacillus -> Firmicutes (*); Bacilli (80); Bacillaceae (95); Bacillus (99)
ID_005 Bacillus -> Firmicutes (*); Bacilli (84); Bacillaceae (89); Bacillus (104)
ID_006 Bacillus -> Firmicutes (*); Bacilli (84); Bacillaceae (89); Bacillus (104)
ID_007 Exiguobacterium -> Firmicutes (*); Bacilli (79); Exiguobacterium (281)
ID_008 Clostridium -> Firmicutes (*); Clostridia (131); Clostridiales (200); Clostridium (179)
ID_009 Sporomusa -> Firmicutes (*); Clostridia (73); Sporomusa (30)
ID_010 Clostridium -> Firmicutes (*); Clostridia (170); Clostridiales (208); Clostridium (168)
```


I pass option `-f "%h -> %p`, which causes the full header (`%h`) to be printed
out, instead of only the ID.

We see that the classifications are correct.  But this  check is not very
strong, because the test sequences were all part of the training set. MLgsc
comes with a program (`mlgsc_xval`) that does cross-validation with disjoint
training and test sets.  This is discussed just below.

### Validation

`mlgsc_xval` performs _leave-one-out_ (LOO) cross-validation: it randomly draws
one sequence (referred to as the _test sequence_) from the reference set and
builds the model using all the remaining sequences (called the _training set_).
Then it classifies the test sequence agaisnt the model. By repeating the
procedure, one obtains an estimate of MLgsc's error rate. Let's see how this
works:

~~~~bash
$ mlgsc_xval -x -T t Prot data/reference.msa data/firmicutes.taxo
ID_353 Anaerostipes (196) -> Firmicutes (*); Clostridia (102); Clostridiales (182); Clostridium (12)
ID_339 Clostridium (212) -> Firmicutes (*); Clostridia (108); Clostridiales (172); Lachnospiraceae (18); Anaerostipes (125)
~~~~

This performs a hundred runs of LOO, using the same input data as for training
the model above. Since we're mostly interested in the failure rate, I pass
option `-x`, which causes `mlgsc_xval` to only display failures. Here there are
two lines, which means that two out of a hundred runs failed, that is, the
estimated error rate is 2%.
