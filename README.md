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

The output has one line per query sequence. Each line contains the query's name
(`query_001`, etc.), followed by an arrow, and finally by `mlgsc`'s
classification of the query. The classification lists the taxa the query is
inferred to belong to, in order of increasing specificity; and each taxon name is followed by a measure of confidence (the higher the better) in parentheses.

For instance, `query_001` is inferred to belong to genus _Bacillus_, itself
within family Bacillaceae, which in turn belongs to class Bacilli. In this
example, confidence values above 10 are fairly reliable (for more about
confidence measures, see below), so all queries are confidently classified at
all levels.

Roadmap
--------

The rest of this document explains:

* how to install MLgsc
* how to build classifiers with your own data
* how to check a classifier's performance
* how to classify unknown sequences with your classifiers, including how to fine-tune the output and the classification parameters



[Downloading and Installing](#installation)
---------------------------

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

##### [Additional Libraries](#additional-libraries)

Libraries not included in the Haskell platform may be installed with the
`cabal` tool (which comes with the platform). The following command will
install all additional packages (you may need to run this as root):

~~~~ {.sourceCode .bash}
$ cabal install vector containers text binary text-binary array \
  parsec learning-hmm random-fu tuple random HUnit process directory \
  filepath MissingH vector-binary
~~~~

### [Building](#building)


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

[Tutorial](#tutorial)
---------------------

This section will walk you through a complete example of using MLgsc to build a
model and use it to classify sequences.

File `data/unknown.pep` contains sequences of the Spo0A protein, the master
regulator of sporulation in some bacteria. It looks like this:

```bash
$ head data/unknown.pep
```

Our task is to determine which genus each sequence belongs to. For this, we
need _reference sequences_, that is, Spo0A sequences from bacteria of known
genus. Spo0A is only found in Firmicutes, a clade of bacteria that includes
well-known genera like _Bacillus_ and _Clostridium_. Here are the first few
line of a file with aligned Spo0A sequences from 38 genera of Firmicutes:

```bash
$ head data/reference.msa
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

This file simply states that, for example, genus _Anaerofustis_ (line 2)
belongs to family Eubacteriaceae, which belongs to order Clostridiales, which
is in class Clostridia, which is itself a subclade of phylum Firmicutes.

We can now build a model for Spo0A in our target taxa. 

```bash
$ mlgsc_train -T t Prot data/reference.msa data/firmicutes.taxo 
```

The output of this command is our Spo0A model for Firmicutes, contained in file
`data/reference.bcls`. We can now use it to classify our unknown sequences:

```bash
$ mlgsc data/unknown.pep data/reference.bcls | head
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

I pass option `-f "%h -> %p", which causes the full header (`%h`) to be printed out, instead of only the ID. We see that the classifications are correct. 

**Note**: the above check is not very strong, because in this case the test
sequences were all part of the training set. MLgsc come swith a program
(`mlgsc_xval`) that does cross-validation with disjoint training and test sets.
This is discussed just below.

### Validation

`mlgsc_xval` performs _leave-one-out_ cross-validation: it randomly draws one
sequence (referred to as the _test sequence_) from the reference set and builds
the model using all the remaining sequences (called the _training set_). Then
it classifies the test sequence agaisnt the model. By repeating the procedure,
one obtains an estimate of MLgsc's error rate. Let's see how this works:

```bash

```

Theory
------

1. Decide on the target taxa and classifying region
1. Obtain a set of aligned reference sequences of the classifying region for
   each of the target taxa, as well as a phylogeny of the target taxa
1. Validate the alignment and phylogeny
1. Build the final classifier
1. Classify unknown sequences

Here are the steps in detail.

### [Choosing the Target taxa and Classifying region](#choosing-the-clade-and-classifying-region)

The first decisions to make are (i) what organisms we wish to be able to
recognize, which we refer to as the *target taxa*, and (ii) with which classifying region. The target taxa will be pretty much determined by the study being conducted. The classifying region can be 

In this tutorial, we have chosen to classify the spore-forming
[Firmicutes](https://en.wikipedia.org/wiki/Firmicutes) using the
[Spo0A](http://www.uniprot.org/uniprot/P06534) gene (at the protein level).
Spo0A is found in every spore-forming Firmicute species, almost always as a
single-copy gene.


### [Obtaining a reference Alignment and Phylogeny](#obtaining-a-reference-alignment-and-phylogeny)

The alignment should be in aligned (gapped) FastA format. The header lines
should contain an ID followed by a taxon name. The ID is ignored for training,
but can be used to identify problematic training sequences; since the first word
of a FastA header is usually an ID this will allow existing FastA alignments to
be used with minimal editing.

#### [Reference Alignment](#reference-alignment)

We need reference sequences for all the target taxa, in the form of a
multiple sequence alignment (in gapped Fasta format). The header lines
should contain an ID followed by a taxon name. The ID is ignored for
training, but can be used to identify problematic training sequences;
since the first word of a FastA header is usually an ID this will allow
existing FastA alignments to be used with minimal editing.

We will use file `Spo0A.msa`, found in the `data` subdirectory. It looks
like this:

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
    >ID_003 Clostridium
    IMPHLDGLGVLEKIGA-TAISK-RPL-FIILSAVGQDKITQRALALGAEYYVVKPFDMEV
    LISRIRQLKNVNQPN--VIRQ----------------DGLSGEVKSSYHPP------QPK
    NLEAEVTNIMHEIGVPAHIKGYQYLRDAIIMVVKDLDVINSITKQLYPTIAKEYNTTPSR
    VERAIRHAIEVAWSRGQIDTIDSLFGYTINVGKGKPTNSEFIAMVAD
    ...

The first entry in the alignment has ID `ID_001` and taxon `Bacillus`.
This states that the sequence is a reference for the *Bacillus* genus.
The next two (`ID_001` and `ID_002`) are reference sequences for genus
*Clostridium*.

It is advisable to have more than one sequence per reference taxon, but
in our experience adding more than a dozen does little to enhance the
classifier's accuracy. Ideally each taxon should be represented by
roughly the same number of sequences, but since the alignment is
subjected to Henikoff weighting the program can tolerate large
variations (this is often the case when downloading all representatives
of a given gene from a database: some genera like *Clostridium* or
*Pseudomonas* have hundreds of known members, while several "rare"
genera have only one.

#### [Reference Phylogeny](#reference-phylogeny)

This should be a Newick-formatted tree in a single line. The leaves
(tips) of the tree must correspond to the taxa in the alignment. The
tree may be a phylogram (i.e., with branch lengths), but the branch
lengths are not used and will be ignored. Inner node labels are allowed
and indeed encouraged, as they will feature in the path through the tree
that `mlgsc` outputs.

We will use file `firmicute_genera.nw`. It represents the following
tree:

                   ┌─────────────────────────────────────────── Sporomusa           
                   │                                                                
                   │                            ┌────────────── Anaerofustis        
                   │              ┌─────────────┤ Eubacteriaceae                    
                   │              │             └────────────── Eubacterium         
                   │              │                                                 
                   │              │             ┌────────────── Anaerostipes        
                   │              │             │                                   
                   │              ├─────────────┼─Lachnospirac. Dorea
                   │              │             │                                   
                   │              │             └────────────── Marvinbryantia      
                   │              │                                                 
                   │              ├──────────────────────────── Clostridium         
                   │              │                                                 
                   │              │             ┌────────────── Dehalobacter        
                   │              ├─────────────┤ Peptococcaceae                    
     ┌─────────────┼─Clostridia───┤ Clostridiales────────────── Desulfotomaculum    
     │             │              │                                                 
     │             │              ├──────────────────────────── Heliobacterium      
     │             │              │                                                 
     │             │              ├──────────────────────────── Oscillibacter       
     │             │              │                                                 
     │             │              ├──────────────────────────── Ruminococcus        
     │             │              │                                                 
     │             │              ├──────────────────────────── Pseudoflavonifractor
     │             │              │                                                 
     │             │              ├──────────────────────────── Sulfobacillus       
     │             │              │                                                 
     │             │              └──────────────────────────── Symbiobacterium     
     │             │                                                                
     │             │              ┌──────────────────────────── Carboxydibrachium   
     │             │              │                                                 
     │             │              ├──────────────────────────── Carboxydothermus    
     │             └──────────────┤ Thermoanaerobacteraceae                         
     │                            ├──────────────────────────── Moorella            
     │                            │                                                 
     │                            └──────────────────────────── Thermacetogenium    
     │                                                                              
     │             ┌─────────────────────────────────────────── Alicyclobacillus    
     │             │                                                                
     │             │              ┌──────────────────────────── Amphibacillus       
     │             │              │                                                 
     │             │              ├──────────────────────────── Anoxybacillus       
    ─┤             │              │                                                 
     │             │              ├──────────────────────────── Bacillus            
     │             ├──────────────┤ Bacillaceae                                     
     │             │              ├──────────────────────────── Geobacillus         
     │             │              │                                                 
     │             │              ├──────────────────────────── Lysinibacillus      
     │             │              │                                                 
     │             │              └──────────────────────────── Oceanobacillus      
     │             │                                                                
     ├─────────────┼─Bacilli─────────────────────────────────── Bhargavaea          
     │             │                                                                
     │             ├─────────────────────────────────────────── Listeria            
     │             │                                                                
     │             ├─────────────────────────────────────────── Exiguobacterium     
     │             │                                                                
     │             │              ┌──────────────────────────── Brevibacillus       
     │             ├──────────────┤ Paenibacillaceae                                
     │             │              └──────────────────────────── Paenibacillus       
     │             │                                                                
     │             ├─────────────────────────────────────────── Pasteuria           
     │             │                                                                
     │             ├─────────────────────────────────────────── Sporosarcina        
     │             │                                                                
     │             └─────────────────────────────────────────── Desmospora          
     │                                                                              
     │             ┌─────────────────────────────────────────── C innocuum          
     │             │                                                                
     └─────────────┼─Erysipelotrichia────────────────────────── E dolichum          
                   │                                                                
                   └─────────────────────────────────────────── Turicibacter        
                                                                                

Note the following:

-   the tree's leaf (tip) labels are the names of the target taxa in the
    reference alignment.
-   the tree need not be fully resolved (bifurcating), although a
    bifurcating tree will make the search faster.

### [Evaluating the Clade and Classifying region](#evaluating-the-clade-and-classifying-region)

Before buliding the final model, we must satisfy ourselves that the
chosen region (Spo0A) is able to classify the chosen group
(spore-forming Firmicutes). This is the function of `mlgsc_xval`. To do
so, it takes the alignment and the tree, and does the following:

1.  Randomly draw one sequence from the alignment. This sequence becomes
    the *test sequence*, while all the remaining sequences form the
    *training set*. The test sequence is thus **not** part of the
    training set;
2.  Train a model using the training set and the tree;
3.  Classify the test sequence using the model.

This form of cross-validation where the test set contains one datum and
the training set contains all other data is called *Leave-one-out*. This
procedure is repeated a certain number of times (by default, one hundred
- the number can be changed with option `-r`).

Let's try this:

~~~~ {.sourceCode .bash}
$ ../src/mlgsc_xval Prot Spo0A.msa firmicutes_by_genus.nw

ID_185 Clostridium (202) -> Clostridia (170); Clostridiales (208); Clostridium (168)
ID_197 Clostridium (201) -> Clostridia (126); Clostridiales (170); Clostridium (134)
ID_177 Clostridium (202) -> Clostridia (170); Clostridiales (208); Clostridium (168)
ID_162 Clostridium (187) -> Clostridia (188); Clostridiales (229); Clostridium (183)
...
~~~~

**Notes**:

-   the sequences are drawn at random, so each run is different! To
    reproduce runs, you can set the pseudo-random number generator's
    [seed](#seed)
-   the first argument (`Prot` ) is a keyword that tells `mlgsc_xval`
    that the sequences are protein
-   each line represents a different trial, with a different test
    sequence and training set

In all the above lines, the program correctly classifies the query as
*Clostridium*. But we're mostly interested in the *errors* -
specifically, in how many there are. Option `-x` instructs `mlgsc_xval`
to print only misclassifications:

~~~~ {.sourceCode .bash}
$ ../src/mlgsc_xval -x Prot Spo0A.msa firmicutes_by_genus.nw 

ID_264 Paenibacillus (196) -> Clostridia (9); Clostridiales (134); Clostridium (59)
~~~~

This run had one error out of 100 trials, that is, a 1% error rate. I
usually get between 0 and 4 errors per 100 trials with this data set.

We can ask for details about the run by passing (`-v 2`). This sets the
verbosity level to 2 (0 is for quiet, i.e. no warnings; 1 is for
normal):

    $ ../src/mlgsc_xval -x -v 2 Prot Spo0A.msa firmicutes_by_genus.nw 
    Performing 100 rounds of LOO
    alignment:  Spo0A.msa
    phylogeny:  firmicutes_by_genus.nw
    seed:   607539836
    indices:    [129,205,121,120,208,60,185,56,147,160,193,119,29,291,184,116,82,72,328,330,136,323,189,247,336,327,168,43,146,73,223,137,176,149,301,108,12,276,228,91,141,218,351,253,46,118,297,324,23,109,221,83,84,296,130,5,24,274,172,249,125,175,209,158,200,100,170,173,3,206,106,266,203,359,352,251,154,0,45,15,37,350,292,114,79,133,307,28,64,27,4,263,287,142,187,139,345,61,321,339]
    min #nb seq / taxon:    3
    Henikoff weighting: True

    ID_219 Clostridium (201) -> Clostridia (126); Clostridiales (215); Lachnospiraceae (25); Anaerostipes (1)
    ID_353 Anaerostipes (196) -> Clostridia (102); Clostridiales (182); Clostridium (12)
    ID_264 Paenibacillus (196) -> Clostridia (9); Clostridiales (134); Clostridium (59)

This now shows, among others, which sequences were drawn as test
sequences (by their indices, i.e. their order in the reference
alignment). It also shows two other possibly important parameters: (i)
the minimal number of sequences per taxon, and (ii) whether Henikoff
weighting was applied.

##### [Seed](#seed)

The purpose of weighting is to compensate for the over-representation of
some taxa (e.g. if one taxon is represented by dozens of reference
sequences, while another has a single representative). But does it have
the intended effect? Let's re-run this test, but without the weighting.
To redo the run on the *same* sequences, we bypass the random draw and
supply the *seed* shown in the (verbose) output above.

~~~~ {.sourceCode .bash}
$ ../src/mlgsc_xval -v 2 -R 607539836 -x -W Prot Spo0A.msa firmicutes_by_genus.nw
Performing 100 rounds of LOO
alignment:  Spo0A.msa
phylogeny:  firmicutes_by_genus.nw
seed:   607539836
indices:    [129,205,121,120,208,60,185,56,147,160,193,119,29,291,184,116,82,72,328,330,136,323,189,247,336,327,168,43,146,73,223,137,176,149,301,108,12,276,228,91,141,218,351,253,46,118,297,324,23,109,221,83,84,296,130,5,24,274,172,249,125,175,209,158,200,100,170,173,3,206,106,266,203,359,352,251,154,0,45,15,37,350,292,114,79,133,307,28,64,27,4,263,287,142,187,139,345,61,321,339]
min #nb seq / taxon:    3
Henikoff weighting: False

ID_219 Clostridium (201) -> Clostridia (125); Clostridiales (204); Lachnospiraceae (46); Dorea (6)
~~~~

Where `-R 607539836` specifies the seed, and `-W` suppresses weighting.
In this case, the error rate is *lower* without weighting! In our
experience, weighting is most useful for very unbalanced sets.
Cross-validation like this helps determine whether or not weighting
should be applied.

### [Training the Model](#training-the-model)

An error rate of around 1% seems about right - now we can train the
final model. This is done with `mlgsc_train`.

This program takes the same three arguments as `mlgsc_xval`, namely :
(i) one of the two keywords `DNA` or `Prot`, to indicate the type of
molecule; (ii) the name of the multiple alignment file; and (iii) the
name of a phylogenetic tree.

Let's train a model with the Spo0A sequences from spore-forming
Firmicutes and the reference phylogeny that we just validated. Since we
saw that Henikoff weighting did not seem to help, we'll leave it out.

~~~~ {.shell}
$ mlgsc_train -W Prot Spo0A.msa firmicutes_by_genus.nw
~~~~

The output is a binary file that contains the model. For this reason, it
is not written to standard output, but directly to a file. By default,
that file's name is derived from the alignment's name. In this case, the
name is `Spo0A.bcls` ("binary classifier"). To specify another name, use
option `-o`.

We can now use the model to classify sequences.

### [Classifying](#classifying)

This is done with program `mlgsc`, which takes two arguments: the name of the
queries file, and the name of the model. Let's run it on its own input, to see
if its predicted classifications make sense:

    $ mlgsc Spo0A.pep Spo0A.bcls
    ID_001 Bacillus -> Bacilli (56); Bacillaceae (97); Bacillus (87)
    ID_002 Clostridium -> Clostridia (106); unnamed (138); Clostridium (83)
    ID_003 Clostridium -> Clostridia (106); unnamed (138); Clostridium (83)
    ID_004 Bacillus -> Bacilli (79); Bacillaceae (95); Bacillus (99)
    ID_005 Bacillus -> Bacilli (84); Bacillaceae (89); Bacillus (104)
    ID_006 Bacillus -> Bacilli (84); Bacillaceae (89); Bacillus (104)
    ...

where `Spo0A.pep` contains the (unaligned) Spo0A sequences. As we can
see, the first six queries are predicted correctly. However, since these
queries were part of the training set, this cannot be used to validate
the classifier's accuracy. 

[Real-world Example](#real-world-example)
-----------------------------------------

It is advisable to have more than one sequence per reference taxon, but in our
experience adding more than a dozen does little to enhance the classifier's
accuracy. Ideally each taxon should be represented by roughly the same number of
sequences, but since the alignment is subjected to Henikoff weighting the
program can tolerate large variations (this is often the case when downloading
all representatives of a given gene from a database: some genera like
_Clostridium_ or _Pseudomonas_ have hundreds of known members, while several
"rare" genera have only one. 

##### Reference Phylogeny

This should be a Newick tree in a single line. The leaves (tips) of the tree
must correspond to the taxa in the alignment. The tree may be a phylogram (i.e., with branch lengths), but the branch lengths are not used and will be ignored. Inner node labels are allowed and indeed encouraged, as they will feature in the path through the tree that `mlgsc` outputs.

###### Example

Here is a sample tree of Firmicute genera:

```
               ┌─────────────────────────────────────────── Sporomusa           
               │                                                                
               │                            ┌────────────── Anaerofustis        
               │              ┌─────────────┤                                   
               │              │             └────────────── Eubacterium         
               │              │                                                 
               │              │             ┌────────────── Anaerostipes        
               │              │             │                                   
               │              ├─────────────┼────────────── Dorea               
               │              │             │                                   
               │              │             └────────────── Marvinbryantia      
               │              │                                                 
               │              ├──────────────────────────── Clostridium         
               │              │                                                 
               │              │             ┌────────────── Dehalobacter        
               │              ├─────────────┤                                   
 ┌─────────────┼─Clostridia───┤             └────────────── Desulfotomaculum    
 │             │              │                                                 
 │             │              └──────────────────────────── Symbiobacterium     
 │             │                                                                
 │             │              ┌──────────────────────────── Carboxydibrachium   
 │             │              │                                                 
 │             │              ├──────────────────────────── Carboxydothermus    
 │             └──────────────┤ Thermoanaerobacteraceae                         
 │                            ├──────────────────────────── Moorella            
 │                            │                                                 
 │                            └──────────────────────────── Thermacetogenium    
─┤
 │             ┌─────────────────────────────────────────── Alicyclobacillus    
 │             │                                                                
 │             │              ┌──────────────────────────── Amphibacillus       
 │             │              │                                                 
 │             │              ├──────────────────────────── Bacillus            
 │             ├──────────────┤ Bacillaceae                                     
 │             │              ├──────────────────────────── Geobacillus         
 │             │              │                                                 
 │             │              └──────────────────────────── Oceanobacillus      
 │             │                                                                
 ├─────────────┼─Bacilli─────────────────────────────────── Bhargavaea          
 │             │                                                                
 │             │              ┌──────────────────────────── Brevibacillus       
 │             ├──────────────┤ Paenibacillaceae                                
 │             │              └──────────────────────────── Paenibacillus       
 │             │                                                                
 │             └─────────────────────────────────────────── Desmospora          
 │                                                                              
 │             ┌─────────────────────────────────────────── C innocuum          
 │             │                                                                
 └─────────────┼─Erysipelotrichia────────────────────────── E dolichum          
               │                                                                
               └─────────────────────────────────────────── Turicibacter 
```

Note that the taxon names in the alignment (`Bacillus`, `Clostridium`, etc.)
appear at the _leaves_ of the tree.

This tree (actually, a slightly larger version - the one above was shortened a
bit to better fit the page) is found in `firmicutes_by_genus.nw`. It is a
Newick-formatted file:

```
((Sporomusa,((Anaerofustis,Eubacterium),(Anaerostipes,Dorea,Marvinbryantia),Clostridium,(Dehalobacter,Desulfotomaculum),Heliobacterium,Oscillibacter,Ruminococcus,Pseudoflavonifractor,Sulfobacillus,Symbiobacterium),(Carboxydibrachium,Carboxydothermus,Moorella,Thermacetogenium)Thermoanaerobacteraceae)Clostridia,(Alicyclobacillus,(Amphibacillus,Anoxybacillus,Bacillus,Geobacillus,Lysinibacillus,Oceanobacillus)Bacillaceae,Bhargavaea,Listeria,Exiguobacterium,(Brevibacillus,Paenibacillus)Paenibacillaceae,Pasteuria,Sporosarcina,Desmospora)Bacilli,(C_innocuum,E_dolichum,Turicibacter)Erysipelotrichia);
```

#### Building the Classifier

To train a model using the above alignment and phylogeny, do:

```shell
$ mlgsc_train Prot Spo0A.msa firmicutes_by_genus.nw
```

The output is a binary file. For this reason, it is not written to standard
output, but directly to a file. By default, that file's name is derived from
the alignment's name. In this case, the name is `Spo0A.bcls` ("binary
classifier"). To specify another name, use option `-o`.

#### `mlgsc`

The classifier can now be used to classify unknown ("query") sequences. This is
done with program `mlgsc`, which takes two arguments: the name of the queries
file, and the name of the classifier. Let's run it on its own input, to see if
its predicted classifications make sense:


```
$ mlgsc Spo0A.pep Spo0A.bcls
ID_001 Bacillus -> Bacilli (56); Bacillaceae (97); Bacillus (87)
ID_002 Clostridium -> Clostridia (106); unnamed (138); Clostridium (83)
ID_003 Clostridium -> Clostridia (106); unnamed (138); Clostridium (83)
ID_004 Bacillus -> Bacilli (79); Bacillaceae (95); Bacillus (99)
ID_005 Bacillus -> Bacilli (84); Bacillaceae (89); Bacillus (104)
ID_006 Bacillus -> Bacilli (84); Bacillaceae (89); Bacillus (104)
...
```

where `Spo0A.pep` contains the (unaligned) Spo0A sequences. As we can see, the
first six queries are predicted correctly. However, since these queries were
part of the training set, this cannot be used to validate the classifier's
accuracy. To do this, we need to evaluate it on queries that are _not_ part of
the training set, and this is the function of the third program in the package,
`mlgsc_xval`.

##### Alternative Modes

`mlgsc` has two other modes:

1. Error-recovery: the program explores more of the tree, allowing it to avoid
   some misclassifications (at the price of longer run times)
2. Full traversal ("debug"): the program traverses the whole tree. This is used
   in case of  misclassification, to see where the classifier makes a wrong choice.

###### Error-Recovery mode

By default, when deciding which subtree to explore, `mlgsc` chooses the subtree
rooted at the node that yields the best score. But with option `-m <threshold>`,
`mlgsc` will explore _all subtree whose evidence ratio with respect to the best
score is less than the threshold_. In other words, if a node's ER is less than
the threshold, the corresponding subtree is considered tied. All ties are
explored, and at the end, the best among the ties (in terms of score of the leaf
node) is reported.

For example, with `-m 10`, `mlgsc` will explore the best-scoring branch _as well
as any branch wth an ER <= 10 with respect to the best one._

Of course, this means that as more branches are explored, the program takes
longer to run. The effect of the tie threshold can be seen on this plot:

![ER tie cutoff](./img/recovery_err_vs_time.pdf)


#### `mlgsc_xval`

The function of `mlgsc_xval` is to validate a classifier. To do so, it takes
the same inputs as `mlgsc_train` (namely, an alignment and a tree), but instead
of directly building a classifier, it does the following:

1. Randomly draw one sequence from the alignment. This sequence becomes the
   _test sequence_, while all the other sequences form the _training set_. The
   test sequence is thus not part of the training set;
2. Build a classifier using the training set and the tree;
3. Classify the test sequence using the classifier.

This procedure is repeated one hundred times (the number can be changed with option `-r`).

This form of cross-validation where the test set contains one datum and the
training set contains all other data is called _Leave-one-out_. In the case of
`mlgsc_xval`, we are constrained by the fact that a taxon in the tree must be
represented by _at least one_ sequence in the alignment, otherwise there is no
way for that taxon to be predicted as a classification. Therefore, a sequence can
become a test sequence only if at least one sequence of the same taxon is left in
the training set. By default, it is required that an taxon contain at least three,
but this number can be changed with option `-m`.

### Complete Example

The following is an example using real data referred to in the
[article](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0129384).
The data are found in subdirectory `data/manuscript`.

### [Training the model](#training-the-model-1)

File `firmicute_Spo0A_prot_train.msa` is a multiple alignment of known
Spo0A sequences extracted from UniProtKB.

File `firmicute_genera.nw` is a Newick-formatted phylogeny of
Firmicutes, downloaded from NCBI Taxonomy and edited so as to have genus
labels instead of IDs at the tree tips.

A protein model of Spo0A is created by the following command:

~~~~ {.shell}
$ mlgsc_train -v 2 -o firmicutes_Spo0A.mod Prot firmicute_Spo0A_prot_train.msa
firmicute_genera.nw
MLGSC - building model 
input alignment:  firmicute_Spo0A_prot_train.msa
input tree: firmicute_genera.nw
output: firmicutes_Spo0A.mod
molecule: Prot
small prob: 1.0e-4
scale factor: 1000.0
~~~~

Option `-v 2` (verbosity level 2) causes run information to be printed. Option
`-o` specifies a name for the output file, that is, he classifier itself.

#### Classifying environmental Amplicons

File `Spo0A_env_ampl_prot.pep` contains translated amplicons of Spo0A from
environmental samples (sediment from Lake Geneva). To classify these sequences,
do:

```bash
$ mlgsc Spo0A_env_ampl_prot.pep firmicutes_Spo0A.mod
IEQTHJI02DW663_1 [1 - 588]  -> unnamed (50); unnamed (44); Brevibacillus (140)
IEQTHJI02DW663_2 [492 - 1] (REVERSE SENSE)  -> unnamed (135); unnamed (182);
Clostridium (98)
IEQTHJI02DXXW9_1 [587 - 3] (REVERSE SENSE)  -> unnamed (3); unnamed (23);
Paenibacillus (5)
IEQTHJI02D2KPX_1 [404 - 3] (REVERSE SENSE)  -> unnamed (23); unnamed (86);
Clostridium (80)
IEQTHJI02D8PM8_1 [1 - 546]  -> unnamed (14); unnamed (147); Clostridium (162)
IEQTHJI02D28VO_1 [183 - 593]  -> unnamed (90); unnamed (191); Clostridium (104)
IEQTHJI02D28VO_2 [593 - 3] (REVERSE SENSE)  -> unnamed (25); unnamed (66);
Paenibacillus (96)
IEQTHJI02C9B6J_1 [1 - 534]  -> unnamed (60); unnamed (96); Clostridium (117)
IEQTHJI02EN3F3_1 [1 - 480]  -> unnamed (5); unnamed (137); Clostridium (151)
IEQTHJI02C74FC_1 [1 - 438]  -> unnamed (8); unnamed (124); Clostridium (152)
...
```

This example uses a tree in which only the leaves are labeled. Leaves must be
labeled with taxon names for classification to work at all, but MLgsc can also
use trees with internal labels, as shown below.

Option `-v 2` (verbosity level 2) causes run information to be printed.
Option `-o` specifies a name for the output file, that is, he model
itself.

### [Classifying environmental Amplicons](#classifying-environmental-amplicons)

File `Spo0A_env_ampl_prot.pep` contains translated amplicons of Spo0A
from environmental samples (sediment from Lake Geneva). To classify
these sequences, do:
```
$ mlgsc_train -o firmicutes_Spo0A.mod Prot firmicute_Spo0A_prot_train.msa firmicute_genera_fully-labeled.nw 
The following tree taxa are NOT found in the alignment:
Listeria
```

MLgsc outputs a warning about the taxon name found in the tree but not in the
alignment. The taxon is simply ignored and this does not prevent MLgsc from
building a classifier, but discrepancies between alignment and tree may
indicate that the wrong file(s) are being used, hence the warnings. At any
rate, any actual _Listeria_ among the queries will be misclassified. To
suppress all warnings, pass `-v 0` (verbosity level 0: quiet).

~~~~ 
$ ../../src/mlgsc Spo0A_env_ampl_prot.pep firmicutes_Spo0A.mod
IEQTHJI02DW663_1 [1 - 588]  -> Bacilli (50); Paenibacillaceae (44); Brevibacillus (140)
IEQTHJI02DW663_2 [492 - 1] (REVERSE SENSE)  -> Clostridia (135); Clostridiales (182); Clostridium (98)
IEQTHJI02DXXW9_1 [587 - 3] (REVERSE SENSE)  -> 
IEQTHJI02D2KPX_1 [404 - 3] (REVERSE SENSE)  -> Clostridia (23); Clostridiales (86); Clostridium (80)
IEQTHJI02D8PM8_1 [1 - 546]  -> Clostridia (14); Clostridiales (147); Clostridium (162)
...
~~~~

We see that the third query sequence (`IEQTHJI02DXXW9_1`) cannot be classified
at all. This is often the case with environmental samples. Possible causes
include:

* short queries
* insufficient training data
* unknown organisms
* and of course, weaknesses in the classification algorithm.

#### Tweaking the Output Format
IEQTHJI02D28VO_1 [183 - 593]  -> Clostridia (90); Clostridiales (191); Clostridium (104)
IEQTHJI02D28VO_2 [593 - 3] (REVERSE SENSE)  -> Bacilli (25); Paenibacillaceae (66); Paenibacillus (96)
IEQTHJI02C9B6J_1 [1 - 534]  -> Clostridia (60); Clostridiales (96); Clostridium (117)
IEQTHJI02EN3F3_1 [1 - 480]  -> Clostridia (5); Clostridiales (137); Clostridium (151)
IEQTHJI02C74FC_1 [1 - 438]  -> Clostridia (8); Clostridiales (124); Clostridium (152)
```

By default, `mlgsc` outputs the whole header of the query sequence - often it
contains just an ID. Sometimes, as is the case here, the header contains extra
information that may not be essential. The format of `mlgsc`'s output can be
controlled via a printf-like format string, as in the following example:

~~~~
$ mlgsc -f "%i -> %p" Spo0A_env_ampl_prot.pep firmicutes_Spo0A.mod | head -5
IEQTHJI02DW663_1 -> Bacilli (50); Paenibacillaceae (44); Brevibacillus (140)
IEQTHJI02DW663_2 -> Clostridia (135); Clostridiales (182); Clostridium (98)
IEQTHJI02DXXW9_1 -> 
IEQTHJI02D2KPX_1 -> Clostridia (23); Clostridiales (86); Clostridium (80)
IEQTHJI02D8PM8_1 -> Clostridia (14); Clostridiales (147); Clostridium (162)
~~~~

Here, option `-f` specifies the format via its argument, `%i -> %p`. The
`%i` is a placeholder for the ID, which is taken to be the first word
(whitespace-separated) in the header; the `%p` is a placeholder for the
predicted classification. The following placeholders are recognized:

  placeholder   meaning
  ------------- -----------------------------------------------------------
  `%a`          aligned query sequence (useful for diagnosing alignment problems)
  `%h`          full FastA header
  `%i`          query ID (1st word of header)
  `%l`          query length (unaligned)
  `%p`          predicted classification (path through the tree)
  `%%`          literal % sign

For example, it may be useful to display the length of the query, since short
sequence carry less information and may be harder to classify. Perhaps the
unclassifiable third sequence (`IEQTHJI02DXXW9_1`) is a bit on the short side?
The following format string shows the query length in parentheses, prefixed by
"`l=`":

~~~~ {.sourceCode .bash}
$ mlgsc -f "%i (l=%l) -> %p" Spo0A_env_ampl_prot.pep firmicutes_Spo0A.mod
IEQTHJI02DW663_1 (l=196) -> Bacilli (50); Paenibacillaceae (44); Brevibacillus
(140)
IEQTHJI02DW663_2 (l=164) -> Clostridia (135); Clostridiales (182); Clostridium
(98)
IEQTHJI02DXXW9_1 (l=195) -> 
IEQTHJI02D2KPX_1 (l=134) -> Clostridia (23); Clostridiales (86); Clostridium
(80)
IEQTHJI02D8PM8_1 (l=182) -> Clostridia (14); Clostridiales (147); Clostridium
(162)
...
~~~~

No, the third sequence is not particularly short - in fact, it is the second
longest of the first five.
