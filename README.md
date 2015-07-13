-   [MLgsc - Maximum-likelihood general sequence classifier](#mlgsc---maximum-likelihood-general-sequence-classifier)
    -   [Example](#example)
    -   [Installation](#installation)
        -   [Installing Haskell](#installing-haskell_)_
        -   [Building](#building)
    -   [Tutorial](#tutorial)
        -   [Choosing the Clade and Classifying region](#choosing-the-clade-and-classifying-region)
        -   [Obtaining a reference Alignment and Phylogeny](#obtaining-a-reference-alignment-and-phylogeny)
        -   [Evaluating the Clade and Classifying region](#evaluating-the-clade-and-classifying-region)
        -   [Training the Model](#training-the-model)
    -   [Real-world Example](#real-world-example)

MLgsc - Maximum-likelihood general sequence classifier
======================================================

MLgsc is a set of programs for classifying sequences into taxa (in other
words, recognizing taxa from sequences). MLgsc trains a model using
reference sequences from a user-specified conserved region (e.g., a
gene) as well as a phylogeny of the taxa of interest. It can work on
*protein as well as nucleic acid* sequences.

The package consists of the following:

-   `mlgsc_train`: trains a model using an alignment and a phylogenetic
    tree
-   `mlgsc`: classifies unknown sequences according to a model produced
    by `mlgsc_train`
-   `mlgsc_xval`: performs leave-one-out cross-validation on an
    alignment and a tree

The distribution contains source code, binaries and example data,
including data used in the
[article](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0129384).

[Example](#example)
-------------------------------------------

Here is a short example of MLgsc used for classifying protein sequences
of Spo0A to genus level in the Firmicutes (for more details, see the
[real-world example](#real-world-example) below).

We start with a multiple alignment of Spo0A protein sequences (stored in
a Fasta file called, in this example, `Spo0A.msa`) and a phylogenetic
tree of the Firmicute genera (file `firmicute_genera.nw`).

First, we train the model with the following command:

~~~~ {.shell}
$ mlgsc_train Prot Spo0A.msa firmicute_genera.nw
~~~~

This produces a binary file named `Spo0A.bcls`, which contains the
model.

We can now use it to classify Spo0A sequences of unknown genus, in this
case they are stored in file `queries.pep` (a Fasta file):

~~~~ {.sourceCode .Bash}
$ mlgsc queries.pep Spo0A.bcls
~~~~

This produces output like the following:

    query_001 -> Bacilli (56); Bacillaceae (97); Bacillus (87)
    query_002 -> Clostridia (106); Clostridiales (138); Clostridium (83)
    query_003 -> Clostridia (106); Clostridiales (138); Clostridium (83)
    ...

Each line lists a query ID (`query_001`, etc.), then an arrow (`->`),
then MLgsc's classification of this query. This consists of a list of
tree nodes, from the most general (leftmost) to the most specific
(rightmost) - in this case, genus. Each node is shown with a support
value (number in parentheses). High values indicate high confidence, in
the sense that well-supported node is much more likely to be correct
than the second best. In the example above, query 001 is confidently
classified as *Bacillus*, while queries 002 and 003 are (also
confidently) classified as *Clostridium*.

In some cases, the confidence is so high that the support value is shown
as an asterisk, as in

    query_214 -> Clostridia (301); Clostridiales (*); Pseudoflavonifractor (*)

Of course, confidence values can also be low. This reflects `mlgsc`'s
unability to confidently classify a query. If the value drops below 10,
`mlgsc` does not try to be more specific, as in

    query_200 -> Clostridia (105)

which shows that while query 200 probably belongs to the Clostridia, it
is not possible to tell to which subclade of the Clostridia it belongs.

The [tutorial](#tutorial) section has more details about training and
validating model, as well as on the various options that can be used to
change the programs' logic and/or output.

[Installation](#installation)
---------------------------------------------------------------

MLgsc is available as **binaries** (Linux x86-64) and as **source
code**. The binaries are found in subdirectory `src` of the
distribution. They can be installed by issuing

    $ sudo make install

in that directory.

To compile from source, see below.

### [Installing Haskell](#installing-haskell)

MLgsc is written in the [Haskell](https://www.haskell.org) language. You
will need a Haskell compiler and libraries; the easiest way of obtaining
this is to install the [Haskell
platform](https://www.haskell.org/platform) on your machine - it is
[available](https://www.haskell.org/platform/linux.html) as a
precompiled package for several major Linux distribution including
Ubuntu, Fedora, Mint, etc.

#### [Additional Libraries](#additional-libraries)

Libraries not included in the Haskell platform may be installed with the
`cabal` tool (which comes with the platform). The following command will
install all additional packages (you may need to run this as root):

~~~~ {.sourceCode .bash}
# cabal install array binary containers filepath mtl optparse-applicative parsec random text text-binary vector vector-binary$
~~~~

### [Building](#building)

If not already done, obtain the source from GitHub (e.g. using the
"Download ZIP" button, or with `git clone`, etc.). Uncompress the
archive if needed, then issue the following commands in the top-level
directory of the source distribution:

~~~~ {.shell}
$ runhaskell Setup.hs configure # may need --user
$ cabal build
$ sudo cabal install --global
~~~~

If you have GNU make, you can also just do

~~~~ {.shell}
$ make
$ sudo make install
~~~~

This will install the programs in `/usr/local/bin`.

[Tutorial](#tutorial)
-----------------------------------------------

### [Choosing the Clade and Classifying region](#choosing-the-clade-and-classifying-region)

The first decisions to make are (i) what organisms we wish to be able to
recognize, which we refer to as the *target taxa*, and (ii) with which
conserved region. Obviously, the conserved region must be found in all
the organisms under consideration. In this tutorial, we have chosen to
classify the spore-forming
[Firmicutes](https://en.wikipedia.org/wiki/Firmicutes) using the
[Spo0A](http://www.uniprot.org/uniprot/P06534) gene (at the protein
level). Spo0A is found in every spore-forming Firmicute species.

### [Obtaining a reference Alignment and Phylogeny](#obtaining-a-reference-alignment-and-phylogeny)

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

This run had one error in 100 trials, that is, a 1% error rate. I
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
---------------------------------------------------------------------------------------

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

Option `-v 2` (verbosity level 2) causes run information to be printed.
Option `-o` specifies a name for the output file, that is, he model
itself.

### [Classifying environmental Amplicons](#classifying-environmental-amplicons)

File `Spo0A_env_ampl_prot.pep` contains translated amplicons of Spo0A
from environmental samples (sediment from Lake Geneva). To classify
these sequences, do:

~~~~ {.sourceCode .bash}
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
~~~~

This example uses a tree in which only the leaves are labeled. Leaves
must be labeled with taxon names for classification to work at all, but
MLgsc can also use trees with internal labels, as shown below.

File `firmicute_genera_fully-labeled.nw` contains a tree in which all
internal nodes are labeled as well. It also contains an additional genus
not found in the alignment.

    $ mlgsc_train -o firmicutes_Spo0A.mod Prot firmicute_Spo0A_prot_train.msa firmicute_genera_fully-labeled.nw 
    The following tree taxa are NOT found in the alignment:
    Listeria

MLgsc outputs a warning about the taxon name found in the tree but not
in the alignment. The taxon is simply ignored and this does not prevent
MLgsc from training a model, but discrepancies between alignment and
tree may indicate that the wrong file(s) are being used, hence the
warnings. At any rate, any actual *Listeria* among the queries will be
misclassified. To suppress all warnings, pass `-v 0` (verbosity level 0:
quiet).

The classification now shows internal tree labels:

    $ mlgsc Spo0A_env_ampl_prot.pep firmicutes_Spo0A.mod | head
    IEQTHJI02DW663_1 [1 - 588]  -> Bacilli (50); Paenibacillaceae (44); Brevibacillus (140)
    IEQTHJI02DW663_2 [492 - 1] (REVERSE SENSE)  -> Clostridia (135); Clostridiales (182); Clostridium (98)
    IEQTHJI02DXXW9_1 [587 - 3] (REVERSE SENSE)  -> Bacilli (3); Paenibacillaceae (23); Paenibacillus (5)
    IEQTHJI02D2KPX_1 [404 - 3] (REVERSE SENSE)  -> Clostridia (23); Clostridiales (86); Clostridium (80)
    IEQTHJI02D8PM8_1 [1 - 546]  -> Clostridia (14); Clostridiales (147); Clostridium (162)
    IEQTHJI02D28VO_1 [183 - 593]  -> Clostridia (90); Clostridiales (191); Clostridium (104)
    IEQTHJI02D28VO_2 [593 - 3] (REVERSE SENSE)  -> Bacilli (25); Paenibacillaceae (66); Paenibacillus (96)
    IEQTHJI02C9B6J_1 [1 - 534]  -> Clostridia (60); Clostridiales (96); Clostridium (117)
    IEQTHJI02EN3F3_1 [1 - 480]  -> Clostridia (5); Clostridiales (137); Clostridium (151)
    IEQTHJI02C74FC_1 [1 - 438]  -> Clostridia (8); Clostridiales (124); Clostridium (152)

By default, `mlgsc` output the whole header of the query sequence -
often it contains just an ID. Sometimes, as is the case here, the header
contains extra information that may not be essential. The format of
`mlgsc`'s output can be controlled via a printf-like format string, as
in the following example:

~~~~ {.sourceCode .bash}
$ mlgsc -f "%i -> %p" Spo0A_env_ampl_prot.pep firmicutes_Spo0A.mod | head
IEQTHJI02DW663_1 -> Bacilli (50); Paenibacillaceae (44); Brevibacillus (140)
IEQTHJI02DW663_2 -> Clostridia (135); Clostridiales (182); Clostridium (98)
IEQTHJI02DXXW9_1 -> Bacilli (3); Paenibacillaceae (23); Paenibacillus (5)
IEQTHJI02D2KPX_1 -> Clostridia (23); Clostridiales (86); Clostridium (80)
IEQTHJI02D8PM8_1 -> Clostridia (14); Clostridiales (147); Clostridium (162)
IEQTHJI02D28VO_1 -> Clostridia (90); Clostridiales (191); Clostridium (104)
IEQTHJI02D28VO_2 -> Bacilli (25); Paenibacillaceae (66); Paenibacillus (96)
IEQTHJI02C9B6J_1 -> Clostridia (60); Clostridiales (96); Clostridium (117)
IEQTHJI02EN3F3_1 -> Clostridia (5); Clostridiales (137); Clostridium (151)
IEQTHJI02C74FC_1 -> Clostridia (8); Clostridiales (124); Clostridium (152)
~~~~

Here, option `-f` specifies the format via its argument, `%i -> %p`. The
`%i` is a placeholder for the ID, which is taken to be the first word
(whitespace-separated) in the header; the `%p` is a placeholder for the
predicted classification. The following placeholders are recognized:

  placeholder   meaning
  ------------- -------------------------------------------------------------------
  `%a`          aligned query sequence (useful for diagnosing alignment problems)
  `%h`          full FastA header
  `%i`          query ID (1st word of header)
  `%l`          query length (unaligned)
  `%p`          predicted classification (path through the tree)
  `%%`          literal % sign

For example, it may be useful to display the length of the query, since
short sequence carry less information and may be harder to classify. The
following format string shows the query length in parentheses, prefixed
by "`l=`":

~~~~ {.sourceCode .bash}
$ mlgsc -f "%i (l=%l) -> %p" Spo0A_env_ampl_prot.pep firmicutes_Spo0A.mod
IEQTHJI02DW663_1 (l=196) -> Bacilli (50); Paenibacillaceae (44); Brevibacillus
(140)
IEQTHJI02DW663_2 (l=164) -> Clostridia (135); Clostridiales (182); Clostridium
(98)
IEQTHJI02DXXW9_1 (l=195) -> Bacilli (3); Paenibacillaceae (23); Paenibacillus (5
...
~~~~
