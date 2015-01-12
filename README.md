MLGSC - Maximum-likelihood  general sequence classifier
=====

MLGSC is a set of programs for classifying sequences according to OTU. It
consists of the following:

* `mlgsc_train`: trains a classifier using an alignment and a phylogenetic tree
* `mlgsc`: classifies unknown sequences according to a classifier produced by
  `mlgsc_train`
* `mlgsc_xval`: performs leave-one-out cross-validation on an alignment and a
  tree

The distribution contains source code, binaries and example data, including data
used in the article (submitted). 

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

MLgsc is available as **binaries** (Linux x86-64) and as **source code**. The binaries
are found in subdirectory `src` of the distribution. They can be installed by
issuing

```
$ sudo make install
```

in that directory. 

To compile from source, see below.

### Installing Haskell

MLgsc is written in the [Haskell](https://www.haskell.org) language. You will
need a Haskell compiler and libraries; the easiest way of obtaining this is to
install the [Haskell platform](https://www.haskell.org/platform) on your
machine - it is [available](https://www.haskell.org/platform/linux.html) as a
precompiled package for several major Linux distribution including Ubuntu,
Fedora, Mint, etc.

#### Additional Libraries

Libraries not included in the Haskell platform may be installed with the
`cabal` tool (which comes with the platform). The following command will
install all additional packages (you may need to run this as root):

```bash
# cabal install array binary containers filepath mtl optparse-applicative parsec random text text-binary vector vector-binary$
```

### Building

If not already done, obtain the source from GitHub (e.g. using the "Download
ZIP" button, or with `git clone`, etc.). Uncompress the archive if needed, then
issue the following commands in the top-level directory of the source
distribution:

```shell
$ runhaskell Setup.hs configure
$ cabal build
$ sudo cabal install --global
```

This will install the programs in `/usr/local/bin`.

### Program Manuals

#### `mlgsc_train`

This trains a classifier model (either DNA or protein) from a multiple alignment
of reference sequences and a phylogenetic tree of the reference OTUs.

This program takes three arguments: (i) one of the two keywords `DNA` or `Prot`,
to indicate the type of molecule; (ii) the name of the multiple alignment file;
and (iii) the name of a phylogenetic tree.

##### Reference Multiple Alignment

The alignment should be in aligned (gapped) FastA format. The header lines
should contain an ID followed by an OTU name. The ID is ignored for training,
but can be used to identify problematic training sequences; since the first word
of a FastA header is usually an ID this will allow existing FastA alignments to
be used with minimal editing.

###### Example

Here are the first three entries in a multiple-FastA alignment of the stage 0
sporulation protein A, Spo0A, in Firmicutes, grouped by genus.

The first entry in the alignment has ID `ID_001` and OTU `Bacillus`. This
states that the sequence is a reference for the _Bacillus_ genus. The next two
(`ID_001` and `ID_002`) are reference sequences for genus _Clostridium_.

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

It is advisable to have more than one reference sequence per OTU, but in our
experience adding more than a dozen does little to enhance the classifier's
accuracy. Ideally each OTU should be represented by roughly the same number of
sequences, but since the alignment is subjected to Henikoff weighting the
program can tolerate large variations (this is often the case when downloading
all representatives of a given gene from a database: some genera like
_Clostridium_ or _Pseudomonas_ have hundreds of known members, while several
"rare" genera have only one. 

##### Reference Phylogeny

This should be a Newick tree in a single line. The leaves (tips) of the tree
must correspond to the OTUs in the alignment. The tree may be a phylogram (i.e., with branch lengths), but the branch lengths are not used and will be ignored. Inner node labels are allowed and indeed encouraged, as they will feature in the path through the tree that `mlgsc` outputs.

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

Note that the OTU names in the alignment (`Bacillus`, `Clostridium`, etc.)
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
`mlgsc_xval`, we are constrained by the fact that an OTU in the tree must be
represented by _at least one_ sequence in the alignment, otherwise there is no
way for that OTU to be predicted as a classification. Therefore, a sequence can
become a test sequence only if at least one sequence of the same OTU is left in
the training set. By default, it is required that an OTU contain at least three,
but this number can be changed with option `-m`.

### Example

The following is an example using real data referred to in the article
(submitted). The data are found in subdirectory `data/manuscript`.

#### Building the classifier

File `firmicute_Spo0A_prot_train.msa` is a multiple alignment of known Spo0A
sequences extracted from UniProtKB. 

File `firmicute_genera.nw` is a Newick-formatted phylogeny of Firmicutes,
downloaded from NCBI Taxonomy and edited so as to have genus labels instead of
IDs at the tree tips.

A protein model of Spo0A is created by the following command:

```shell
$ mlgsc_train -v 2 -o firmicutes_Spo0A.mod Prot firmicute_Spo0A_prot_train.msa
firmicute_genera.nw
MLGSC - building model 
input alignment:  firmicute_Spo0A_prot_train.msa
input tree: firmicute_genera.nw
output: firmicutes_Spo0A.mod
molecule: Prot
small prob: 1.0e-4
scale factor: 1000.0
```

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
