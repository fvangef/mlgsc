Alignment Problem
-----------------

In one case at least, the alignment produced by `Align.hs` is wrong.

### To reproduce

Build a model from the multiple sequence alignment:

```
$ cd data/aln_prob
$ mlgsc_train DNA Plantae_trnL_train.msa Plantae_genera.nw
```

Now take a sequence from the same alignment, de-gap it, and classify:

```bash
$ degapseq Plantae_trnL_train.msa:ID_1602 > Bouteloua_1602.dna
$ ID_1602 Bouteloua ->  (*);  (258);  (265);  (*);  (210); Spermatophyta (299);
Angiospermae (209);  (127);  (*); Mesangiospermae (108);  (128);  (*);  (34);
Eudicotyledoneae (46); Gunneridae (70);  (1); Superasteridae (*); Pentapetalae
(*);  (63); Asteridae (*);  (24); Lamiidae (21);  (*);  (*);  (*);  (*);  (78);
(28); Lamiales (34);  (173);  (116);  (*);  (39);  (*);  (23);  (122);  (10);
(55);  (87); Gesneriaceae (*);  (75);  (105);  (*);  (9);  (*);  (*);  (13);
(*);  (*); Boea (*)
```

_Boea_? In the Gesneriaceae? _Bouteloua_ is a **grass**!! This is totally,
embarrassingly wrong. The LCA of _Bouteloua_ and _Boea_ lived something like 100
Mya, in the early Cretaceous.

Now, let's look at the query sequence, as aligned to the others in the training
set (by `mafft`):

```bash
>ID_1602 Bouteloua
--------gactt-g-a-tt-gt-attgagcc-tt-----gg-tatgg---aggcctgc-
t-aa-at--ggtaac-ttccaaattcagag-aaa-ccct-gg------------aat---
----g-----------aaaaat-------gggtaatcctgagc-----c-aaa-------
------------------------------------------------------tccc-t
---------------------------------ttttttg---aaaaaac------aaat
agttc-----t--caaact-agaaacta-gaacccaaag---------------------
----------------------------------------
```

and here is how `mlgsc` aligns it (folded too 60 chars for ease of comparison):

```bash
--------GACTT-G-A-TT-GT-ATTGAGCC-TT-----GG-TAT-GG--AGGCCTGC-
T-AAA-T--GGTAAC-TTCCAAATTCAGAG-AAA-CCCT-GG------------AAT---
---G------------AAAAA------T-GGGTAATCCTGAG------C-CAA---ATC-
----------CC-------------------T-----T----------------TTTT-T
G---------------------A-----AA--AA----AC-----A-A----AT--A--G
-TTC------TC-AA-ACTAG--------AAACTAGAACCC------AAAG---------
----------------------------------------
```

The two can be viewed together (`seaview`, `showalign`, ...) in `wrong_aln`.
Basically only the first fourth of the alignment looks more or less ok. So, the
hypothesis is that the classification screw-up is due to errors in the
alignment. This can be tested by running the classifier without alignment, using
the original query as aligned by Mafft:

```bash
ID_1602 Bouteloua ->  (*);  (303);  (288);  (*);  (92); Spermatophyta (*);
Angiospermae (225);  (237);  (*); Mesangiospermae (214);  (224);  (*);
Monocotyledoneae (67); Nartheciidae (*); Petrosaviidae (267);  (*);  (141);
Commelinidae (89); Poales (119);  (*);  (234);  (106);  (110);  (65);  (*);
(*); Poaceae (*);  (*);  (*);  (96);  (101);  (*);  (*);  (*);  (79);  (*);
(*);  (*);  (*);  (*);  (89);  (*);  (*);  (*);  (*);  (*);  (*);  (*);  (*);
(83);  (*);  (*);  (*);  (*);  (*);  (*);  (0); Bouteloua (*)
```

Bingo! So the problem lies in the alignment. That does not mean that the
solution will be easy to find, but at least we know where the problem is. The
way to go, I think, is to see if we can obtain the original (i.e., Mafft)
alignment (or something close to it) by tweaking the alignment parameters.
