-- H. Bayesian Classifier
-- TODO: separate model code from classifier code
--
module Model where

import Data.List 
import Data.Map
import Data.Set
import Data.Tree
import qualified Data.Text.Lazy as T

import Trees
import FastA
import MlgscTypes
import Cdf

type MaskPredicate = Int -> Bool

-- TODO: the distributions and matrices should be real types (data) not
-- aliases.

-- An OTU model has a OTU name and a ISL matrix

data Model      = Model {
                        name    :: T.Text,
                        matrix  :: ISLProbMatrix
                } deriving (Show, Read)

-- A raw model is similar, but has a RawProbMatrix (i.e., floating-point prob
-- values, 0 <= p <= 1), instead of integer log-probs.

data RawModel   = RawModel {
                        otuName :: T.Text,
                        rawMatrix  :: RawProbMatrix
                } deriving (Show, Read)

data Classifier = Classifier {
        modelTree       :: Tree Model
        , scoreCDF       :: EmpiricalCDF Int
        } deriving (Show, Read)

type TreeBuilder    = 
    Float -> Int -> [(T.Text,T.Text)] -> Tree T.Text -> Tree Model

strict_nt   = Data.Set.fromList "ATGC-"
nucleotides = Data.Set.fromList "ATGCMRWSYKVHDBN-"
amino_acids = Data.Set.fromList "ACDEFGHIKLMNPQRSTVWY-"

mol2set :: Molecule -> ResidueSet
mol2set AminoAcid = amino_acids
mol2set Nucleotide = strict_nt
mol2set IUPACNucleotide = nucleotides

-- see e.g. http://droog.gs.washington.edu/parc/images/iupac.html
ambig2unambig = Data.Map.fromList [
                ('M', "AC"),
                ('R', "AG"),
                ('W', "AT"),
                ('S', "CG"),
                ('Y', "CT"),
                ('K', "GT"),
                ('V', "ACG"),
                ('H', "ACT"),
                ('D', "AGT"),
                ('B', "CGT"),
                ('N', "ACGT")
        ]

{--| Constructs a frequency matrix from an alignment, given also the set of
 - allowed residues (can include '-') as well as a value for small
 - probabilities (to prevent an unobserved residue (hence with a probability of
 - 0) to nullify the whole sequence's probability (or to give -Infinity when
 - using logarithms). Data.List.transpose transforms a list of sequences in a
 - list of columns, from which the frequency distributions are computed by
 - col2distribution.
 - The matrix's values can be transformed with matrixMap. --}

-- TODO: nucleotide matrices need to be able to handle IUPAC ambiguity codes. I
-- had written code for that, it just needs to be called only for nuc, not aa.
-- Check in the log.

{-
rawProbMatrix :: ResidueSet -> Prob -> Alignment -> RawProbMatrix
rawProbMatrix allowedResidues smallProb =
        Data.List.map (col2distribution allowedResidues smallProb) . transpose
-}

-- Tight version

tightRawProbMatrix :: Prob -> Alignment -> RawProbMatrix
tightRawProbMatrix smallProb = 
        Data.List.map (col2tightDistribution smallProb) . T.transpose

{- Computes a relative frequency distribution for all th eresidues at a
 - particular column (= position in an alignment).  'resWithUniformProb' is a
 - list of tuples, one per residue in 'col', with the form [(res_1, 1/L),
 - (res_2, 1/L), ..., (res_L, 1/L)], where L is the length of 'col' (and hence
 - also the number of sequences in the alignment).  When passed to fromListWith
 - (+), the result is a Map, which has as many keys as there are _distinct_
 - residues in 'resWithUniformProb'. The value associated with each key is
 - count(key) * 1/L, IOW the relative frequency of that residue. E.g. [('A',
 - 1/3), ('G', 1/3), ('A', 1/3)] -> fromList [('A', 2/3),('G',1/3)]. Note: C, T
 - are _not_ in the map (yet...).
 -
 - Now this map is unioned with base_distrib, which is another list of tuples,
 - one per _allowed_ residue, associated with a "frequency" of 0.0. The effect
 - of this union (note that the order of the arguments is important) is to add
 - to the map any residue not present in 'col' (with a frequency of zero).
-}

{-
col2distribution :: ResidueSet -> Prob -> Column -> RawProbDistribution
-- TODO: cycle [1/num_seq] is constant, and should be computed once per aln.
-- TODO: try repeat a instead of cycle [a]
col2distribution allowedRes smallProb col
        = Data.Map.union col_distrib base_distrib
        where   col_distrib     = fromListWith (+) resWithUniformProb
                resWithUniformProb      = zip col (cycle [1 / num_seq])
                num_seq         = fromIntegral (length col)
                base_distrib    = fromSet (\e -> smallProb) allowedRes
-}

col2tightDistribution :: Prob -> Column -> RawProbDistribution
-- TODO: cycle [1/num_seq] is constant, and should be computed once per aln.
-- TODO: try repeat a instead of cycle [a]
col2tightDistribution smallProb col =  fromListWith (+) resWithUniformProb
        where   resWithUniformProb      = zip (T.unpack col) (cycle [1 / num_seq])
                num_seq         = fromIntegral (T.length col)

-- handle IUPAC codes in a list of (residue, frequency) pairs. I.e., ('Y', x) ->
-- [('C', x/2),('T', x/2)]
handleIUPAC :: [(Char, Float)] -> [(Char, Float)]
handleIUPAC [] = []
handleIUPAC ((res, freq):rfs) =
        case Data.Map.lookup res ambig2unambig of
                Nothing -> (res, freq):handleIUPAC rfs
                Just unamb_res -> unamb_pairs ++ handleIUPAC rfs
                        where   unamb_pairs = zip unamb_res (repeat (freq/l))
                                l = fromIntegral (length unamb_res)

-- Same idea, but on a model (TODO: ideally Model should be an instance of
-- Functor, but for this it must be parametric).
modelMap f (Model name mat) = Model name (matrixMap f mat)

-- matrixScore :: ProbMatrix -> Sequence -> Float
matrixScore f mat seq = 
        foldl1 f $ zipWith (Data.Map.!) mat seq

matrixScoreLog mat seq = matrixScore (+) mat seq
matrixScoreRaw mat seq = matrixScore (*) mat seq -- is this ever used?

{-
matrixScoreLogMask :: ISLProbMatrix -> [Bool] -> Sequence -> Int
matrixScoreLogMask mat mask seq = sum $ zipWith3 f mat mask seq
        where f dist keep res
                | keep == True  = (Data.Map.!) dist res
                | keep == False = 0
-}

tightMatrixScore :: Int -> ISLProbMatrix -> T.Text -> Int
tightMatrixScore smallProbLog mat seq =
        sum $ zipWith (Data.Map.findWithDefault smallProbLog)
                      (T.unpack seq) mat

{- this should be adapted to the fact that Model now has a ISLProbMatrix
inputModels = Data.Map.mapWithKey makeModel input_lines_byname
        where makeModel name aln = Model name (rawProbMatrix nucleotides 0.0001 aln)
-}

-- This function constructs a hierarchical classifier from input lines and a
-- (binary!) tree.

-- lines2modelTree :: [String] -> BinaryTree String -> BinaryTree Model
{-
lines2modelTree lines mol tree = fmap (pair2model resSet) whole_seq_tree
        where   resSet = case mol of
                        "nuc" -> nucleotides
                        "aa" -> amino_acids
                whole_seq_tree = merge leaf_seq_tree
                leaf_seq_tree = fmap (seqLookup alnMap) tree
                alnMap = fromListWith (++) $
                        Data.List.map line2pair $ Data.List.map words lines
-}

-- Takes a list of (otu-name, sequence) pairs, a molecule specification ("nuc"
-- or "aa"), and a BinaryTree, and builds a hierarchical classifier.

{-
pairs2modelTree :: [(String, String)] -> String -> BinaryTree String -> BinaryTree Model
pairs2modelTree pairs mol tree = fmap (pair2model resSet) whole_seq_tree
        where   resSet = case mol of
                        "nuc" -> nucleotides
                        "aa" -> amino_acids
                whole_seq_tree = merge leaf_seq_tree
                leaf_seq_tree = fmap (seqLookup alnMap) tree
                alnMap = fromListWith (++) $
                        Data.List.map (\p -> (fst p, [snd p])) pairs
-}

-- Same as above, but with a Rose tree (in fact the above f for bin trees will
-- probably no longer be of much use). 

{-
pairs2modelRTree ::
        [(String,String)] -> ResidueSet -> RoseTree String -> RoseTree Model
pairs2modelRTree pairs resSet tree = fmap (pair2model resSet) whole_seq_tree
        where   whole_seq_tree = mergeR leaf_seq_tree
                leaf_seq_tree = fmap (seqLookup alnMap) tree
                alnMap = pairs2AlnMap pairs
-}

-- same, tight models

pairs2tightModelRTree :: Float -> Int ->
        [(T.Text,T.Text)] -> Tree T.Text -> Tree Model
pairs2tightModelRTree smallprob scale_factor pairs tree =
		fmap (pair2tightModel smallprob scale_factor) whole_seq_tree
        where   whole_seq_tree = mergeR leaf_seq_tree
                leaf_seq_tree = fmap (seqLookup alnMap) tree
                alnMap = pairs2AlnMap pairs

-- same, balanced models: all OTUs have the same weight in the model, even if
-- they have different numbers of sequences (which is NOT the case with
-- pairs2tightModelRTree above).

-- This is for leave-one-out testing: for all OTUs that have >1 sequence, a
-- model is built using all sequences except one. That model is returned in the
-- first element of the returned pair. The second element is a concatenation of
-- all sequences that were NOT included in the models - this should be tested
-- aginst the model.
-- TODO: check this option; also allow balanced trees if these work better.

{-
pairs2Leave1Out :: Float -> Int ->
        [(T.Text,T.Text)] -> RoseTree T.Text -> (RoseTree Model, T.Text)
pairs2Leave1Out smallprob scale_factor pairs tree =
		(fmap (pair2tightModel smallprob scale_factor)  whole_seq_tree, trainSet)
        where   whole_seq_tree = mergeR leaf_seq_tree
                leaf_seq_tree = fmap (seqLookup l1oMap) tree
                l1oMap = fmap tailUnlessSingle alnMap
                trainSet = T.concatMap pair2fasta $ assocs $ fmap head alnMap
                alnMap = pairs2AlnMap pairs

-- Builds a OTU-name-indexed map of lists of sequences
-}

pairs2AlnMap :: [(T.Text,T.Text)] -> Map T.Text [T.Text]
pairs2AlnMap pairs = fromListWith (++) $
                Data.List.map (\p -> (fst p, [snd p])) pairs

-- Return the tail of a list, unless the list has one element or less (in which
-- case, return the list untouched).

tailUnlessSingle :: [a] -> [a]
tailUnlessSingle []     = []
tailUnlessSingle [x]    = [x]
tailUnlessSingle (x:xs) = xs

-- Convert a (OTU-name, sequence) pair into a FastA string. Remove gaps along
-- the way.

{-
pair2fasta' :: (T.Text, T.Text) -> T.Text
pair2fasta' (name, alnseq) = ">" ++ name ++ "\n" ++
        (Data.List.filter (/= '-') alnseq) ++ "\n"
-}

pair2fasta :: (T.Text, T.Text) -> T.Text
pair2fasta (name, alnseq) = T.concat [
                                T.pack ">",
                                name,
                                T.replace (T.pack "-") (T.pack "") alnseq
                            ]


-- Converts a [<name>, <seq>] split line to (<name>, [seq]). This format is
-- required by fromListWith.
{-
line2pair :: [String] -> (String, [Sequence])
line2pair split_line = (head split_line, tail split_line)
-}

{-
il2seqs_byname = fromListWith (++) $ Data.List.map line2pair $ 
        Data.List.map words input_lines_2

leaf_seq_tree = fmap (seqLookup il2seqs_byname) tax2
-}

seqLookup :: Map T.Text Alignment -> T.Text -> (T.Text, Alignment) 
seqLookup map name = case Data.Map.lookup name map of
                        Just x -> (name, x)
                        Nothing -> (name, [])

-- takes a BinaryTree of (spc-name, [Sequence]) pairs and returns another
-- BinaryTree of (spc-name, [Sequence]) pairs. The Sequence lists at the
-- internal nodes of the result contain a concatenation of the lists of the two
-- children. Thus the root nodes's list contains a concatenation of all lists
-- at the leaves. The names are left untouched. That is, if the input tree has
-- names only for leaves and "" elsewhere, the returned tree has empty names in
-- the inner nodes, and identical names for the leaves.

{-
merge :: BinaryTree (String, [a])  -> BinaryTree (String, [a])
merge (Leaf (name, list))       = Leaf (name, list)
merge (Node (name, list) l r)   = Node (name, concat)  ml mr
        where   ml = merge l
                mr = merge r
                concat = snd (treeData ml) ++ snd (treeData mr)
-}

-- this version of merge is for Rose trees 
mergeR :: Tree (T.Text, [a]) -> Tree (T.Text, [a])
mergeR (Node (name, seqlist) [])    = Node (name, seqlist) []
mergeR (Node (name, seqlist) kids)  =
        Node (name, merged_seq_list) merged_kids
        where   merged_kids = Data.List.map mergeR kids
                merged_seq_list = foldr1 (++) $
                        Prelude.map (snd . rootLabel) merged_kids 

-- Similar, but instead of merging trees of sequence lists (= OTU alignments)
-- like mergeR does, this one merges _raw probability matrices_. The effect is
-- that all OTUs have the same weight.

mergeBalancedR :: Tree RawModel -> Tree RawModel
mergeBalancedR leaf@(Node _ []) = leaf
mergeBalancedR (Node rmod kids) =
    Node (RawModel (T.pack "") avg_mat) merged_kids
    where   avg_mat = averageRPD merged_kid_mats
            merged_kids = Data.List.map mergeBalancedR kids
            merged_kid_mats = Data.List.map (rawMatrix . rootLabel) merged_kids 

-- Constructs a Model from a OTU name and a list of (aligned) sequences

{- 
pair2model :: ResidueSet -> (String, [Sequence]) -> Model
pair2model resSet (name, seq_list) = Model name isl_mat
        where   mat = rawProbMatrix resSet 0.0001 seq_list
                isl_mat = matrixMap (round . (*scale_factor) . (logBase 10)) mat
-}

-- same, but tight model

pair2tightModel :: Float -> Int -> (T.Text, [Sequence]) -> Model
pair2tightModel smallprob scale_factor (name, seq_list) = Model name isl_mat
        where   mat = tightRawProbMatrix smallprob seq_list
                isl_mat = matrixMap (round . (*scale) . (logBase 10)) mat
                scale = fromIntegral scale_factor

-- same, but raw model (all models are "tight" now, so no need to state this;
-- non-tight functions should really go away...)

pair2RawModel :: Float -> (T.Text, [Sequence]) -> RawModel 
pair2RawModel smallprob (name, seq_list) = RawModel name mat
        where   mat = tightRawProbMatrix smallprob seq_list

-- converts a RawModel to an equivalent Model

rawModel2Model :: Float -> Int -> RawModel -> Model
rawModel2Model smallprob scale_factor (RawModel name mat) = Model name isl_mat
        where   isl_mat = matrixMap (round . (*scale) . (logBase 10)) mat
                scale = fromIntegral scale_factor

balancedModelRTree :: Float -> Int -> [(T.Text,T.Text)] -> Tree T.Text 
    -> Tree Model
balancedModelRTree smallprob scale pairs otuname_tree =
    fmap (rawModel2Model smallprob scale) merged_mod_tree 
    where   merged_mod_tree = mergeBalancedR leaf_mod_tree 
            leaf_mod_tree   = fmap (pair2RawModel smallprob) leaf_aln_tree
            leaf_aln_tree   = fmap (seqLookup alnMap) otuname_tree
            alnMap          = pairs2AlnMap pairs

{-
modelTreeScore :: BinaryTree Model -> Sequence -> (String, Int)
modelTreeScore (Leaf (Model name mat)) seq = (name, matrixScoreLog mat seq)
modelTreeScore (Node mod left right) seq
        | leftScore > rightScore        = modelTreeScore left seq  
        | otherwise                     = modelTreeScore right seq
                where   leftScore       = matrixScoreLog
                                                (matrix $ treeData left) seq
                        rightScore      = matrixScoreLog
                                                (matrix $ treeData right) seq
-}

{-
modelRTreeScore :: Tree Model -> Sequence -> (T.Text, Int)
modelRTreeScore (RoseTree (Model name mat) []) seq =
        (name, matrixScoreLog mat seq)
modelRTreeScore (RoseTree mod kids) seq = modelRTreeScore best seq
        where   best = fst $ maximumBy cmpModScTuple kidsModScTuples
                kidsModScTuples = zip kids kidsScoreTuples
                kidsScoreTuples = Data.List.map scoreTuple kidsModels
                kidsModels = Data.List.map rTreeData kids
                scoreTuple mod = (name mod, matrixScoreLog (matrix mod) seq)
                cmpModScTuple (_, (_, a)) (_, (_, b))
                                | a <= b = LT
                                | otherwise = GT
-}

-- Same, for tight matrices
-- TODO: derive small prob score (-4000) from actual small prob
tightModelRTreeScore :: Tree Model -> Sequence -> (T.Text, Int)
tightModelRTreeScore (Node (Model name mat) []) seq =
        (name, tightMatrixScore (-4000) mat seq)
tightModelRTreeScore (Node mod kids) seq = tightModelRTreeScore best seq
        where   best = fst $ maximumBy cmpModScTuple kidsModScTuples
                kidsModScTuples = zip kids kidsScoreTuples
                kidsScoreTuples = Data.List.map scoreTuple kidsModels
                kidsModels = Data.List.map rootLabel kids
                scoreTuple mod = (name mod, tightMatrixScore (-4000) (matrix mod) seq)
                cmpModScTuple (_, (_, a)) (_, (_, b))
                                | a <= b = LT
                                | otherwise = GT

-- We could keep a single model tree score function, ans pass a trivial mask
-- consisting only of True. But this would make the function slower than
-- necessary. So we keep a maskable one and a (moderately, I suppose) faster
-- one.

{-
modelRTreeScoreMask :: RoseTree Model -> [Bool] -> Sequence -> (String, Int)
modelRTreeScoreMask (RoseTree (Model name mat) []) mask seq =
        (name, matrixScoreLogMask mat mask seq)
modelRTreeScoreMask (RoseTree mod kids) mask seq = modelRTreeScoreMask best mask seq
        where   best = fst $ maximumBy cmpModScTuple kidsModScTuples
                kidsModScTuples = zip kids kidsScoreTuples
                kidsScoreTuples = Data.List.map scoreTuple kidsModels
                kidsModels = Data.List.map rTreeData kids
                scoreTuple mod = (name mod, matrixScoreLogMask (matrix mod) mask seq)
                cmpModScTuple (_, (_, a)) (_, (_, b))
                                | a <= b = LT
                                | otherwise = GT
-}

-- To mask certain positions in a matrix. Inputs are a list of integers

maskMatrix :: [Int] -> MaskPredicate -> ISLProbMatrix -> ISLProbMatrix
maskMatrix scores pred mat = Prelude.map (mask pred) (zip scores mat)
    where mask pred (score, dist) 
                | pred score    = Data.Map.map (\_ -> 0) dist
                | otherwise     = dist

-- Functions for saving and loading classifiers (should perhaps not belong here)

save :: Tree Model -> FilePath -> IO ()
save hm f = writeFile f (show hm)

loadClassifier :: FilePath -> IO Classifier
loadClassifier f = do   s <- readFile f
                        let classifier = (read s)::Classifier
                        return classifier

-- Debugging / Info functions

dumpMatrix :: ISLProbMatrix -> String
dumpMatrix = (intercalate "\n" . Data.List.map (show . Data.Map.elems))
