module MlgscTypes where

import Data.Map as Map
import Data.Set as Set
import Data.List as List
import Data.Text.Lazy as T

data Molecule = AminoAcid | Nucleotide | IUPACNucleotide
	deriving (Show, Eq)

type Alignment = [Sequence]
type Column = T.Text
type ISLProbDistribution = Map Residue Int	-- ISL: integer, scaled, log
type ISLProbMatrix = [ISLProbDistribution]
type Prob = Float
type RawProbDistribution = Map Residue Float
type RawProbMatrix = [RawProbDistribution]
type Residue = Char
type ResidueSet = Set Residue
type Sequence = T.Text

strToMolecule :: String -> Molecule
strToMolecule str = case str of
	 "snuc"	->	Nucleotide	-- strict nucleotides (no Y, R, stc.)
	 "nuc"	->	IUPACNucleotide
	 "aa"	->	AminoAcid

seqToMolecule :: String -> Molecule
seqToMolecule seq 
	| nuc_sum >= 0.8 	= Nucleotide
	| otherwise		= AminoAcid
	where	nuc_sum = ( (freq ! 'A') + (freq ! 'C') +
			(freq ! 'G') + (freq ! 'T') )
		freq = fromListWith (+) (base ++ freq')
		freq' = List.zip seq (repeat $ 1 / len)
		len = (fromIntegral . List.length) seq
		base = List.zip "ATGC" (repeat 0)

-- This applies a function to all the probabilities in a matrix. For example,
-- if 'mat' is expressed in terms of relative frequencies, I can convert them
-- to logarithms base 10, scale them by a factor of 1000, and round to integers
-- in a single call: matrixMap (round . (* 1000) . (logBase 10)) mat
-- TODO: couldn't we just do this with fmap, since Map is a functor?

matrixMap :: (a -> b) -> [Map k a] -> [Map k b]
matrixMap f = List.map (Map.map f)

colSum :: (Num c, Ord k) => Map k c -> Map k c -> Map k c
colSum = Map.mergeWithKey msum id id
    where msum _ v1 v2 = Just (v1 + v2)

matrixSum :: (Num c, Ord k) => [Map k c] -> [Map k c] -> [Map k c]
matrixSum = List.zipWith colSum

averageRPD :: [RawProbMatrix] -> RawProbMatrix
averageRPD mats = List.foldr1 matrixSum normalizedRPDMats
    where   normalizedRPDMats = List.map (matrixMap (/ len)) mats
            len = fromIntegral $ List.length mats
