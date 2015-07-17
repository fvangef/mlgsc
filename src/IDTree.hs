module IDTree where

import Data.Tree
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (nub, sort)

import MlgscTypes

condenseByTaxon :: Tree IDTaxonPair -> Tree ([SeqID], OTUName)
condenseByTaxon (Node (id,taxon) []) = Node ([id], taxon) []
condenseByTaxon (Node (id,taxon) kids) =
    case uniqueCondensedKidsTaxa of
        [] -> error "taxa list must not be empty"
        [taxon] -> if (taxon == T.empty)
                        then Node ([], T.empty) condensedKids
                        else Node (concatIDs, taxon) []
        (txn:taxa) -> Node ([], T.empty) condensedKids
        where   condensedKids = map condenseByTaxon kids
                condensedKidsTaxa = map (snd . rootLabel) condensedKids
                uniqueCondensedKidsTaxa = nub $ sort condensedKidsTaxa 
                concatIDs = concatMap (fst . rootLabel) condensedKids 


