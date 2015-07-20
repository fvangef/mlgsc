module IDTree where

import Data.Tree
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import Data.Text (Text)
import qualified Data.Map as M
import Data.Map (Map, findWithDefault, (!))
import Data.List (nub, sort)

import MlgscTypes
import FastA

renumberedTaxonMap :: Tree SeqID -> [FastA] -> Map SeqID OTUName
renumberedTaxonMap tree fastaRecs =
    M.fromList $ concatMap renumberTaxon $ M.toList tax2id
    where   
            tax2id = M.fromListWith (++) fringe
            fringe = [(taxon,[ids])
                | (ids,taxon) <- flatten cndsTreeWTaxa, not $ null ids]
            cndsTreeWTaxa = condenseByTaxon treeWtaxa
            treeWtaxa =
                fmap (\id -> (id, findWithDefault ST.empty id id2tax'')) tree
            -- The map's keys and values are lazy text; we want both strict:
            id2tax'' = M.mapKeys LT.toStrict id2tax'
            id2tax' = M.map LT.toStrict id2tax
            id2tax = idToTaxonMap fastaRecs

condenseByTaxon :: Tree IDTaxonPair -> Tree ([SeqID], OTUName)
condenseByTaxon (Node (id,taxon) []) = Node ([id], taxon) []
condenseByTaxon (Node (id,taxon) kids) =
    case uniqueCondensedKidsTaxa of
        [] -> error "taxa list must not be empty"
        [taxon] -> if (taxon == ST.empty)
                        then Node ([], ST.empty) condensedKids
                        else Node (concatIDs, taxon) []
        (txn:taxa) -> Node ([], ST.empty) condensedKids
        where   condensedKids = map condenseByTaxon kids
                condensedKidsTaxa = map (snd . rootLabel) condensedKids
                uniqueCondensedKidsTaxa = nub $ sort condensedKidsTaxa 
                concatIDs = concatMap (fst . rootLabel) condensedKids 

renumberTaxon :: (OTUName, [[SeqID]]) -> [IDTaxonPair]
-- If a taxon hs a single group (i.e., is monophyletic), don't renumber
renumberTaxon (taxon,[singleton]) = 
    zip singleton $ repeat taxon
-- Otherwise, append ".1", ".2", etc.
renumberTaxon (taxon,idgroups) =
    concat $ zipWith (setNumberedTaxon taxon) [1..] idgroups

-- Takes a taxon name, an int, and a list of sequence IDs, and builds a numbered
-- taxon name by appending the int to the taxon name, then "attributes" the
-- numbered taxon name to all IDs in the list.

setNumberedTaxon :: OTUName -> Int -> [SeqID] -> [IDTaxonPair]
setNumberedTaxon taxon n ids = zip ids $ repeat numtax
    where numtax    = ST.append taxon $ ST.pack $ "." ++ show n

idTreeToRenumTaxonTree :: Map SeqID OTUName -> Tree SeqID -> Tree OTUName
idTreeToRenumTaxonTree map tree = fmap (rename map) tree
    where   rename map seqID = findWithDefault ST.empty seqID map
