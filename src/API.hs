{-
 - The closest thing MLgsc has to an API. The functions in this module are
 - meant to be called by application code, while other non-app functions are to
 - be called, directly or not, by functions in this module. Functions found here
 - are not specialized to a single app.
 -}

module API (
    seqsAndTree
   )  where

import Data.Tree

import MlgscTypes
import IDTree (renumFastaRecs, renumTaxonTree,
                renumberedTaxonMap)
import FastA

    {-
    let rawTree = if optTaxonomy params
                    then parseTaxonomy $ treeString
                    else nwTree
                        where (Right nwTree) = parseNewickTree treeString
    fastAInput <-  LTIO.readFile $ alnFName params
    let rawFastaRecs = fastATextToRecords fastAInput
    let (fastaRecs, tree) = if optIDtree params
                                then (renumFastaRecs rnMap rawFastaRecs,
                                        renumTaxonTree rnMap rawTree)
                                else (rawFastaRecs, rawTree)
                                    where (Just rnMap) = renumberedTaxonMap
                                                         rawTree rawFastaRecs
                                                         -}

{- If the tree is an ID tree (that is, the leaf labels are sequence IDs rather than
 - reference taxon manes - e.g. if the tree has been computed from the sequences
 - in the reference alignment), then a few things need to happen:
 - 1) The sequence IDs must be mapped to taxa. For this, we use the ID and taxon
 -    names from the Fasta headers in the alignment, as usual.
 - 2) The tree must be condensed, i.e. all clades consisting of the same taxon
 -    must be reduced to a single leaf (à la nw_condense).
 - 3) Any remaining multiple occurrences of the same taxon (IOW, the taxon was
 -    not monophyletic - this often occurs and may be due to many causes,
 -    including noise and nomenclature discrepancies) must be sorted out. In
 -    this case, we just number them, for example if taxon A appears three times
 -    in the tree even after condensing, we renumber them to A.1, A.2 and A.3.
 - 4) The taxa in the reference sequence headers must be renamed to reflect this
 -    fact. This way, the classifier will distinguish A.1 from A.2 and A.3, etc.
 -
 - The following function takes care of this. The boolean isIDtree states if the
 - tree is an ID tree or not. The other two arguments are the "raw" records and
 - tree. Since both the tree and alignment may be modified, this function
 - returns an (alignmemnt, tree) tuple.
 -}


seqsAndTree :: Bool -> [FastA] -> Tree SeqID -> ([FastA], Tree OTUName)
seqsAndTree isIDtree rawFastaRecs rawTree
    | isIDtree  = (renumFastaRecs rnMap rawFastaRecs, renumTaxonTree rnMap rawTree)
    | otherwise = (rawFastaRecs, rawTree)
        where (Just rnMap) = renumberedTaxonMap rawTree rawFastaRecs
    
mkClassifier = undefined
