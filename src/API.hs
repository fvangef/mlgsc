{-
 - The closest thing MLgsc has to an API. The functions in this module are
 - meant to be called by application code, while other non-app functions are to
 - be called, directly or not, by functions in this module. Functions found here
 - are not specialized to a single app.
 -}

module API (
    rawTree,
    fastaRecsAndTree,
    otuAlignmentMap,
    parsePhyloFormat
   )  where

import qualified Data.Text as ST
import Data.Tree

import MlgscTypes
import NewickParser
import Alignment
import Weights
import TaxoParser (parseTaxonomy)
import IDTree (renumFastaRecs, renumTaxonTree,
                renumberedTaxonMap)
import FastA


-- Used in the options parser to determine if tree is Newick or Taxo
-- TODO: extend this to any prefix of "Newick" or "Taxonomy"; handle unknown
-- format

parsePhyloFormat :: Monad m => String -> m PhyloFormat
parsePhyloFormat s = if 'N' == head s
                                then return Newick
                                else return Taxonomy

{- The phylogeny may be passed as a tree (Newick) or as a taxonomy. The contents
 - of the phylogeny file must be passed 
 - TODO: this function does no error handling!
 -}

rawTree :: PhyloFormat -> String -> Tree ST.Text
rawTree Newick treeString = nwTree
    where (Right nwTree) = parseNewickTree treeString
rawTree Taxonomy treeString = parseTaxonomy treeString

{- If the tree is an ID tree (that is, the leaf labels are sequence IDs rather
 - than reference taxon manes e.g. if the tree has been computed from the
 - sequences in the reference alignment), then a few things need to happen:
 -
 - 1) The sequence IDs must be mapped to taxa. For this, we use the ID and
 -    taxon names from the Fasta headers in the alignment, as usual.
 - 2) The tre must be condensed, i.e. all clades consisting of the same
 -    taxon must be reduced to a single leaf (à la nw_condense).
 - 3) Any remaining multiple occurrences of the same taxon (IOW, the taxon
 -    was not monophyletic this often occurs and may be due to many causes,
 -    including noise and nomenclature discrepancies) must be sorted out. In
 -    this case, we just number them, for example if taxon A appears three times
 -    in the tree even after condensing, we renumber them to A.1, A.2 and A.3.
 - 4) The taxa in the reference sequence headers must be renamed to reflect
 -    this fact. This way, the classifier will distinguish A.1 from A.2 and A.3,
 -    etc.
 -
 - The following function takes care of this. The boolean isIDtree states if the
 - tree is an ID tree or not. The other two arguments are the "raw" records and
 - tree. Since both the tree and alignment may be modified, this function
 - returns an (alignmemnt, tree) tuple.
 -}


fastaRecsAndTree :: Bool -> [FastA] -> Tree SeqID -> ([FastA], Tree OTUName)
fastaRecsAndTree isIDtree rawFastaRecs rawTree
    | isIDtree  = (renumFastaRecs rnMap rawFastaRecs,
                   renumTaxonTree rnMap rawTree)
    | otherwise = (rawFastaRecs, rawTree)
        where (Just rnMap) = renumberedTaxonMap rawTree rawFastaRecs
    
{- Takes the list of FastA records and builds a Taxon -> Record map; also takes
 - care of applying Henikoff weighting unless disabled ('noHenWt'). -}

otuAlignmentMap noHenWt fastaRecs = alnToAlnMap wtOtuAln
    where   otuAln = fastARecordsToAln fastaRecs
            wtOtuAln = if noHenWt
                        then otuAln
                        else henikoffWeightAln otuAln

