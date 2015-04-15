module MlgscTypes where

import Data.Text
import Data.Tree
import qualified Data.Map.Strict as M

data Molecule = DNA | Prot
    deriving (Show, Eq, Read)

type Sequence       = Text
type Column         = Text
type Residue        = Char
type Position       = Int
type OTUName        = Text  -- TODO: s/OTU/Taxon/g...
type CladeName      = Text
type OTUTree        = Tree OTUName
type ScaleFactor    = Double -- TODO shouldn't this be an Int?
type SmallProb      = Double
type Score          = Int

-- A classification step at one node in the model tree, in which the OTU name of
-- best model, score of best model, score of next-best, and log10 Evidence Ratio
-- are stored.

data Step = PWMStep {
                otuName             :: OTUName
                , bestScore         :: Score
                , secondBestScore   :: Score
                , log10ER           :: Double
                }

-- A trail of classification steps. Starts at the root of the tree and ends at a
-- leaf.

type Trail          = [Step]
