-- A type class for clade models, i.e., a conserved sequence regions that should
-- be able to recognize a clade (be it a OTU, or a species, or whatever "rank").

import Data.Text

module CladeModel
(
)
where

type Sequence = Data.Text
type Residue = Char

class CladeModel a where
    scoreOf :: a -> Residue -> Position -> Numeric
    scoreSeq :: a -> Sequence -> Numeric
