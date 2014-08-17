-- A type class for clade models, i.e., a conserved sequence regions that should
-- be able to recognize a clade (be it a OTU, or a species, or whatever "rank").

module CladeModel where

import MlgscTypes

class CladeModel a where
    -- The score of a residue at a position. This is the log observed frequency
    -- of said residue at that position, scaled and rounded.
    scoreOf ::  a -> Residue -> Position -> Int
    -- The score of a sequence according to the model
    scoreSeq :: a -> Sequence -> Int
    -- The length (in aligned positions) of the model
    modLength :: a -> Int
    -- Score of a residue not found at some position
    absentResScore :: a -> Int
