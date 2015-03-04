-- A type class for clade models, i.e., a conserved sequence regions that should
-- be able to recognize a clade (be it a OTU, or a species, or whatever "rank").

module CladeModel (CladeModel(..), scoreOf, scoreSeq, modLength, cladeName,
    absentResScore) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Binary (Binary, put, get, Get, Word8)

-- TODO: streamline imports
import MlgscTypes
import SimpleNucModel 
import SimplePepModel

data CladeModel = NucCladeModel SimpleNucModel
                | SimplePepCladeModel SimplePepModel
                deriving (Show, Eq)

scoreOf :: CladeModel -> Residue -> Position -> Int
scoreOf (NucCladeModel nm) res pos = simpleNucScoreOf nm res pos
scoreOf (SimplePepCladeModel spm) res pos = simplePepScoreOf spm res pos

scoreSeq :: CladeModel -> Sequence -> Int
scoreSeq (NucCladeModel nm) seq = simpleNucScoreSeq nm seq
scoreSeq (SimplePepCladeModel spm) seq = simplePepScoreSeq spm seq

modLength :: CladeModel -> Int
modLength (NucCladeModel nm) = simpleNucModLength nm
modLength (SimplePepCladeModel spm) = simplePepModLength spm

absentResScore :: CladeModel -> Int
absentResScore (NucCladeModel nm) = simpleNucAbsentResScore nm
absentResScore (SimplePepCladeModel spm) = simplePepAbsentResScore spm

cladeName :: CladeModel -> CladeName
cladeName (SimplePepCladeModel spm) = simplePepCladeName spm
cladeName (NucCladeModel nm) = simpleNucCladeName nm

instance Binary CladeModel where
    put (NucCladeModel nm) = do
        put (0 :: Word8) >> put nm
    put (SimplePepCladeModel pm) = do
        put (1 :: Word8) >> put pm
    
    get = do
        mol <- get :: Get Word8
        case mol of
            0 -> do
                nm <- get :: Get SimpleNucModel
                return $ NucCladeModel nm
            1 -> do
                pm <- get :: Get SimplePepModel
                return $ SimplePepCladeModel pm

-- TODO: factor out these two
