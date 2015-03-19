-- A type class for clade models, i.e., a conserved sequence regions that should
-- be able to recognize a clade (be it a OTU, or a species, or whatever "rank").

-- TODO: streamline exports and imports
module CladeModel where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Binary (Binary, put, get, Get, Word8)

import MlgscTypes
import NucModel 
import PepModel

data CladeModel = NucCladeModel NucModel
                | PepCladeModel PepModel
                deriving (Show, Eq)

scoreOf :: CladeModel -> Residue -> Position -> Int
scoreOf (NucCladeModel nm) res pos = nucScoreOf nm res pos
scoreOf (PepCladeModel spm) res pos = pepScoreOf spm res pos

scoreSeq :: CladeModel -> Sequence -> Int
scoreSeq (NucCladeModel nm) seq = nucScoreSeq nm seq
scoreSeq (PepCladeModel spm) seq = pepScoreSeq spm seq

modLength :: CladeModel -> Int
modLength (NucCladeModel nm) = nucModLength nm
modLength (PepCladeModel spm) = pepModLength spm

absentResScore :: CladeModel -> Int
absentResScore (NucCladeModel nm) = nucAbsentResScore nm
absentResScore (PepCladeModel spm) = pepAbsentResScore spm

cladeName :: CladeModel -> CladeName
cladeName (PepCladeModel spm) = pepCladeName spm
cladeName (NucCladeModel nm) = nucCladeName nm

instance Binary CladeModel where
    put (NucCladeModel nm) = do
        put (0 :: Word8) >> put nm
    put (PepCladeModel pm) = do
        put (1 :: Word8) >> put pm
    
    get = do
        mol <- get :: Get Word8
        case mol of
            0 -> do
                nm <- get :: Get NucModel
                return $ NucCladeModel nm
            1 -> do
                pm <- get :: Get PepModel
                return $ PepCladeModel pm

class CModel a where
    cscoreOf :: a -> Residue -> Position -> Int
    cscoreSeq :: a -> Sequence -> Int
    cmodLength :: a -> Int
    cabsentResScore :: a -> Int
    ccladeName :: a -> CladeName

instance CModel NucModel where
    cscoreOf = nucScoreOf
    cscoreSeq = nucScoreSeq
    cmodLength = nucModLength
    cabsentResScore = nucAbsentResScore
    ccladeName = nucCladeName

instance CModel PepModel where
    cscoreOf = pepScoreOf
    cscoreSeq = pepScoreSeq
    cmodLength = pepModLength
    cabsentResScore = pepAbsentResScore
    ccladeName = pepCladeName
