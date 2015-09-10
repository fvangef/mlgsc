-- A data type for clade models, i.e., a conserved sequence regions that should
-- be able to recognize a clade (be it a OTU, or a species, or whatever "rank").

module PWMModel (PWMModel(..), scoreOf, scoreSeq, modLength, cladeName,
    absentResScore, prettyPrint) where

import Data.Binary (Binary, put, get, Get, Word8)

import MlgscTypes
import NucModel 
import PepModel

data PWMModel = NucPWMModel NucModel
                | PepPWMModel PepModel
                deriving (Show, Eq)

-- By convention, a '.' denotes a masked position, to which we attribute a score
-- of zero.
scoreOf :: PWMModel -> Residue -> Position -> Int
scoreOf _ '.' _                 = 0
scoreOf (NucPWMModel nm) res pos = nucScoreOf nm res pos
scoreOf (PepPWMModel spm) res pos = pepScoreOf spm res pos

scoreSeq :: PWMModel -> Sequence -> Int
scoreSeq (NucPWMModel nm) seq = nucScoreSeq nm seq
scoreSeq (PepPWMModel spm) seq = pepScoreSeq spm seq

modLength :: PWMModel -> Int
modLength (NucPWMModel nm) = nucModLength nm
modLength (PepPWMModel spm) = pepModLength spm

absentResScore :: PWMModel -> Int
absentResScore (NucPWMModel nm) = nucAbsentResScore nm
absentResScore (PepPWMModel spm) = pepAbsentResScore spm

cladeName :: PWMModel -> CladeName
cladeName (PepPWMModel spm) = pepCladeName spm
cladeName (NucPWMModel nm) = nucCladeName nm

prettyPrint :: PWMModel -> String
prettyPrint (PepPWMModel spm) = pepPrettyPrint spm
prettyPrint (NucPWMModel nm) = nucPrettyPrint nm

instance Binary PWMModel where
    put (NucPWMModel nm) = do
        put (0 :: Word8) >> put nm
    put (PepPWMModel pm) = do
        put (1 :: Word8) >> put pm
    
    get = do
        mol <- get :: Get Word8
        case mol of
            0 -> do
                nm <- get :: Get NucModel
                return $ NucPWMModel nm
            1 -> do
                pm <- get :: Get PepModel
                return $ PepPWMModel pm

-- TODO: factor out these two
