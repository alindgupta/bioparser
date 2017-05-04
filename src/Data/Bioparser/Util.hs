module Data.Bioparser.Util
    ( Defline
    , Sequence
    , Plusline
    , Scoreline
    , FastaRecord(..)
    , FastqRecord(..)
    ) where

import Data.Word (Word8)

type Defline = [Word8]
type Sequence = [Word8]
type Plusline = [Word8]
type Scoreline = [Word8]

newtype FastaRecord = FastaRecord 
    { getFastaRecord ::  (Defline, Sequence) }
    deriving (Show)

newtype FastqRecord = FastqRecord
    { getFastqRecord :: (Defline, Sequence, Plusline, Scoreline) }
    deriving (Show)
