module Data.Bioparser.Util
    ( FastaRecord
    , FastqRecord
    ) where

import Data.Vector (Vector)
import Data.ByteString (ByteString)

type Defline = ByteString
type Sequence = ByteString
type Plusline = ByteString
type Scoreline = ByteString

newtype FastaRecord = FastaRecord 
    { getFastaRecord ::  Vector (Defline, Sequence) }
    deriving (Show)

newtype FastqRecord = FastqRecord
    { getFastqRecord :: Vector (Defline, Sequence, Plusline, Scoreline) }
    deriving (Show)
