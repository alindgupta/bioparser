module Data.Bioparser.Util
    ( Defline
    , Sequence
    , Plusline
    , Scoreline
    , FastaRecord(..)
    , FastqRecord(..)
    ) where

import Data.ByteString (ByteString)

type Defline = ByteString
type Sequence = ByteString
type Plusline = ByteString
type Scoreline = ByteString

newtype FastaRecord = FastaRecord (Defline, Sequence)
    deriving (Show)

newtype FastqRecord = FastqRecord (Defline, Sequence, Plusline, Scoreline)
    deriving (Show)
