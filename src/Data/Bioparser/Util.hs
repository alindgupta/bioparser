module Data.Bioparser.Util
    ( Defline
    , Sequence
    , PlusLine
    , ScoreLine
    , FastaRecord(..)
    , FastqRecord(..)
    ) where

import Data.ByteString (ByteString)

type Defline = ByteString
type Sequence = ByteString
type PlusLine = ByteString
type ScoreLine = ByteString

newtype FastaRecord = FastaRecord (Defline, Sequence)
    deriving (Show)

newtype FastqRecord = FastqRecord (Defline, Sequence, PlusLine, ScoreLine)
    deriving (Show)
