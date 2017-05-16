-- |
--      Newtype wrappers
--
--     
--      TODO: Util.hs should include utility tools for analyzing files


module Data.Bioparser.Types
    ( Defline
    , Sequence
    , PlusLine
    , ScoreLine
    , FastaRecord(..)
    , FastqRecord(..)
    ) where


import Data.ByteString (ByteString)
import Control.DeepSeq


type Defline = ByteString
type Sequence = ByteString
type PlusLine = ByteString
type ScoreLine = ByteString

newtype FastaRecord = FastaRecord (Defline, Sequence)
    deriving (Show)

-- | Required for benchmarking purposes
instance NFData FastaRecord where
    rnf (FastaRecord _) = ()

newtype FastqRecord = FastqRecord (Defline, Sequence, PlusLine, ScoreLine)
    deriving (Show)
